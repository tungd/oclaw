(** YAML-backed runtime configuration for OClaw.

    The configuration surface is intentionally small: model/provider settings,
    runtime directories, debug mode, agent iteration limits, and API retry
    behavior. Values can be supplied from defaults, a YAML file, environment
    variables, and CLI overrides.
*)

module Yaml_lib = Yaml
module Yaml = Protocol_conv_yaml.Yaml

(* ============================================================================
   Configuration Type
   ============================================================================ *)

type config = {
  (* LLM Settings - required *)
  llm_model : string;
  llm_api_key : string;
  llm_api_base : string;
  
  (* Data Directory *)
  data_dir : string;
  
  (* Agent Limits *)
  max_tool_iterations : int;

  (* Debug *)
  debug : bool;
  
  (* API Retry Configuration *)
  api_retry_enabled : bool;
  api_retry_max_retries : int;
  api_retry_base_delay_ms : int;
  api_retry_max_delay_ms : int;
}
[@@deriving protocol ~driver:(module Yaml)]

(* ============================================================================
   Hardcoded Constants
   ============================================================================ *)

let default_llm_api_base = "https://coding-intl.dashscope.aliyuncs.com/v1"
let default_data_dir = "workspace"
let default_max_tool_iterations = 256
let default_llm_timeout = 60
(* API Retry defaults *)
let default_api_retry_enabled = true
let default_api_retry_max_retries = 3
let default_api_retry_base_delay_ms = 1000
let default_api_retry_max_delay_ms = 30000

(* ============================================================================
   Default Configuration
   ============================================================================ *)

let default_config : config = {
  llm_model = "qwen3.5-plus";
  llm_api_key = "";
  llm_api_base = default_llm_api_base;
  data_dir = default_data_dir;
  max_tool_iterations = default_max_tool_iterations;
  debug = false;
  api_retry_enabled = default_api_retry_enabled;
  api_retry_max_retries = default_api_retry_max_retries;
  api_retry_base_delay_ms = default_api_retry_base_delay_ms;
  api_retry_max_delay_ms = default_api_retry_max_delay_ms;
}

(* ============================================================================
   Descriptor-Driven Configuration Loading
   ============================================================================ *)

type config_source =
  | File of string
  | Env
  | Cli

type config_error = {
  source : config_source;
  key : string;
  value : string option;
  message : string;
}

let string_of_source = function
  | File path -> Printf.sprintf "config file %s" path
  | Env -> "environment"
  | Cli -> "CLI"

let string_of_error error =
  match error.value with
  | Some value ->
      Printf.sprintf "%s: invalid %s=%S (%s)"
        (string_of_source error.source) error.key value error.message
  | None ->
      Printf.sprintf "%s: invalid %s (%s)"
        (string_of_source error.source) error.key error.message

let parse_string value =
  Ok value

let parse_bool value =
  match String.lowercase_ascii (String.trim value) with
  | "1" | "true" | "yes" -> Ok true
  | "0" | "false" | "no" -> Ok false
  | _ -> Error "expected one of: true, false, 1, 0, yes, no"

let parse_int value =
  match int_of_string_opt (String.trim value) with
  | Some parsed -> Ok parsed
  | None ->
      begin
        match float_of_string_opt (String.trim value) with
        | Some parsed when Float.equal parsed (Float.round parsed) -> Ok (int_of_float parsed)
        | _ -> Error "expected an integer"
      end

let yaml_scalar_to_string = function
  | `String value -> Ok value
  | `Float value -> Ok (string_of_float value)
  | `Bool value -> Ok (string_of_bool value)
  | `Null -> Ok ""
  | _ -> Error "expected a scalar value"

module Config_value = struct
  type arg_kind =
    | Flag
    | Value

  type 'a field = {
    key : string;
    env : string;
    arg : string;
    arg_kind : arg_kind;
    desc : string;
    parse : string -> ('a, string) result;
    set : 'a -> config -> config;
  }

  type t = Field : 'a field -> t

  let fold_result f init items =
    let rec loop acc errors = function
      | [] ->
          if errors = [] then Ok acc else Error (List.rev errors)
      | item :: rest ->
          begin
            match f acc item with
            | Ok next -> loop next errors rest
            | Error err -> loop acc (err :: errors) rest
          end
    in
    loop init [] items

  let update_assoc key value assoc =
    (key, value) :: List.remove_assoc key assoc

  let find_by_arg args needle =
    List.find_map
      (fun (Field field as packed) ->
        if String.equal field.arg needle then Some packed else None)
      args

  let has_key fields needle =
    List.exists
      (fun (Field field) -> String.equal field.key needle)
      fields

  let parse_cli_args fields args =
    let rec loop seen errors remaining =
      match remaining with
      | [] -> (seen, List.rev errors)
      | flag :: rest ->
          begin
            match find_by_arg fields flag with
            | None ->
                let err = { source = Cli; key = flag; value = None; message = "unknown configuration argument" } in
                loop seen (err :: errors) rest
            | Some (Field field) ->
                begin
                  match field.arg_kind with
                  | Flag ->
                      let seen = update_assoc field.arg "true" seen in
                      loop seen errors rest
                  | Value ->
                      begin
                        match rest with
                        | value :: tail ->
                            let seen = update_assoc field.arg value seen in
                            loop seen errors tail
                        | [] ->
                            let err = { source = Cli; key = field.arg; value = None; message = "missing argument value" } in
                            loop seen (err :: errors) []
                      end
                end
          end
    in
    loop [] [] args

  let parse_yaml_fields fields = function
    | None -> ([], [])
    | Some (`O yaml_fields) ->
        let parse_one seen (key, value) =
          if not (has_key fields key) then
            Error { source = File "<config>"; key; value = None; message = "unknown configuration key" }
          else
            match yaml_scalar_to_string value with
            | Ok raw -> Ok (update_assoc key raw seen)
            | Error message ->
                Error { source = File "<config>"; key; value = None; message }
        in
        begin
          match fold_result parse_one [] yaml_fields with
          | Ok values -> (values, [])
          | Error errors -> ([], errors)
        end
    | Some _ ->
        ([], [{ source = File "<config>"; key = "<config>"; value = None; message = "expected a YAML object at the top level" }])

  let apply_field cli_values yaml_values cfg errors (Field field) =
    let apply source key raw =
      match field.parse raw with
      | Ok parsed -> (field.set parsed cfg, errors)
      | Error message ->
          let err = { source; key; value = Some raw; message } in
          (cfg, err :: errors)
    in
    match List.assoc_opt field.arg cli_values with
    | Some raw -> apply Cli field.arg raw
    | None ->
        begin
          match Sys.getenv_opt field.env with
          | Some value when String.trim value <> "" -> apply Env field.env value
          | _ ->
              begin
                match List.assoc_opt field.key yaml_values with
                | Some raw -> apply (File "<config>") field.key raw
                | None -> (cfg, errors)
              end
        end

  let fold yaml argv initial fields =
    let cli_values, cli_errors = parse_cli_args fields argv in
    let yaml_values, yaml_errors = parse_yaml_fields fields yaml in
    let cfg, field_errors =
      List.fold_left
        (fun (cfg, errors) field -> apply_field cli_values yaml_values cfg errors field)
        (initial, [])
        fields
    in
    let errors = List.rev_append field_errors (List.rev_append yaml_errors cli_errors) in
    if errors = [] then Ok cfg else Error errors
end

let config_values : Config_value.t list =
  let open Config_value in
  [
    Field {
      key = "llm_model";
      env = "OCLAW_MODEL";
      arg = "--model";
      arg_kind = Value;
      desc = "Override the model name";
      parse = parse_string;
      set = (fun value (cfg : config) -> { cfg with llm_model = value });
    };
    Field {
      key = "llm_api_key";
      env = "OCLAW_API_KEY";
      arg = "--api-key";
      arg_kind = Value;
      desc = "Override the API key";
      parse = parse_string;
      set = (fun value (cfg : config) -> { cfg with llm_api_key = value });
    };
    Field {
      key = "llm_api_base";
      env = "OCLAW_API_BASE";
      arg = "--api-base";
      arg_kind = Value;
      desc = "Override the API base URL";
      parse = parse_string;
      set = (fun value (cfg : config) -> { cfg with llm_api_base = value });
    };
    Field {
      key = "data_dir";
      env = "OCLAW_DATA_DIR";
      arg = "--data-dir";
      arg_kind = Value;
      desc = "Set the runtime data root";
      parse = parse_string;
      set = (fun value (cfg : config) -> { cfg with data_dir = value });
    };
    Field {
      key = "max_tool_iterations";
      env = "OCLAW_MAX_TOOL_ITERATIONS";
      arg = "--max-tool-iterations";
      arg_kind = Value;
      desc = "Set max tool iterations";
      parse = parse_int;
      set = (fun value (cfg : config) -> { cfg with max_tool_iterations = value });
    };
    Field {
      key = "debug";
      env = "OCLAW_DEBUG";
      arg = "--debug";
      arg_kind = Flag;
      desc = "Enable debug logging";
      parse = parse_bool;
      set = (fun value (cfg : config) -> { cfg with debug = value });
    };
    Field {
      key = "api_retry_enabled";
      env = "OCLAW_API_RETRY_ENABLED";
      arg = "--api-retry-enabled";
      arg_kind = Value;
      desc = "Enable or disable API retries";
      parse = parse_bool;
      set = (fun value (cfg : config) -> { cfg with api_retry_enabled = value });
    };
    Field {
      key = "api_retry_max_retries";
      env = "OCLAW_API_RETRY_MAX_RETRIES";
      arg = "--api-retry-max-retries";
      arg_kind = Value;
      desc = "Set API retry count";
      parse = parse_int;
      set = (fun value (cfg : config) -> { cfg with api_retry_max_retries = value });
    };
    Field {
      key = "api_retry_base_delay_ms";
      env = "OCLAW_API_RETRY_BASE_DELAY_MS";
      arg = "--api-retry-base-delay-ms";
      arg_kind = Value;
      desc = "Set API retry base delay";
      parse = parse_int;
      set = (fun value (cfg : config) -> { cfg with api_retry_base_delay_ms = value });
    };
    Field {
      key = "api_retry_max_delay_ms";
      env = "OCLAW_API_RETRY_MAX_DELAY_MS";
      arg = "--api-retry-max-delay-ms";
      arg_kind = Value;
      desc = "Set API retry max delay";
      parse = parse_int;
      set = (fun value (cfg : config) -> { cfg with api_retry_max_delay_ms = value });
    };
  ]

let parse_file_yaml filename =
  let source = File filename in
  try
    let yaml_content = In_channel.with_open_text filename In_channel.input_all in
    match Yaml_lib.of_string yaml_content with
    | Ok yaml -> (Some yaml, [])
    | Error (`Msg msg) ->
        (None, [{ source; key = filename; value = None; message = msg }])
  with
  | Sys_error message ->
      (None, [{ source; key = filename; value = None; message }])
  | exn ->
      (None, [{ source; key = filename; value = None; message = Printexc.to_string exn }])

(** Load configuration from a YAML file. *)
let from_file filename =
  let yaml_opt, file_errors = parse_file_yaml filename in
  match Config_value.fold yaml_opt [] default_config config_values with
  | Ok config ->
      if file_errors = [] then Ok config else Error file_errors
  | Error errors ->
      Error (file_errors @ errors)

(** Load configuration from environment variables. *)
let from_env () =
  Config_value.fold None [] default_config config_values

(** Parse configuration from command-line arguments. *)
let from_args args =
  Config_value.fold None args default_config config_values

(** Convenience function to load config with standard priority: file < env < args *)
let load ?config_file ?(cli_args=[]) () =
  let yaml_opt, file_errors =
    match config_file with
    | None -> (None, [])
    | Some path -> parse_file_yaml path
  in
  match Config_value.fold yaml_opt cli_args default_config config_values with
  | Ok config ->
      if file_errors = [] then Ok config else Error file_errors
  | Error errors ->
      Error (file_errors @ errors)

(* ============================================================================
   Save Configuration
   ============================================================================ *)

let save filename config =
  try
    let yaml_value = config_to_yaml config in
    let yaml_string = Yaml_lib.to_string_exn yaml_value in
    Out_channel.with_open_text filename (fun channel -> output_string channel yaml_string);
    true
  with exn ->
    Printf.printf "Error saving config: %s\n" (Printexc.to_string exn);
    false

let create_default filename =
  save filename default_config

(* ============================================================================
   Validation
   ============================================================================ *)

let validate (config : config) =
  let errors = ref [] in
  if String.trim config.llm_api_key = "" then
    errors := "LLM API key is required. Set llm_api_key or OCLAW_API_KEY." :: !errors;
  if config.max_tool_iterations <= 0 then
    errors := "max_tool_iterations must be positive" :: !errors;
  if !errors = [] then Ok config else Error !errors

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let runtime_data_dir (config : config) =
  Filename.concat config.data_dir "runtime"

let skills_data_dir (config : config) =
  Filename.concat config.data_dir "skills"

let llm_timeout _config =
  default_llm_timeout

(* ============================================================================
   Conversion to LLM Provider Config
   ============================================================================ *)

let to_llm_provider_config (config : config) =
  Llm_provider.{
    api_base = config.llm_api_base;
    api_key = config.llm_api_key;
    model_name = config.llm_model;
    max_tokens = 4096;
    timeout = default_llm_timeout;
  }
