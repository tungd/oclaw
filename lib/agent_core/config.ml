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
   Immutable Configuration Loading Functions
   ============================================================================ *)

(** Load configuration from a YAML file *)
let from_file filename =
  try
    let yaml_content = In_channel.with_open_text filename In_channel.input_all in
    match Yaml_lib.of_string yaml_content with
    | Error (`Msg msg) ->
        Printf.printf "Error loading config: %s\nUsing default configuration.\n" msg;
        default_config
    | Ok yaml_value ->
        (try config_of_yaml_exn yaml_value
         with exn ->
           Printf.printf "Error loading config: %s\nUsing default configuration.\n" (Printexc.to_string exn);
           default_config)
  with exn ->
    Printf.printf "Error loading config: %s\nUsing default configuration.\n" (Printexc.to_string exn);
    default_config

(** Load configuration from environment variables *)
let from_env () =
  let env_string name default =
    match Sys.getenv_opt name with
    | Some value when String.trim value <> "" -> value
    | _ -> default
  in
  let env_bool name default =
    match Sys.getenv_opt name with
    | Some value ->
        let value = String.lowercase_ascii (String.trim value) in
        value = "1" || value = "true" || value = "yes" || (default && value <> "0" && value <> "false" && value <> "no")
    | None -> default
  in
  let env_int name default =
    match Sys.getenv_opt name with
    | Some value ->
        begin
          match int_of_string_opt (String.trim value) with
          | Some parsed -> parsed
          | None -> default
        end
    | None -> default
  in
  {
    llm_model = env_string "OCLAW_MODEL" default_config.llm_model;
    llm_api_key = env_string "OCLAW_API_KEY" default_config.llm_api_key;
    llm_api_base = env_string "OCLAW_API_BASE" default_config.llm_api_base;
    data_dir = env_string "OCLAW_DATA_DIR" default_config.data_dir;
    max_tool_iterations = env_int "OCLAW_MAX_TOOL_ITERATIONS" default_config.max_tool_iterations;
    debug = env_bool "OCLAW_DEBUG" default_config.debug;
    api_retry_enabled = env_bool "OCLAW_API_RETRY_ENABLED" default_config.api_retry_enabled;
    api_retry_max_retries = env_int "OCLAW_API_RETRY_MAX_RETRIES" default_config.api_retry_max_retries;
    api_retry_base_delay_ms = env_int "OCLAW_API_RETRY_BASE_DELAY_MS" default_config.api_retry_base_delay_ms;
    api_retry_max_delay_ms = env_int "OCLAW_API_RETRY_MAX_DELAY_MS" default_config.api_retry_max_delay_ms;
  }

(** Parse configuration from command-line arguments.
    Returns a config with only the overridden fields set, others use defaults.
    
    Supported arguments:
    - --model <string>
    - --api-key <string>
    - --api-base <string>
    - --data-dir <string>
    - --max-tool-iterations <int>
    - --debug
    - --api-retry-enabled <bool>
    - --api-retry-max-retries <int>
    - --api-retry-base-delay-ms <int>
    - --api-retry-max-delay-ms <int>
*)
let from_args args =
  let rec parse acc remaining =
    match remaining with
    | [] -> acc
    | "--model" :: value :: rest ->
        parse { acc with llm_model = value } rest
    | "--api-key" :: value :: rest ->
        parse { acc with llm_api_key = value } rest
    | "--api-base" :: value :: rest ->
        parse { acc with llm_api_base = value } rest
    | "--data-dir" :: value :: rest ->
        parse { acc with data_dir = value } rest
    | "--max-tool-iterations" :: value :: rest ->
        begin
          match int_of_string_opt value with
          | Some v -> parse { acc with max_tool_iterations = v } rest
          | None -> parse acc rest
        end
    | "--debug" :: rest ->
        parse { acc with debug = true } rest
    | "--api-retry-enabled" :: value :: rest ->
        parse { acc with api_retry_enabled = (value = "true" || value = "1") } rest
    | "--api-retry-max-retries" :: value :: rest ->
        begin
          match int_of_string_opt value with
          | Some v -> parse { acc with api_retry_max_retries = v } rest
          | None -> parse acc rest
        end
    | "--api-retry-base-delay-ms" :: value :: rest ->
        begin
          match int_of_string_opt value with
          | Some v -> parse { acc with api_retry_base_delay_ms = v } rest
          | None -> parse acc rest
        end
    | "--api-retry-max-delay-ms" :: value :: rest ->
        begin
          match int_of_string_opt value with
          | Some v -> parse { acc with api_retry_max_delay_ms = v } rest
          | None -> parse acc rest
        end
    | _ :: rest ->
        parse acc rest
  in
  parse default_config args

(** Merge multiple configurations. Later configs override earlier ones. *)
let merge configs =
  let merge_two base override =
    {
      llm_model = if override.llm_model <> default_config.llm_model then override.llm_model else base.llm_model;
      llm_api_key = if override.llm_api_key <> default_config.llm_api_key then override.llm_api_key else base.llm_api_key;
      llm_api_base = if override.llm_api_base <> default_config.llm_api_base then override.llm_api_base else base.llm_api_base;
      data_dir = if override.data_dir <> default_config.data_dir then override.data_dir else base.data_dir;
      max_tool_iterations = if override.max_tool_iterations <> default_config.max_tool_iterations then override.max_tool_iterations else base.max_tool_iterations;
      debug = base.debug || override.debug;
      api_retry_enabled = if override.api_retry_enabled <> default_config.api_retry_enabled then override.api_retry_enabled else base.api_retry_enabled;
      api_retry_max_retries = if override.api_retry_max_retries <> default_config.api_retry_max_retries then override.api_retry_max_retries else base.api_retry_max_retries;
      api_retry_base_delay_ms = if override.api_retry_base_delay_ms <> default_config.api_retry_base_delay_ms then override.api_retry_base_delay_ms else base.api_retry_base_delay_ms;
      api_retry_max_delay_ms = if override.api_retry_max_delay_ms <> default_config.api_retry_max_delay_ms then override.api_retry_max_delay_ms else base.api_retry_max_delay_ms;
    }
  in
  List.fold_left merge_two default_config configs

(** Convenience function to load config with standard priority: file < env < args *)
let load ?config_file ?(cli_args=[]) () =
  let file_config = match config_file with
    | Some path when Sys.file_exists path -> from_file path
    | _ -> default_config
  in
  let env_config = from_env () in
  let args_config = from_args cli_args in
  merge [file_config; env_config; args_config]

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

let validate config =
  let errors = ref [] in
  if String.trim config.llm_api_key = "" then
    errors := "LLM API key is required. Set llm_api_key or OCLAW_API_KEY." :: !errors;
  if config.max_tool_iterations <= 0 then
    errors := "max_tool_iterations must be positive" :: !errors;
  if !errors = [] then Ok config else Error !errors

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let runtime_data_dir config =
  Filename.concat config.data_dir "runtime"

let skills_data_dir config =
  Filename.concat config.data_dir "skills"

let llm_timeout _config =
  default_llm_timeout

(* ============================================================================
   Conversion to LLM Provider Config
   ============================================================================ *)

let to_llm_provider_config config =
  let model = {
    Llm_provider.id = config.llm_model;
    name = config.llm_model;
    reasoning = false;
    input_types = [ "text" ];
    cost = (0.0, 0.0, 0.0, 0.0);
    context_window = 1000000;
    max_tokens = 4096;
  } in
  Llm_provider.{
    api_base = config.llm_api_base;
    api_key = config.llm_api_key;
    model;
    max_tokens = 4096;
    timeout = default_llm_timeout;
  }
