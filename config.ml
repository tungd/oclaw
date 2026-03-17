(** YAML-backed configuration for the CLI-first assistant. *)

module Yaml_lib = Yaml
module Yaml = Protocol_conv_yaml.Yaml

type config = {
  llm_provider : string [@default "dashscope"];
  llm_model : string [@default "qwen3.5-plus"];
  llm_temperature : float [@default 0.7];
  llm_max_tokens : int [@default 4096];
  llm_api_key : string [@default ""];
  llm_api_base : string [@default "https://coding-intl.dashscope.aliyuncs.com/v1"];
  llm_timeout : int [@default 60];
  data_dir : string [@default "workspace"];
  skills_dir : string [@default ""];
  max_history_messages : int [@default 24];
  max_tool_iterations : int [@default 256];
  tools_workspace : string [@default "."];
  tools_restrict_to_workspace : bool [@default true];
  tools_allow_read_paths : string list [@default []];
  tools_allow_write_paths : string list [@default []];
  tools_exec_timeout_seconds : int [@default 60];
  tools_exec_enable_deny_patterns : bool [@default true];
  tools_exec_custom_deny_patterns : string list [@default []];
  tools_exec_custom_allow_patterns : string list [@default []];
  debug : bool [@default false];
}
[@@deriving protocol ~driver:(module Yaml)]

let default_config : config =
  config_of_yaml_exn (`O [])

let load_config filename =
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

let save_config filename config =
  try
    let yaml_value = config_to_yaml config in
    let yaml_string = Yaml_lib.to_string_exn yaml_value in
    Out_channel.with_open_text filename (fun channel -> output_string channel yaml_string);
    true
  with exn ->
    Printf.printf "Error saving config: %s\n" (Printexc.to_string exn);
    false

let create_default_config filename =
  save_config filename default_config

let env_string name current =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> current

let env_bool name current =
  match Sys.getenv_opt name with
  | Some value ->
      let value = String.lowercase_ascii (String.trim value) in
      value = "1" || value = "true" || value = "yes" || (current && value <> "0" && value <> "false" && value <> "no")
  | None -> current

let env_int name current =
  match Sys.getenv_opt name with
  | Some value ->
      begin
        match int_of_string_opt (String.trim value) with
        | Some parsed -> parsed
        | None -> current
      end
  | None -> current

let env_float name current =
  match Sys.getenv_opt name with
  | Some value ->
      begin
        match float_of_string_opt (String.trim value) with
        | Some parsed -> parsed
        | None -> current
      end
  | None -> current

let env_paths name current =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" ->
      String.split_on_char ':' value |> List.filter (fun path -> String.trim path <> "")
  | _ -> current

let apply_env_overrides config =
  {
    config with
    llm_provider = env_string "OCLAW_LLM_PROVIDER" config.llm_provider;
    llm_model = env_string "OCLAW_MODEL" config.llm_model;
    llm_temperature = env_float "OCLAW_TEMPERATURE" config.llm_temperature;
    llm_max_tokens = env_int "OCLAW_MAX_TOKENS" config.llm_max_tokens;
    llm_api_key = env_string "OCLAW_API_KEY" config.llm_api_key;
    llm_api_base = env_string "OCLAW_API_BASE" config.llm_api_base;
    llm_timeout = env_int "OCLAW_TIMEOUT" config.llm_timeout;
    data_dir = env_string "OCLAW_DATA_DIR" config.data_dir;
    skills_dir = env_string "OCLAW_SKILLS_DIR" config.skills_dir;
    max_history_messages = env_int "OCLAW_MAX_HISTORY_MESSAGES" config.max_history_messages;
    max_tool_iterations = env_int "OCLAW_MAX_TOOL_ITERATIONS" config.max_tool_iterations;
    tools_workspace = env_string "OCLAW_WORKSPACE" config.tools_workspace;
    tools_restrict_to_workspace = env_bool "OCLAW_RESTRICT_TO_WORKSPACE" config.tools_restrict_to_workspace;
    tools_allow_read_paths = env_paths "OCLAW_ALLOW_READ_PATHS" config.tools_allow_read_paths;
    tools_allow_write_paths = env_paths "OCLAW_ALLOW_WRITE_PATHS" config.tools_allow_write_paths;
    tools_exec_timeout_seconds = env_int "OCLAW_EXEC_TIMEOUT" config.tools_exec_timeout_seconds;
    tools_exec_enable_deny_patterns =
      env_bool "OCLAW_EXEC_ENABLE_DENY_PATTERNS" config.tools_exec_enable_deny_patterns;
    tools_exec_custom_deny_patterns =
      env_paths "OCLAW_EXEC_CUSTOM_DENY_PATTERNS" config.tools_exec_custom_deny_patterns;
    tools_exec_custom_allow_patterns =
      env_paths "OCLAW_EXEC_CUSTOM_ALLOW_PATTERNS" config.tools_exec_custom_allow_patterns;
    debug = env_bool "OCLAW_DEBUG" config.debug;
  }

let to_llm_provider_config config =
  let model = {
    Llm_provider.id = config.llm_model;
    name = config.llm_model;
    reasoning = false;
    input_types = [ "text" ];
    cost = (0.0, 0.0, 0.0, 0.0);
    context_window = 1000000;
    max_tokens = config.llm_max_tokens;
  } in
  Llm_provider.{
    api_base = config.llm_api_base;
    api_key = config.llm_api_key;
    model;
    temperature = config.llm_temperature;
    max_tokens = config.llm_max_tokens;
    timeout = config.llm_timeout;
  }

let validate_config config =
  let errors = ref [] in
  if String.trim config.llm_api_key = "" then
    errors := "LLM API key is required. Set llm_api_key or OCLAW_API_KEY." :: !errors;
  if config.llm_timeout <= 0 then
    errors := "LLM timeout must be positive" :: !errors;
  if config.llm_max_tokens <= 0 then
    errors := "LLM max_tokens must be positive" :: !errors;
  if config.max_history_messages <= 0 then
    errors := "max_history_messages must be positive" :: !errors;
  if config.max_tool_iterations <= 0 then
    errors := "max_tool_iterations must be positive" :: !errors;
  if config.tools_exec_timeout_seconds <= 0 then
    errors := "Tools exec_timeout_seconds must be positive" :: !errors;
  if !errors = [] then Ok config else Error !errors

let runtime_data_dir config =
  Filename.concat config.data_dir "runtime"

let skills_data_dir config =
  if String.trim config.skills_dir <> "" then config.skills_dir
  else Filename.concat config.data_dir "skills"
