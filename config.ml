(** YAML-based configuration using ppx_protocol_conv codecs *)

module Yaml_lib = Yaml
module Yaml = Protocol_conv_yaml.Yaml

module Log = (val Logs.src_log (Logs.Src.create "config") : Logs.LOG)

type config = {
  llm_provider : string [@default "dashscope"];
  llm_model : string [@default "qwen3.5-plus"];
  llm_temperature : float [@default 0.7];
  llm_max_tokens : int [@default 8192];
  llm_api_key : string [@default "sk-sp-4326acec735b4e29b33b31e97d1d66fa"];
  llm_api_base : string [@default "https://coding-intl.dashscope.aliyuncs.com/v1"];
  llm_timeout : int [@default 60];
  tools_enabled : bool [@default true];
  tools_max_concurrent : int [@default 3];
  tools_timeout : int [@default 30];
  tools_workspace : string [@default "."];
  tools_restrict_to_workspace : bool [@default true];
  tools_allow_read_paths : string list [@default []];
  tools_allow_write_paths : string list [@default []];
  tools_exec_timeout_seconds : int [@default 60];
  tools_exec_enable_deny_patterns : bool [@default true];
  tools_exec_custom_deny_patterns : string list [@default []];
  tools_exec_custom_allow_patterns : string list [@default []];
  tasks_db_path : string [@default "workspace/state/tasks.db"];
  tasks_default_limit : int [@default 50];
  tasks_max_limit : int [@default 200];
  tasks_busy_timeout_ms : int [@default 5000];
  tasks_event_retention_days : int [@default 30];
  agent_system_prompt : string [@default "You are a helpful AI assistant."];
  agent_memory_window : int [@default 10];
  agent_max_iterations : int [@default 5];
  debug : bool [@default false];
  log_file : string option [@default Some "oclaw.log"];
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
    Out_channel.with_open_text filename (fun channel ->
      output_string channel yaml_string
    );
    true
  with exn ->
    Printf.printf "Error saving config: %s\n" (Printexc.to_string exn);
    false

let create_default_config filename =
  save_config filename default_config

(* Convert config to LLM provider config *)
let to_llm_provider_config config =
  Log.info (fun m -> m "Converting config to LLM provider config with model: %s" config.llm_model);
  (* Create a model based on the configured model name *)
  let model = {
    Llm_provider.id = config.llm_model;
    Llm_provider.name = config.llm_model;
    Llm_provider.reasoning = false;
    Llm_provider.input_types = ["text"];
    Llm_provider.cost = (0.0, 0.0, 0.0, 0.0);
    Llm_provider.context_window = 1000000;
    Llm_provider.max_tokens = config.llm_max_tokens
  } in
  if config.debug then
    Printf.printf "Using LLM model: %s\n" config.llm_model;
  Llm_provider.{
    api_base = config.llm_api_base;
    api_key = config.llm_api_key;
    model = model;
    temperature = config.llm_temperature;
    max_tokens = config.llm_max_tokens;
    timeout = config.llm_timeout
  }

(* Validate configuration *)
let validate_config config =
  let errors = ref [] in

  (* Check LLM configuration *)
  if config.llm_api_key = "" then
    errors := "LLM API key is empty" :: !errors;
  if config.llm_timeout <= 0 then
    errors := "LLM timeout must be positive" :: !errors;
  if config.llm_max_tokens <= 0 then
    errors := "LLM max_tokens must be positive" :: !errors;

  (* Check tools configuration *)
  if config.tools_max_concurrent <= 0 then
    errors := "Tools max_concurrent must be positive" :: !errors;
  if config.tools_timeout <= 0 then
    errors := "Tools timeout must be positive" :: !errors;
  if config.tools_exec_timeout_seconds <= 0 then
    errors := "Tools exec_timeout_seconds must be positive" :: !errors;
  if config.tasks_default_limit <= 0 then
    errors := "Tasks default_limit must be positive" :: !errors;
  if config.tasks_max_limit <= 0 then
    errors := "Tasks max_limit must be positive" :: !errors;
  if config.tasks_busy_timeout_ms <= 0 then
    errors := "Tasks busy_timeout_ms must be positive" :: !errors;
  if config.tasks_event_retention_days <= 0 then
    errors := "Tasks event_retention_days must be positive" :: !errors;

  (* Check agent configuration *)
  if config.agent_memory_window <= 0 then
    errors := "Agent memory_window must be positive" :: !errors;
  if config.agent_max_iterations <= 0 then
    errors := "Agent max_iterations must be positive" :: !errors;

  if !errors = [] then
    Ok config
  else
    Error !errors
