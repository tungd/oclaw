(** Simple JSON-based Configuration system *)

open Yojson.Basic.Util

(* Configuration types *)
type config = {
  llm_provider : string;
  llm_model : string;
  llm_temperature : float;
  llm_max_tokens : int;
  llm_api_key : string;
  llm_api_base : string;
  llm_timeout : int;
  tools_enabled : bool;
  tools_max_concurrent : int;
  tools_timeout : int;
  agent_system_prompt : string;
  agent_memory_window : int;
  agent_max_iterations : int;
  debug : bool;
  log_file : string option
}

(* Helper functions with defaults *)
let to_string_with_default default = function
  | `String s -> s
  | _ -> default

let to_float_with_default default = function
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> default

let to_int_with_default default = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | _ -> default

let to_bool_with_default default = function
  | `Bool b -> b
  | `String "true" -> true
  | `String "false" -> false
  | _ -> default

(* Default configuration *)
let default_config = {
  llm_provider = "dashscope";
  llm_model = "qwen3.5-plus";
  llm_temperature = 0.7;
  llm_max_tokens = 1000;
  llm_api_key = "sk-sp-4326acec735b4e29b33b31e97d1d66fa";
  llm_api_base = "https://coding-intl.dashscope.aliyuncs.com/v1";
  llm_timeout = 60;
  tools_enabled = true;
  tools_max_concurrent = 3;
  tools_timeout = 30;
  agent_system_prompt = "You are a helpful AI assistant.";
  agent_memory_window = 10;
  agent_max_iterations = 5;
  debug = false;
  log_file = Some "nanobot.log"
}

(* Load configuration from JSON file *)
let load_config filename =
  try
    let json = Yojson.Basic.from_file filename in
    {
      llm_provider = json |> member "llm_provider" |> to_string_with_default "dashscope";
      llm_model = json |> member "llm_model" |> to_string_with_default "qwen3.5-plus";
      llm_temperature = json |> member "llm_temperature" |> to_float_with_default 0.7;
      llm_max_tokens = json |> member "llm_max_tokens" |> to_int_with_default 1000;
      llm_api_key = json |> member "llm_api_key" |> to_string_with_default "sk-sp-4326acec735b4e29b33b31e97d1d66fa";
      llm_api_base = json |> member "llm_api_base" |> to_string_with_default "https://coding-intl.dashscope.aliyuncs.com/v1";
      llm_timeout = json |> member "llm_timeout" |> to_int_with_default 60;
      tools_enabled = json |> member "tools_enabled" |> to_bool_with_default true;
      tools_max_concurrent = json |> member "tools_max_concurrent" |> to_int_with_default 3;
      tools_timeout = json |> member "tools_timeout" |> to_int_with_default 30;
      agent_system_prompt = json |> member "agent_system_prompt" |> to_string_with_default "You are a helpful AI assistant.";
      agent_memory_window = json |> member "agent_memory_window" |> to_int_with_default 10;
      agent_max_iterations = json |> member "agent_max_iterations" |> to_int_with_default 5;
      debug = json |> member "debug" |> to_bool_with_default false;
      log_file = 
        try Some (json |> member "log_file" |> to_string)
        with _ -> Some "nanobot.log"
    }
  with exn ->
    Printf.printf "Error loading config: %s\nUsing default configuration.\n" (Printexc.to_string exn);
    default_config

(* Save configuration to JSON file *)
let save_config filename config =
  try
    let json = `Assoc [
      ("llm_provider", `String config.llm_provider);
      ("llm_model", `String config.llm_model);
      ("llm_temperature", `Float config.llm_temperature);
      ("llm_max_tokens", `Int config.llm_max_tokens);
      ("llm_api_key", `String config.llm_api_key);
      ("llm_api_base", `String config.llm_api_base);
      ("llm_timeout", `Int config.llm_timeout);
      ("tools_enabled", `Bool config.tools_enabled);
      ("tools_max_concurrent", `Int config.tools_max_concurrent);
      ("tools_timeout", `Int config.tools_timeout);
      ("agent_system_prompt", `String config.agent_system_prompt);
      ("agent_memory_window", `Int config.agent_memory_window);
      ("agent_max_iterations", `Int config.agent_max_iterations);
      ("debug", `Bool config.debug);
      ("log_file", match config.log_file with Some f -> `String f | None -> `Null)
    ] in
    let channel = open_out filename in
    Yojson.Basic.to_channel channel json;
    close_out channel;
    true
  with exn ->
    Printf.printf "Error saving config: %s\n" (Printexc.to_string exn);
    false

(* Create default configuration file *)
let create_default_config filename =
  save_config filename default_config

(* Convert config to LLM provider config *)
let to_llm_provider_config config =
  Llm_provider.{
    api_base = config.llm_api_base;
    api_key = config.llm_api_key;
    model = Llm_provider.qwen35_plus_model; (* Use the default model *)
    temperature = config.llm_temperature;
    max_tokens = config.llm_max_tokens;
    timeout = config.llm_timeout
  }

(* Helper functions with defaults *)
let to_string_with_default default = function
  | `String s -> s
  | _ -> default

let to_float_with_default default = function
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> default

let to_int_with_default default = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | _ -> default

let to_bool_with_default default = function
  | `Bool b -> b
  | `String "true" -> true
  | `String "false" -> false
  | _ -> default

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
  
  (* Check agent configuration *)
  if config.agent_memory_window <= 0 then
    errors := "Agent memory_window must be positive" :: !errors;
  if config.agent_max_iterations <= 0 then
    errors := "Agent max_iterations must be positive" :: !errors;
  
  if !errors = [] then
    Ok config
  else
    Error !errors