(* OClaw - OCaml port of the ultra-lightweight personal AI assistant *)

open Effect.Deep
open Yojson.Safe.Util

module Http = Http_client
module LLM = Llm_provider
module Tools = Tools
module Memory = Memory
module Config = Oclaw_config.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)
module LogColor = Log_color

(* {1 Legacy agent types for backwards compatibility } *)

type message = {
  content : string;
  sender : string;
  channel : string;
  chat_id : string;
}

type agent_config = {
  provider : LLM.provider_config;
  temperature : float;
  max_tokens : int;
  timeout : int;
}

(* Workspace directory for prompt files *)
let workspace_dir = "workspace"

(* Load a markdown file if it exists *)
let load_workspace_file filename =
  let path = Filename.concat workspace_dir filename in
  if Sys.file_exists path then
    try
      let channel = open_in path in
      let content = really_input_string channel (in_channel_length channel) in
      close_in channel;
      Some content
    with _ -> None
  else
    None

(* Build system prompt from workspace files and skills *)
let build_system_prompt () =
  let parts = ref [] in

  let workspace_files = [
    "IDENTITY.md";
    "AGENTS.md";
    "SOUL.md";
    "USER.md"
  ] in

  List.iter (fun filename ->
    match load_workspace_file filename with
    | Some content -> parts := !parts @ [content]
    | None -> ()
  ) workspace_files;

  let skills = Skills.load_skills () in
  if skills <> [] then
    parts := !parts @ [Skills.skills_to_prompt skills];

  if !parts = [] then
    "You are a helpful AI assistant with access to tools. Use tools when helpful to answer questions about files, directories, or to search the web."
  else
    String.concat "\n\n---\n\n" !parts

(* Enhanced agent that uses our LLM provider with memory *)
let call_llm_api message config session_id =
  let session =
    match Memory.get_history session_id 1 with
    | Some _ -> Memory.create_session session_id
    | None -> Memory.create_session session_id
  in

  let system_prompt = build_system_prompt () in
  let context_json =
    match Memory.build_context session_id system_prompt 4000 with
    | Some json -> json
    | None ->
        `List [`Assoc [("role", `String "system"); ("content", `String system_prompt)]]
  in

  let context_messages = context_json |> Yojson.Basic.Util.to_list in
  let llm_messages =
    context_messages @ [
      `Assoc [
        ("role", `String "user");
        ("content", `String message.content)
      ]
    ]
  in

  let llm_messages_structured = List.map (fun msg_json ->
    let role_str = msg_json |> Yojson.Basic.Util.member "role" |> Yojson.Basic.Util.to_string in
    let content = msg_json |> Yojson.Basic.Util.member "content" |> Yojson.Basic.Util.to_string in
    {
      LLM.role = LLM.string_to_role role_str;
      content = content;
      LLM.tool_call_id = None;
      LLM.tool_calls = []
    }
  ) llm_messages in

  let tools_json = Tools.tools_to_json () in

  let max_tool_rounds = 64 in

  let rec resolve_with_tools messages rounds_remaining =
    let result = LLM.call_llm config.provider messages ~tools:(Some tools_json) () in
    match result with
    | LLM.Error error -> `Error error
    | LLM.Success llm_response ->
        if llm_response.LLM.choices = [] then
          `Error "No choices in LLM response"
        else
          let choice = List.hd llm_response.LLM.choices in
          let tool_calls = choice.LLM.message.LLM.tool_calls in
          if tool_calls = [] then
            `Success choice.LLM.message.LLM.content
          else if rounds_remaining <= 0 then
            `Error "Tool-call recursion limit exceeded"
          else (
            Log.debug (fun m -> m "Processing %d tool calls" (List.length tool_calls));
            let tool_messages = List.map (fun tool_call ->
              let args_json =
                try Yojson.Safe.from_string tool_call.LLM.function_args
                with _ -> `Assoc []
              in
              let result = Tools.execute_tool tool_call.LLM.function_name args_json in
              Log.debug (fun m -> m "Tool %s returned: %s" tool_call.LLM.function_name
                (if String.length result > 100 then String.sub result 0 100 ^ "..." else result));
              {
                LLM.role = LLM.Tool;
                content = result;
                LLM.tool_call_id = Some tool_call.LLM.id;
                LLM.tool_calls = []
              }
            ) tool_calls in
            let next_messages = messages @ [choice.LLM.message] @ tool_messages in
            resolve_with_tools next_messages (rounds_remaining - 1)
          )
  in

  match resolve_with_tools llm_messages_structured max_tool_rounds with
  | `Error error -> `Error error
  | `Success content ->
      let _ = Memory.add_message session content "assistant" Memory.default_cleanup_policy in
      `Success content

(* Simple agent loop *)
let agent_loop config =
  Printf.printf "OClaw agent started with model: %s\n" config.provider.LLM.model.LLM.name;

  let rec loop () =
    Printf.printf "> ";
    flush stdout;
    let input = read_line () in

    if input = "exit" || input = "quit" then (
      Printf.printf "Goodbye!\n";
      ()
    ) else (
      let message = {
        content = input;
        sender = "user";
        channel = "cli";
        chat_id = "test"
      } in

      match call_llm_api message config "cli_session" with
      | `Error error -> Printf.printf "Error: %s\n" error
      | `Success response -> Printf.printf "Response: %s\n" response
      ;
      loop ()
    )
  in
  loop ()

(* {1 Server mode with effect handlers } *)

let run_server_mode config =
  Log.info (fun m -> m "Starting OClaw in server mode with effect handlers");

  let provider = Config.to_llm_provider_config config in
  let base_url = "http://127.0.0.1:8080" in
  Task_api_client.set_default_base_url base_url;
  Task_api_client.set_default_timeout config.llm_timeout;

  let server_config = {
    Oclaw_server.host = "127.0.0.1";
    port = 8080;
    Oclaw_server.llm_config = provider;
    model = config.llm_model;
    max_connections = 100;
    tasks_db_path = config.tasks_db_path;
    tasks_default_limit = config.tasks_default_limit;
    tasks_max_limit = config.tasks_max_limit;
    tasks_busy_timeout_ms = config.tasks_busy_timeout_ms;
    tasks_event_retention_days = config.tasks_event_retention_days;
  } in

  let server = Oclaw_server.create server_config in

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Log.info (fun m -> m "Received shutdown signal");
    Oclaw_server.stop server;
    Printf.printf "\nOClaw server stopped.\n";
    exit 0
  ));

  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
    Log.info (fun m -> m "Received shutdown signal");
    Oclaw_server.stop server;
    Printf.printf "\nOClaw server stopped.\n";
    exit 0
  ));

  Printf.printf "Starting OClaw server with effect handlers on http://127.0.0.1:8080\n";
  Printf.printf "Press Ctrl+C to stop\n\n";

  Oclaw_server.start server;

  while Oclaw_server.is_running server do
    Unix.sleepf 1.0
  done

(* {1 Command line interface } *)

let single_shot = ref false
let server_mode = ref false

let spec = [
  ("--single-shot", Arg.Unit (fun () -> single_shot := true), " Run in single-shot mode (read from stdin, output response, exit)");
  ("--test", Arg.Unit (fun () -> single_shot := true), " Alias for --single-shot");
  ("--server", Arg.Unit (fun () -> server_mode := true), " Run in server mode with effect handlers");
]

let () =
  Arg.parse spec (fun _ -> ()) "Usage: oclaw [--single-shot|--server]";

  (* Setup colored logging *)
  LogColor.setup_auto ~level:(Some Logs.Info) ~format_time:true ();

  Printf.printf "OClaw OCaml version - starting...\n";
  Log.info (fun m -> m "OClaw starting up");

  let config =
    if Sys.file_exists "config.yaml" then (
      Log.info (fun m -> m "Loading configuration from config.yaml");
      Config.load_config "config.yaml"
    ) else (
      Log.warn (fun m -> m "No config.yaml found, creating default configuration");
      ignore (Config.create_default_config "config.yaml");
      Config.default_config
    )
  in

  if config.debug then (
    LogColor.setup_auto ~level:(Some Logs.Debug) ~format_time:true ();
    Log.info (fun m -> m "Debug mode enabled - verbose logging active")
  );

  (match Config.validate_config config with
  | Ok _ -> Printf.printf "Configuration loaded successfully\n"
  | Error errors ->
      Printf.printf "Configuration validation errors:\n";
      List.iter (fun err -> Printf.printf "  - %s\n" err) errors;
      Printf.printf "Using default configuration\n");

  let mode =
    if !server_mode then `server
    else if !single_shot then `single_shot
    else `interactive
  in

  let sandbox_config = {
    Tools.workspace_root = config.tools_workspace;
    restrict_to_workspace = config.tools_restrict_to_workspace;
    allow_read_paths = config.tools_allow_read_paths;
    allow_write_paths = config.tools_allow_write_paths;
    exec_timeout_seconds = config.tools_exec_timeout_seconds;
    exec_enable_deny_patterns = config.tools_exec_enable_deny_patterns;
    exec_custom_deny_patterns = config.tools_exec_custom_deny_patterns;
    exec_custom_allow_patterns = config.tools_exec_custom_allow_patterns;
    web_fetch_max_chars = config.tools_web_fetch_max_chars;
    web_fetch_max_bytes = config.tools_web_fetch_max_bytes;
    python_sessions_enabled = config.tools_python_sessions_enabled;
    python_session_idle_ttl_seconds = config.tools_python_session_idle_ttl_seconds;
    python_session_max_count = config.tools_python_session_max_count;
    python_timeout_seconds = config.tools_python_timeout_seconds;
    python_max_output_chars = config.tools_python_max_output_chars;
    python_max_code_chars = config.tools_python_max_code_chars;
    python_allowed_imports = config.tools_python_allowed_imports;
    python_allowed_subprocess_bins = config.tools_python_allowed_subprocess_bins;
    python_capability_profile = config.tools_python_capability_profile;
  } in

  Tools.init_default_tools ~sandbox_config ();
  let tool_count = List.length (Tools.get_all_tools ()) in
  Printf.printf "Tools system initialized with %d tools\n" tool_count;

  (match mode with
  | `server ->
      Printf.printf "Starting in server mode with effect handlers...\n";
      run_server_mode config
  | `single_shot | `interactive ->
      let provider = Config.to_llm_provider_config config in
      let runtime_agent =
        Agent.create
          ~llm_config:provider
          ~model:config.llm_model
          ~temperature:config.llm_temperature
          ~max_tokens:config.llm_max_tokens
          ()
      in

      if not !single_shot then (
        Printf.printf "OClaw initialization complete.\n";
        Printf.printf "Successfully connected to DashScope Qwen3.5+ API!\n"
      );

      if !single_shot then (
        Log.info (fun m -> m "Running in single-shot mode");
        try
          let input = read_line () in
          match Agent.process_query runtime_agent ~session_id:"single_shot_session" ~content:input with
          | Error error ->
              Printf.printf "Error: %s\n" error;
              exit 1
          | Ok response ->
              Printf.printf "%s\n" response;
              exit 0
        with End_of_file ->
          Log.info (fun m -> m "No input provided in single-shot mode");
          exit 0
      ) else (
        Printf.printf "\nStarting interactive mode (type 'exit' to quit)...\n";
        let rec loop () =
          Printf.printf "> ";
          flush stdout;
          let input = read_line () in
          if input = "exit" || input = "quit" then
            Printf.printf "Goodbye!\n"
          else (
            match Agent.process_query runtime_agent ~session_id:"cli_session" ~content:input with
            | Error error -> Printf.printf "Error: %s\n" error
            | Ok response -> Printf.printf "Response: %s\n" response;
            loop ()
          )
        in
        loop ()
      ));
  exit 0
