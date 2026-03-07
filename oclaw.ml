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
module AcpTransport = Acp_transport
module AcpClient = Acp_client
module AcpServer = Acp_server

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
    "You are a helpful AI assistant with access to filesystem and shell tools. Use skills and primitive tools together when helpful."
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

let run_server_mode config endpoint =
  Log.info (fun m -> m "Starting OClaw ACP server mode");

  let provider = Config.to_llm_provider_config config in

  let server_config = {
    AcpServer.host = endpoint.AcpTransport.host;
    port = endpoint.port;
    AcpServer.llm_config = provider;
    model = config.llm_model;
    tasks_db_path = config.tasks_db_path;
    tasks_default_limit = config.tasks_default_limit;
    tasks_max_limit = config.tasks_max_limit;
    tasks_busy_timeout_ms = config.tasks_busy_timeout_ms;
    tasks_event_retention_days = config.tasks_event_retention_days;
  } in

  let server = AcpServer.create server_config in

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Log.info (fun m -> m "Received shutdown signal");
    AcpServer.stop server;
    Printf.printf "\nOClaw server stopped.\n";
    exit 0
  ));

  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
    Log.info (fun m -> m "Received shutdown signal");
    AcpServer.stop server;
    Printf.printf "\nOClaw server stopped.\n";
    exit 0
  ));

  Printf.printf "Starting OClaw ACP server on %s\n" (AcpTransport.endpoint_to_string endpoint);
  Printf.printf "Press Ctrl+C to stop\n\n";

  AcpServer.start server;

  while AcpServer.is_running server do
    Unix.sleepf 1.0
  done

let read_single_shot_input () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done;
    ""
  with End_of_file ->
    List.rev !lines |> String.concat "\n"

let run_client_mode endpoint ~single_shot =
  if single_shot then
    let input = read_single_shot_input () in
    if String.trim input = "" then Ok ()
    else AcpClient.run_single_shot endpoint input
  else
    AcpClient.run_interactive endpoint

(* {1 Command line interface } *)

let single_shot = ref false
let server_mode = ref false
let connect_target = ref None

let spec = [
  ("--single-shot", Arg.Unit (fun () -> single_shot := true), " Run in single-shot mode (read from stdin, output response, exit)");
  ("--test", Arg.Unit (fun () -> single_shot := true), " Alias for --single-shot");
  ("--server", Arg.Unit (fun () -> server_mode := true), " Run in ACP server mode");
  ("--connect", Arg.String (fun s -> connect_target := Some s), " Connect to ACP agent endpoint (host:port)");
]

let () =
  Arg.parse spec (fun _ -> ()) "Usage: oclaw [--server] [--connect host:port] [--single-shot]";

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

  let endpoint =
    match !connect_target with
    | None -> AcpTransport.default_endpoint
    | Some raw ->
        begin
          match AcpTransport.parse_endpoint raw with
          | Ok endpoint -> endpoint
          | Error err ->
              Printf.eprintf "Invalid ACP endpoint: %s\n" err;
              exit 2
        end
  in

  if !server_mode then (
    let sandbox_config = {
      Tools.workspace_root = config.tools_workspace;
      restrict_to_workspace = config.tools_restrict_to_workspace;
      allow_read_paths = config.tools_allow_read_paths;
      allow_write_paths = config.tools_allow_write_paths;
      exec_timeout_seconds = config.tools_exec_timeout_seconds;
      exec_enable_deny_patterns = config.tools_exec_enable_deny_patterns;
      exec_custom_deny_patterns = config.tools_exec_custom_deny_patterns;
      exec_custom_allow_patterns = config.tools_exec_custom_allow_patterns;
    } in
    Tools.init_default_tools ~sandbox_config ();
    let tool_count = List.length (Tools.get_all_tools ()) in
    Printf.printf "Tools system initialized with %d tools\n" tool_count;
    run_server_mode config endpoint
  ) else (
    match run_client_mode endpoint ~single_shot:!single_shot with
    | Ok () -> ()
    | Error err ->
        Printf.eprintf "ACP client error: %s\n" err;
        exit 1
  );
  exit 0
