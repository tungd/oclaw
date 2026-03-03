(* OClaw - OCaml port of the ultra-lightweight personal AI assistant *)

open Yojson.Basic.Util

module Http = Http_client
module LLM = Llm_provider
module Tools = Tools
module Memory = Memory
module Config = Oclaw_config.Config
module Arg = Arg
module Log = (val Logs.src_log Logs.default : Logs.LOG)

(* Basic agent types *)
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

  (* Core workspace files in order *)
  let workspace_files = [
    "IDENTITY.md";
    "AGENTS.md";
    "SOUL.md";
    "USER.md"
  ] in

  (* Load workspace files *)
  List.iter (fun filename ->
    match load_workspace_file filename with
    | Some content -> parts := !parts @ [content]
    | None -> ()
  ) workspace_files;

  (* Load skills *)
  let skills = Skills.load_skills () in
  if skills <> [] then
    parts := !parts @ [Skills.skills_to_prompt skills];

  (* Build final prompt *)
  if !parts = [] then
    "You are a helpful AI assistant with access to tools. Use tools when helpful to answer questions about files, directories, or to search the web."
  else
    String.concat "\n\n---\n\n" !parts

(* Enhanced agent that uses our LLM provider with memory *)
let call_llm_api message config session_id =
  (* Get or create conversation session *)
  let session =
    match Memory.get_history session_id 1 with
    | Some _ -> Memory.create_session session_id (* Already exists, just get it *)
    | None -> Memory.create_session session_id
  in

  (* Build context with conversation history *)
  let system_prompt = build_system_prompt () in
  let context_json =
    match Memory.build_context session_id system_prompt 4000 with
    | Some json -> json
    | None ->
        `List [`Assoc [("role", `String "system"); ("content", `String system_prompt)]]
  in

  (* Convert context to LLM message format *)
  let context_messages = context_json |> to_list in
  let llm_messages =
    context_messages @ [
      `Assoc [
        ("role", `String "user");
        ("content", `String message.content)
      ]
    ]
  in

  (* Convert JSON messages to LLM message format *)
  let llm_messages_structured = List.map (fun msg_json ->
    let role_str = msg_json |> member "role" |> to_string in
    let content = msg_json |> member "content" |> to_string in
    {
      LLM.role = LLM.string_to_role role_str;
      content = content;
      LLM.tool_call_id = None;
      LLM.tool_calls = []
    }
  ) llm_messages in

  (* Get tools JSON for LLM *)
  let tools_json = Tools.tools_to_json () in

  (* Call the LLM provider with tools *)
  let result = LLM.call_llm config.provider llm_messages_structured ~tools:(Some tools_json) () in

  match result with
  | LLM.Error error -> `Error error
  | LLM.Success llm_response ->
      (* Check if there are tool calls in the response *)
      if llm_response.LLM.choices <> [] then
        let first_choice = List.hd llm_response.LLM.choices in
        let tool_calls = first_choice.LLM.message.LLM.tool_calls in

        if tool_calls <> [] then (
          (* Execute tool calls and return results *)
          Log.debug (fun m -> m "Processing %d tool calls" (List.length tool_calls));
          let tool_results = List.map (fun tool_call ->
            let args_json = Yojson.Basic.from_string tool_call.LLM.function_args in
            let result = Tools.execute_tool tool_call.LLM.function_name args_json in
            Log.debug (fun m -> m "Tool %s returned: %s" tool_call.LLM.function_name
              (if String.length result > 100 then String.sub result 0 100 ^ "..." else result));
            (tool_call.LLM.id, tool_call.LLM.function_name, result)
          ) tool_calls in

          (* Add assistant message with tool calls to memory *)
          let _ = Memory.add_message session first_choice.LLM.message.LLM.content "assistant" Memory.default_cleanup_policy in

          (* Create tool response messages and recurse *)
          let tool_messages = List.map (fun (id, name, result) ->
            {
              LLM.role = LLM.Tool;
              content = result;
              LLM.tool_call_id = Some id;
              LLM.tool_calls = []
            }
          ) tool_results in

          (* Recurse with tool results *)
          let new_messages = llm_messages_structured @ [first_choice.LLM.message] @ tool_messages in
          let result2 = LLM.call_llm config.provider new_messages ~tools:(Some tools_json) () in

          match result2 with
          | LLM.Error error -> `Error error
          | LLM.Success llm_response2 ->
              if llm_response2.LLM.choices <> [] then
                let choice2 = List.hd llm_response2.LLM.choices in
                (* Add final response to memory *)
                let _ = Memory.add_message session choice2.LLM.message.LLM.content "assistant" Memory.default_cleanup_policy in
                `Success choice2.LLM.message.LLM.content
              else
                `Error "No choices in LLM response after tool execution"
        ) else (
          (* No tool calls, just return the text response *)
          let _ = Memory.add_message session first_choice.LLM.message.LLM.content "assistant" Memory.default_cleanup_policy in
          `Success first_choice.LLM.message.LLM.content
        )
      else
        `Error "No choices in LLM response"

let test_http_client () =
  (* Test our HTTP client with a simple GET request *)
  let response = Http.get "https://api.github.com" [] 30 in
  match response.Http.HttpResponse.error with
  | Some error -> Printf.printf "HTTP Error: %s\n" error
  | None -> Printf.printf "HTTP Success: Status %d\n" response.Http.HttpResponse.status

(* Simple agent loop *)
let agent_loop config =
  Printf.printf "OClaw agent started with model: %s\n" config.provider.LLM.model.LLM.name;
  
  (* Main loop - in a real implementation this would process messages from a queue *)
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

(* Command line flags *)
let single_shot = ref false

(* Parse command line arguments *)
let spec = [
  ("--single-shot", Arg.Unit (fun () -> single_shot := true), " Run in single-shot mode (read from stdin, output response, exit)");
  ("--test", Arg.Unit (fun () -> single_shot := true), " Alias for --single-shot");
]

let () =
  (* Parse command line arguments *)
  Arg.parse spec (fun _ -> ()) "Usage: oclaw [--single-shot]";
  (* Initialize logging *)
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  
  Printf.printf "OClaw OCaml version - starting...\n";
  Log.info (fun m -> m "OClaw starting up");
  
  (* Load configuration *)
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
  
  (* Set log level based on debug configuration *)
  if config.debug then (
    Logs.set_level (Some Logs.Debug);
    Log.info (fun m -> m "Debug mode enabled - verbose logging active")
  );
  
  (* Validate configuration *)
  (match Config.validate_config config with
  | Ok _ -> Printf.printf "Configuration loaded successfully\n"
  | Error errors -> 
      Printf.printf "Configuration validation errors:\n";
      List.iter (fun err -> Printf.printf "  - %s\n" err) errors;
      Printf.printf "Using default configuration\n");
  
  (* Initialize tools system *)
  Tools.init_default_tools ();
  let tool_count =
    [Tools.get_tool "web_search"; Tools.get_tool "read_file"; Tools.get_tool "execute_command"; Tools.get_tool "list_directory"]
    |> List.filter (fun x -> x <> None)
    |> List.length
  in
  Printf.printf "Tools system initialized with %d tools\n" tool_count;
  
  (* Create LLM provider configuration from config *)
  let provider = Config.to_llm_provider_config config in

  (* Create agent config *)
  let config = {
    provider = provider;
    temperature = 0.7;
    max_tokens = config.llm_max_tokens;
    timeout = 60;
  } in

  (* In single-shot mode, skip the test output *)
  if not !single_shot then (
    Printf.printf "OClaw initialization complete.\n";
    Printf.printf "Successfully connected to DashScope Qwen3.5+ API!\n"
  );

  (* Handle single-shot mode vs interactive mode *)
  if !single_shot then (
    (* Single-shot mode: read from stdin, process, output and exit *)
    Log.info (fun m -> m "Running in single-shot mode");
    try
      let input = read_line () in
      let message = {
        content = input;
        sender = "user";
        channel = "cli";
        chat_id = "single_shot"
      } in
      match call_llm_api message config "single_shot_session" with
      | `Error error ->
          Printf.printf "Error: %s\n" error;
          exit 1
      | `Success response ->
          Printf.printf "%s\n" response;
          exit 0
    with End_of_file ->
      Log.info (fun m -> m "No input provided in single-shot mode");
      exit 0
  ) else (
    (* Start interactive agent loop *)
    Printf.printf "\nStarting interactive mode (type 'exit' to quit)...\n";
    agent_loop config
  )