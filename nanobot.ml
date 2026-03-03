(* nanobot - OCaml port of the ultra-lightweight personal AI assistant *)

open Yojson.Basic.Util

module Http = Http_client
module LLM = Llm_provider
module Tools = Tools
module Memory = Memory
module Config = Nanobot_config.Config_json

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

(* Enhanced agent that uses our LLM provider with memory *)
let call_llm_api message config session_id =
  (* Get or create conversation session *)
  let session = 
    match Memory.get_history session_id 1 with
    | Some _ -> Memory.create_session session_id (* Already exists, just get it *)
    | None -> Memory.create_session session_id
  in
  
  (* Build context with conversation history *)
  let system_prompt = "You are a helpful AI assistant." in
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
    }
  ) llm_messages in
  
  (* Call the LLM provider *)
  let result = LLM.call_llm config.provider llm_messages_structured () in
  
  match result with
  | LLM.Error error -> `Error error
  | LLM.Success llm_response ->
      (* The LLM provider already parsed the JSON, so we can use the structured response *)
      (* Extract the assistant's response from the parsed LLM response *)
      if llm_response.LLM.choices <> [] then
        let first_choice = List.hd llm_response.LLM.choices in
        (* Add assistant response to memory *)
        let _ = Memory.add_message session first_choice.LLM.message.LLM.content "assistant" Memory.default_cleanup_policy in
        `Success first_choice.LLM.message.LLM.content
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
  Printf.printf "nanobot agent started with model: %s\n" config.provider.LLM.model.LLM.name;
  
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

let () =
  Printf.printf "nanobot OCaml version - starting...\n";
  test_http_client ();
  
  (* Load configuration *)
  let config = 
    if Sys.file_exists "config.yaml" then
      Config.load_config "config.yaml"
    else (
      Printf.printf "No config.yaml found, creating default configuration...\n";
      ignore (Config.create_default_config "config.yaml");
      Config.default_config
    )
  in
  
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
    [Tools.get_tool "web_search"; Tools.get_tool "read_file"; Tools.get_tool "execute_command"] 
    |> List.filter (fun x -> x <> None) 
    |> List.length
  in
  Printf.printf "Tools system initialized with %d tools\n" tool_count;
  
  (* Create LLM provider configuration from config *)
  let provider = Config.to_llm_provider_config config in
  
  (* Simple test - make a test API call *)
  let config = {
    provider = provider;
    temperature = 0.7;
    max_tokens = 100;
    timeout = 60;
  } in
  
  let test_message = {
    content = "Hello! This is nanobot OCaml version talking to Qwen3.5+. What's the capital of France?";
    sender = "test_user";
    channel = "test";
    chat_id = "test_session"
  } in
  
  Printf.printf "Testing LLM API call with DashScope Qwen3.5+...\n";
  match call_llm_api test_message config "test_session" with
  | `Error error -> Printf.printf "LLM API Error: %s\n" error
  | `Success response -> 
      Printf.printf "LLM API Response:\n%s\n" response;
      Printf.printf "Response length: %d characters\n" (String.length response)
  ;
  
  Printf.printf "nanobot initialization complete.\n";
  Printf.printf "Successfully connected to DashScope Qwen3.5+ API!\n";
  
  (* Start interactive agent loop *)
  Printf.printf "\nStarting interactive mode (type 'exit' to quit)...\n";
  agent_loop config