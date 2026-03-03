(* Test file for LLM provider functionality *)

let test_dashscope_provider () =
  Printf.printf "Testing DashScope LLM provider...\n";
  
  (* Create provider configuration *)
  let provider = Llm_provider.create_dashscope_provider ~temperature:0.7 ~max_tokens:1000 () in
  
  Printf.printf "Provider configured:\n";
  Printf.printf "  API Base: %s\n" provider.Llm_provider.api_base;
  Printf.printf "  Model: %s\n" provider.model.Llm_provider.name;
  Printf.printf "  Temperature: %.2f\n" provider.temperature;
  Printf.printf "  Max Tokens: %d\n" provider.max_tokens;
  
  (* Create test messages *)
  let messages = [
    {
      Llm_provider.role = Llm_provider.User;
      content = "Hello! This is a test of the DashScope LLM provider.";
      tool_call_id = None;
      tool_calls = []
    }
  ] in
  
  Printf.printf "Sending test message to LLM...\n";
  
  (* Call LLM *)
  let result = Llm_provider.call_llm provider messages () in
  
  match result with
  | Llm_provider.Error error ->
      Printf.printf "LLM Error: %s\n" error
  | Llm_provider.Success response ->
      Printf.printf "LLM Success!\n";
      Printf.printf "  Model: %s\n" response.model;
      Printf.printf "  Created: %d\n" response.created;
      Printf.printf "  Choices: %d\n" (List.length response.choices);
      
      (* Extract assistant message *)
      match Llm_provider.get_assistant_message result with
      | Some msg ->
          Printf.printf "  Assistant Response (first 200 chars): %s\n"
            (String.sub msg.Llm_provider.content 0 (min 200 (String.length msg.content)))
      | None ->
          Printf.printf "  No assistant message found\n"

let test_message_conversion () =
  Printf.printf "\nTesting message role conversion...\n";
  
  let test_roles = [
    Llm_provider.System;
    Llm_provider.User;
    Llm_provider.Assistant;
    Llm_provider.Tool;
  ] in
  
  List.iter (fun role ->
    let role_str = Llm_provider.role_to_string role in
    let converted_back = Llm_provider.string_to_role role_str in
    Printf.printf "  %s -> %s -> %s\n"
      (match role with Llm_provider.System -> "System" | Llm_provider.User -> "User" | Llm_provider.Assistant -> "Assistant" | Llm_provider.Tool -> "Tool")
      role_str
      (match converted_back with Llm_provider.System -> "System" | Llm_provider.User -> "User" | Llm_provider.Assistant -> "Assistant" | Llm_provider.Tool -> "Tool")
  ) test_roles

let () =
  test_dashscope_provider ();
  test_message_conversion ();
  Printf.printf "\nAll LLM tests completed.\n"