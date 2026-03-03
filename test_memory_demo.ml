(* Simple test to demonstrate memory working *)

let () =
  Printf.printf "Testing memory system...\n";
  
  (* Create a session and add some messages *)
  let session = Memory.create_session "demo" in
  Printf.printf "Created session: %s\n" session.session_id;
  
  (* Add some messages *)
  let session = Memory.add_message session "Hello, how are you?" "user" Memory.default_cleanup_policy in
  let session = Memory.add_message session "I'm doing well, thank you!" "assistant" Memory.default_cleanup_policy in
  let _ = session (* Keep session alive *) in
  
  (* Get history *)
  match Memory.get_history "demo" 10 with
  | Some messages ->
      Printf.printf "Conversation history (%d messages):\n" (List.length messages);
      List.iter (fun msg ->
        Printf.printf "  [%s] %s\n" msg.Memory.role msg.Memory.content
      ) messages
  | None ->
      Printf.printf "No history found\n";
  
  (* Build context *)
  match Memory.build_context "demo" "You are a helpful assistant." 1000 with
  | Some context_json ->
      Printf.printf "Built context: %s\n" (Yojson.Basic.to_string context_json)
  | None ->
      Printf.printf "Could not build context\n"