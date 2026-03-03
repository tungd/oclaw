(* Test file for memory system functionality *)

let test_memory_system () =
  Printf.printf "Testing memory system...\n";
  
  (* Create a session *)
  let session = Memory.create_session "test_session" in
  Printf.printf "Created session: %s\n" session.Memory.session_id;
  
  (* Add some messages *)
  let session1 = Memory.add_message session "Hello!" "user" Memory.default_cleanup_policy in
  let session2 = Memory.add_message session1 "Hi there! How can I help?" "assistant" Memory.default_cleanup_policy in
  let session3 = Memory.add_message session2 "What's the weather today?" "user" Memory.default_cleanup_policy in
  
  (* Get history *)
  (match Memory.get_history "test_session" 5 with
  | Some messages ->
      Printf.printf "Session history (%d messages):\n" (List.length messages);
      List.iter (fun msg ->
        Printf.printf "  [%s] %s\n" msg.Memory.role msg.Memory.content
      ) messages
  | None ->
      Printf.printf "No history found\n");
  
  (* Get context *)
  (match Memory.get_context "test_session" with
  | Some ctx ->
      Printf.printf "Current context (%d items): %s\n" (List.length ctx) (String.concat ", " ctx)
  | None ->
      Printf.printf "No context found\n");
  
  (* Test session summary *)
  (match Memory.get_session_summary "test_session" with
  | Some summary ->
      Printf.printf "Session summary: %s\n" summary
  | None ->
      Printf.printf "No session found\n");
  
  (* Test memory stats *)
  let stats = Memory.get_stats () in
  Printf.printf "Memory stats:\n";
  Printf.printf "  Total sessions: %d\n" stats.Memory.total_sessions;
  Printf.printf "  Total messages: %d\n" stats.Memory.total_messages;
  Printf.printf "  Avg messages/session: %d\n" stats.Memory.avg_messages_per_session;
  
  (* Test save/load *)
  Printf.printf "Testing save/load...\n";
  (match Memory.save_memory "test_memory.json" with
  | true ->
      (match Memory.load_memory "test_memory.json" with
      | true ->
          let loaded_stats = Memory.get_stats () in
          Printf.printf "Successfully loaded memory: %d sessions, %d messages\n"
            loaded_stats.Memory.total_sessions loaded_stats.Memory.total_messages
      | false ->
          Printf.printf "Failed to load memory\n")
  | false ->
      Printf.printf "Failed to save memory\n");
  
  (* Clean up *)
  Memory.clear_history "test_session";
  Printf.printf "Memory test completed.\n"

let () =
  test_memory_system ()