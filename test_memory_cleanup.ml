(* Test memory cleanup policy functionality *)

let test_cleanup_policy () =
  Printf.printf "Testing memory cleanup policy...
";
  
  (* Create a session with custom cleanup policy *)
  let strict_policy = {
    Memory.max_age_seconds = 1.0; (* Very short age limit *)
    Memory.max_tokens = 10; (* Very low token limit *)
    Memory.max_messages = 2; (* Only keep 2 messages *)
    Memory.importance_threshold = 0.8 (* Only keep very important messages *)
  } in
  
  let session = Memory.create_session "cleanup_test" in
  
  (* Add messages with cleanup policy *)
  let session1 = Memory.add_message session "Short message" "user" strict_policy in
  let session2 = Memory.add_message session1 "Another short msg" "assistant" strict_policy in
  let session3 = Memory.add_message session2 "This is a much longer message that should exceed the token limit and get filtered out" "user" strict_policy in
  
  (* Check how many messages remain *)
  (match Memory.get_history "cleanup_test" 10 with
  | Some messages ->
      Printf.printf "After cleanup: %d messages remain (expected <= 2)\
" (List.length messages);
      List.iter (fun msg ->
        Printf.printf "  [%s] %s (tokens: %d, importance: %.1f)\
"
          msg.Memory.role msg.Memory.content msg.Memory.estimated_tokens msg.Memory.importance
      ) messages
  | None ->
      Printf.printf "No messages found\n");
  
  (* Test token-based context building *)
  (match Memory.build_context "cleanup_test" "You are a test assistant." 20 with
  | Some context ->
      let json_str = Yojson.Basic.to_string context in
      Printf.printf "Context built successfully (%d chars): %s\n"
        (String.length json_str) json_str
  | None ->
      Printf.printf "Context building failed (probably due to token limits)\n");
  
  (* Test memory stats with token counting *)
  let stats = Memory.get_stats () in
  Printf.printf "Memory stats with token counting:\n";
  Printf.printf "  Total sessions: %d\n" stats.Memory.total_sessions;
  Printf.printf "  Total messages: %d\n" stats.Memory.total_messages;
  Printf.printf "  Estimated tokens: %d\n" stats.Memory.estimated_tokens;
  
  (* Clean up *)
  Memory.clear_history "cleanup_test";
  Printf.printf "Cleanup policy test completed.\n"

let () =
  test_cleanup_policy ()