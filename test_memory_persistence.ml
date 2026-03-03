(* Test memory persistence *)

let () =
  Printf.printf "Testing memory persistence...\n";
  
  (* Clear any existing memory *)
  let _ = Memory.clear_history "persist_test" in
  
  (* Create a session and add some messages *)
  let session = Memory.create_session "persist_test" in
  Printf.printf "Created session\n";
  
  (* Add some messages *)
  let session = Memory.add_message session "Hello" "user" Memory.default_cleanup_policy in
  let session = Memory.add_message session "Hi there!" "assistant" Memory.default_cleanup_policy in
  let _ = session in
  
  (* Save memory to file *)
  Printf.printf "Saving...\n";
  let save_success = Memory.save_memory "memory_test.json" in
  Printf.printf "Save: %b\n" save_success;
  
  (* Clear memory *)
  let _ = Memory.clear_history "persist_test" in
  
  (* Load memory from file *)
  Printf.printf "Loading...\n";
  let load_success = Memory.load_memory "memory_test.json" in
  Printf.printf "Load: %b\n" load_success;
  
  (* Check restored memory *)
  match Memory.get_history "persist_test" 10 with
  | Some messages ->
      Printf.printf "Restored %d messages\n" (List.length messages)
  | None ->
      Printf.printf "No memory found\n"