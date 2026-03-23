(** Test tiktoken implementation *)

let () =
  Printf.printf "=== Tiktoken Tests ===\n\n%!";
  
  (* Test 1: Load vocabulary *)
  Printf.printf "Test 1: Loading cl100k_base encoding...\n%!";
  let t0 = Unix.gettimeofday () in
  let encoding = Tiktoken.get_encoding "cl100k_base" in
  let t1 = Unix.gettimeofday () in
  Printf.printf "  Loaded in %.2f seconds, vocab size: %d\n%!" (t1 -. t0) (Hashtbl.length encoding.Tiktoken.Encoding.vocab);
  assert (Hashtbl.length encoding.Tiktoken.Encoding.vocab = 100256);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 2: Basic encoding *)
  Printf.printf "Test 2: Basic encoding...\n%!";
  let test_cases = [
    ("Hello", 1);      (* Single token *)
    ("Hello, world!", 4);  (* "Hello", ",", " world", "!" *)
    ("Hello world", 2);     (* "Hello", " world" *)
  ] in
  List.iter (fun (text, expected_min) ->
    let tokens = Tiktoken.encode encoding text in
    let count = List.length tokens in
    Printf.printf "  '%s' -> %d tokens: %s\n%!" text count (String.concat " " (List.map string_of_int tokens));
    assert (count >= expected_min)
  ) test_cases;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 3: Decode *)
  Printf.printf "Test 3: Decode...\n%!";
  let text = "Hello, world!" in
  let tokens = Tiktoken.encode encoding text in
  let decoded = Tiktoken.decode encoding tokens in
  Printf.printf "  Original: '%s'\n%!" text;
  Printf.printf "  Tokens: %s\n%!" (String.concat " " (List.map string_of_int tokens));
  Printf.printf "  Decoded: '%s'\n%!" decoded;
  assert (String.equal text decoded);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 4: Count tokens *)
  Printf.printf "Test 4: Count tokens...\n%!";
  let text = "The quick brown fox jumps over the lazy dog." in
  let count = Tiktoken.count_tokens encoding text in
  Printf.printf "  '%s'\n%!" text;
  Printf.printf "  Token count: %d\n%!" count;
  assert (count > 0);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 5: Message estimation *)
  Printf.printf "Test 5: Message estimation...\n%!";
  let messages = [
    ("user", "Hello, how are you?");
    ("assistant", "I'm doing well, thank you for asking!");
  ] in
  let message_tokens = Tiktoken.estimate_messages_tokens encoding messages in
  Printf.printf "  Messages: %d pairs\n%!" (List.length messages);
  Printf.printf "  Estimated tokens: %d\n%!" message_tokens;
  assert (message_tokens > 0);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 6: Context info *)
  Printf.printf "Test 6: Context info...\n%!";
  let info = Tiktoken.context_info ~used:1000 ~limit:4096 in
  Printf.printf "  %s\n%!" (Tiktoken.format_context_info info);
  let check = Tiktoken.check_context ~used:3500 ~limit:4096 ~threshold:0.8 in
  Printf.printf "  Check at 3500/4096 (threshold 0.8): %s\n%!" 
    (match check with `Ok -> "OK" | `Warning -> "WARNING" | `Exceeded -> "EXCEEDED");
  assert (check = `Warning);
  Printf.printf "  PASS\n\n%!";
  
  Printf.printf "=== All tests passed! ===\n%!"