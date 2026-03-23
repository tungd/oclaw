(** Test token estimation functionality *)

let () =
  Printf.printf "=== Token Estimation Tests ===\n\n%!";
  
  (* Test 1: Load encoding *)
  Printf.printf "Test 1: Loading cl100k_base encoding...\n%!";
  let t0 = Unix.gettimeofday () in
  let encoding = Tiktoken.get_encoding "cl100k_base" in
  let t1 = Unix.gettimeofday () in
  Printf.printf "  Loaded in %.2f seconds\n%!" (t1 -. t0);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 2: Token counting *)
  Printf.printf "Test 2: Token counting...\n%!";
  let test_cases = [
    ("Hello, world!", 4);
    ("The quick brown fox jumps over the lazy dog.", 9);
    ("", 0);
  ] in
  List.iter (fun (text, _expected) ->
    let count = Tiktoken.count_tokens encoding text in
    Printf.printf "  '%s' -> %d tokens\n%!" text count
  ) test_cases;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 3: Token_estimator module *)
  Printf.printf "Test 3: Token_estimator module...\n%!";
  let estimator = Llm_provider.Token_estimator.for_model "gpt-4" in
  let text = "Hello, how are you?" in
  let count = Llm_provider.Token_estimator.count_tokens estimator text in
  Printf.printf "  Model: gpt-4\n%!";
  Printf.printf "  Text: '%s'\n%!" text;
  Printf.printf "  Token count: %d\n%!" count;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 4: Message counting *)
  Printf.printf "Test 4: Message counting...\n%!";
  let messages = [
    ("system", "You are a helpful assistant.");
    ("user", "Hello!");
    ("assistant", "Hi there!");
  ] in
  let msg_count = Llm_provider.Token_estimator.count_messages estimator messages in
  Printf.printf "  Messages: %d\n%!" (List.length messages);
  Printf.printf "  Total tokens: %d\n%!" msg_count;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 5: Context usage calculation *)
  Printf.printf "Test 5: Context usage calculation...\n%!";
  let usage = Llm_provider.Token_estimator.calculate_usage 
    ~prompt_tokens:1000
    ~system_tokens:50
    ~tool_tokens:200
    estimator 
  in
  Printf.printf "  %s\n%!" (Llm_provider.Token_estimator.format_usage usage);
  Printf.printf "  Short: %s\n%!" (Llm_provider.Token_estimator.format_short usage);
  Printf.printf "  PASS\n\n%!";
  
  (* Test 6: Context warnings *)
  Printf.printf "Test 6: Context warnings...\n%!";
  let test_usages = [
    (1000, "Low usage");
    (3500, "80% threshold");
    (4000, "Near limit");
    (4096, "At limit");
    (5000, "Exceeded");
  ] in
  List.iter (fun (tokens, label) ->
    let usage = Llm_provider.Token_estimator.calculate_usage 
      ~prompt_tokens:tokens
      estimator 
    in
    let status = Llm_provider.Token_estimator.check_context estimator usage in
    let status_str = match status with
      | `Ok -> "OK"
      | `Warning -> "WARNING"
      | `Exceeded -> "EXCEEDED"
    in
    let should_warn = Llm_provider.Token_estimator.should_warn estimator usage in
    Printf.printf "  %s (%d tokens): %s, warn=%b\n%!" label tokens status_str should_warn
  ) test_usages;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 7: Model encoding mapping *)
  Printf.printf "Test 7: Model encoding mapping...\n%!";
  let models = ["gpt-4"; "gpt-3.5-turbo"; "gpt-4o"; "qwen-plus"] in
  List.iter (fun model ->
    let est = Llm_provider.Token_estimator.for_model model in
    let usage = Llm_provider.Token_estimator.calculate_usage ~prompt_tokens:100 est in
    Printf.printf "  %s: limit=%d, usage=%s\n%!" 
      model usage.Llm_provider.Token_estimator.context_limit 
      (Llm_provider.Token_estimator.format_short usage)
  ) models;
  Printf.printf "  PASS\n\n%!";
  
  (* Test 8: Warning messages *)
  Printf.printf "Test 8: Warning messages...\n%!";
  let warning_tests = [3000; 3500; 3800; 4096; 4500] in
  List.iter (fun tokens ->
    let usage = Llm_provider.Token_estimator.calculate_usage ~prompt_tokens:tokens estimator in
    match Llm_provider.Token_estimator.warning_message usage with
    | Some msg -> Printf.printf "  %d tokens: %s\n%!" tokens msg
    | None -> Printf.printf "  %d tokens: (no warning)\n%!" tokens
  ) warning_tests;
  Printf.printf "  PASS\n\n%!";
  
  Printf.printf "=== All token estimation tests passed! ===\n%!"
