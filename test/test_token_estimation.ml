(** Test token estimation functionality *)

open Lwt.Infix

let test_token_counting () =
  let%lwt () = Lwt_log.info "Testing token counting..." in
  
  (* Test cl100k_base encoding *)
  let%lwt encoding = Tiktoken.get_encoding "cl100k_base" in
  
  (* Test simple string *)
  let text = "Hello, world!" in
  let tokens = Tiktoken.encode encoding text in
  let count = List.length tokens in
  let%lwt () = Lwt_log.info "Text: %s" text in
  let%lwt () = Lwt_log.info "Token count: %d" count in
  let%lwt () = Lwt_log.info "Tokens: %s" (String.concat ", " (List.map string_of_int tokens)) in
  
  (* Test count_tokens function *)
  let count2 = Tiktoken.count_tokens encoding text in
  let%lwt () = Lwt_log.info "count_tokens result: %d" count2 in
  
  (* Test longer text *)
  let long_text = "The quick brown fox jumps over the lazy dog. " ^
                  "Pack my box with five dozen liquor jugs. " ^
                  "How vexingly quick daft zebras jump!" in
  let long_count = Tiktoken.count_tokens encoding long_text in
  let%lwt () = Lwt_log.info "Long text token count: %d" long_count in
  
  (* Test message counting *)
  let messages = [
    ("system", "You are a helpful assistant.");
    ("user", "Hello, how are you?");
    ("assistant", "I'm doing well, thank you!");
  ] in
  let msg_count = Tiktoken.estimate_messages_tokens encoding messages in
  let%lwt () = Lwt_log.info "Message tokens (3 messages): %d" msg_count in
  
  (* Test Token_estimator module *)
  let%lwt () = Lwt_log.info "\nTesting Token_estimator module..." in
  let estimator = Llm_provider.Token_estimator.for_model "gpt-4" in
  let usage = Llm_provider.Token_estimator.calculate_usage 
    ~prompt_tokens:1000
    ~system_tokens:50
    ~tool_tokens:200
    estimator 
  in
  let%lwt () = Lwt_log.info "Usage: %s" (Llm_provider.Token_estimator.format_usage usage) in
  let%lwt () = Lwt_log.info "Short format: %s" (Llm_provider.Token_estimator.format_short usage) in
  
  (* Check context status *)
  let status = Llm_provider.Token_estimator.check_context estimator usage in
  let status_str = match status with
    | `Ok -> "OK"
    | `Warning -> "Warning"
    | `Exceeded -> "Exceeded"
  in
  let%lwt () = Lwt_log.info "Context status: %s" status_str in
  
  (* Test with high usage *)
  let high_usage = Llm_provider.Token_estimator.calculate_usage 
    ~prompt_tokens:7000
    ~system_tokens:50
    ~tool_tokens:200
    estimator 
  in
  let%lwt () = Lwt_log.info "High usage: %s" (Llm_provider.Token_estimator.format_usage high_usage) in
  let should_warn = Llm_provider.Token_estimator.should_warn estimator high_usage in
  let%lwt () = Lwt_log.info "Should warn: %b" should_warn in
  let warning_msg = Llm_provider.Token_estimator.warning_message high_usage in
  Option.iter (fun msg -> Lwt_log.info "Warning: %s" msg |> Lwt.ignore_result) warning_msg;
  
  Lwt.return ()

let test_model_encodings () =
  let%lwt () = Lwt_log.info "\nTesting model-to-encoding mapping..." in
  
  let models = [
    "gpt-4";
    "gpt-4-0613";
    "gpt-3.5-turbo";
    "gpt-4o";
    "gpt-4o-mini";
    "qwen-plus";
  ] in
  
  List.iter (fun model ->
    let encoding = Tiktoken.encoding_for_model model in
    let limit = Tiktoken.get_context_limit model in
    Lwt_log.info "Model: %s -> Encoding: %s, Limit: %d" model encoding.name limit
    |> Lwt.ignore_result
  ) models;
  
  Lwt.return ()

let () =
  let () = Logs.set_level (Some Logs.Info) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  
  let test =
    test_token_counting () >>= fun () ->
    test_model_encodings ()
  in
  
  Lwt_main.run test
