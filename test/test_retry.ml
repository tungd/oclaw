(** Test suite for LLM API retry logic *)

module Retry = Llm_provider.Retry

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let retry_error ?http_status message =
  Retry.{ message; http_status }

(* ============================================================================
   Error Classification Tests
   ============================================================================ *)

let test_is_retryable_error_rate_limit () =
  (* 429 should always be retryable *)
  expect (Retry.is_retryable_error ~http_status:(Some 429) ~error_body:"") "429 should be retryable";
  
  (* Rate limit in error body *)
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"rate limit exceeded") "rate limit should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"Rate-Limited") "Rate-Limited should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"too many requests") "too many requests should be retryable";
  print_endline "  ✓ is_retryable_error_rate_limit"

let test_is_retryable_error_server_error () =
  (* 5xx errors should be retryable *)
  expect (Retry.is_retryable_error ~http_status:(Some 500) ~error_body:"") "500 should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 502) ~error_body:"") "502 should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 503) ~error_body:"") "503 should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 504) ~error_body:"") "504 should be retryable";
  
  (* Server error in body *)
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"503 Service Unavailable") "503 in body should be retryable";
  print_endline "  ✓ is_retryable_error_server_error"

let test_is_retryable_error_network () =
  (* No HTTP status (network error) should be retryable *)
  expect (Retry.is_retryable_error ~http_status:None ~error_body:"") "network error should be retryable";
  
  (* Network errors in body *)
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"connection reset") "connection reset should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"connection refused") "connection refused should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"network error") "network error should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"timeout") "timeout should be retryable";
  expect (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"timed out") "timed out should be retryable";
  print_endline "  ✓ is_retryable_error_network"

let test_is_retryable_error_not_retryable () =
  (* 401 unauthorized should NOT be retryable *)
  expect (not (Retry.is_retryable_error ~http_status:(Some 401) ~error_body:"")) "401 should NOT be retryable";
  
  (* 400 bad request without retryable patterns *)
  expect (not (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"invalid request")) "invalid request should NOT be retryable";
  expect (not (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"bad syntax")) "bad syntax should NOT be retryable";
  
  (* Context length errors should NOT be retryable *)
  expect (not (Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"context length exceeded")) "context length should NOT be retryable";
  print_endline "  ✓ is_retryable_error_not_retryable"

(* ============================================================================
   Delay Calculation Tests
   ============================================================================ *)

let test_calculate_delay () =
  let config = Retry.default_config in
  
  (* First attempt: base delay *)
  expect (Retry.calculate_delay ~config ~attempt:1 = 1000) "attempt 1 should be 1000ms";
  
  (* Second attempt: 2x base delay *)
  expect (Retry.calculate_delay ~config ~attempt:2 = 2000) "attempt 2 should be 2000ms";
  
  (* Third attempt: 4x base delay *)
  expect (Retry.calculate_delay ~config ~attempt:3 = 4000) "attempt 3 should be 4000ms";
  
  (* Should cap at max_delay_ms *)
  let config_with_small_max = { config with max_delay_ms = 3000 } in
  expect (Retry.calculate_delay ~config:config_with_small_max ~attempt:3 = 3000) "should cap at max_delay_ms";
  print_endline "  ✓ calculate_delay"

(* ============================================================================
   Retry Logic Tests
   ============================================================================ *)

let test_with_retry_success () =
  let config = { Retry.default_config with max_retries = 3 } in
  
  (* Function that succeeds immediately *)
  let f () = Ok "success" in
  match Retry.with_retry ~config f with
  | Retry.Success result -> 
      expect (result = "success") "should return success";
      print_endline "  ✓ with_retry_success"
  | Retry.Failed _ -> fail "Expected success but got failure"

let test_with_retry_eventual_success () =
  let config = { Retry.default_config with max_retries = 3; base_delay_ms = 10 } in
  let attempts = ref 0 in
  
  (* Function that fails twice, then succeeds *)
  let f () =
    incr attempts;
    if !attempts < 3 then
      Error (retry_error "request timeout")  (* Retryable error *)
    else
      Ok "success"
  in
  
  match Retry.with_retry ~config f with
  | Retry.Success result ->
      expect (result = "success") "should eventually succeed";
      expect (!attempts = 3) "should take 3 attempts";
      print_endline "  ✓ with_retry_eventual_success"
  | Retry.Failed _ -> fail "Expected success but got failure"

let test_with_retry_max_retries () =
  let config = { Retry.default_config with max_retries = 2; base_delay_ms = 10 } in
  let attempts = ref 0 in
  
  (* Function that always fails with retryable error *)
  let f () =
    incr attempts;
    Error (retry_error "connection timeout")  (* Retryable error *)
  in
  
  match Retry.with_retry ~config f with
  | Retry.Success _ -> fail "Expected failure but got success"
  | Retry.Failed (error, count) ->
      expect (error.message = "connection timeout") "should have correct error message";
      expect (count = 2) "should report 2 retries";
      expect (!attempts = 2) "should attempt 2 times";
  print_endline "  ✓ with_retry_max_retries"

let test_with_retry_non_retryable () =
  let config = { Retry.default_config with max_retries = 3 } in
  let attempts = ref 0 in
  
  (* Function that fails with non-retryable error *)
  let f () =
    incr attempts;
    Error (retry_error ~http_status:401 "invalid API key")  (* Not a retryable error *)
  in
  
  match Retry.with_retry ~config f with
  | Retry.Success _ -> fail "Expected failure but got success"
  | Retry.Failed (error, count) ->
      expect (error.message = "invalid API key") "should have correct error message";
      expect (error.http_status = Some 401) "should preserve HTTP status";
      expect (count = 1) "should fail immediately without retries";
      expect (!attempts = 1) "should only attempt once";
  print_endline "  ✓ with_retry_non_retryable"

let test_with_retry_uses_structured_http_status () =
  let config = { Retry.default_config with max_retries = 2; base_delay_ms = 10 } in
  let attempts = ref 0 in

  let f () =
    incr attempts;
    Error (retry_error ~http_status:429 "rate limit")
  in

  match Retry.with_retry ~config f with
  | Retry.Success _ -> fail "Expected failure but got success"
  | Retry.Failed (error, count) ->
      expect (error.http_status = Some 429) "should preserve HTTP status";
      expect (count = 2) "should retry based on structured HTTP status";
      expect (!attempts = 2) "should attempt 2 times";
  print_endline "  ✓ with_retry_uses_structured_http_status"

(* ============================================================================
   Main
   ============================================================================ *)

let () =
  print_endline "Running retry tests...";
  test_is_retryable_error_rate_limit ();
  test_is_retryable_error_server_error ();
  test_is_retryable_error_network ();
  test_is_retryable_error_not_retryable ();
  test_calculate_delay ();
  test_with_retry_success ();
  test_with_retry_eventual_success ();
  test_with_retry_max_retries ();
  test_with_retry_non_retryable ();
  test_with_retry_uses_structured_http_status ();
  print_endline "[PASS] all retry tests ✓"
