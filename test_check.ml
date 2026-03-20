module Retry = Llm_provider.Retry

let () =
  let result = Retry.is_retryable_error ~http_status:(Some 400) ~error_body:"invalid API key" in
  Printf.printf "is_retryable_error('invalid API key') = %b\n" result
