(** LLM API retry with exponential backoff.
    
    Uses regex patterns to detect retryable errors in API responses.
*)

module Log = (val Logs.src_log (Logs.Src.create "llm_retry") : Logs.LOG)

type retry_config = {
  max_retries : int;
  base_delay_ms : int;
  max_delay_ms : int;
  exponential_base : float;
}

type 'a retry_result =
  | Success of 'a
  | Failed of string * int

let default_config = {
  max_retries = 3;
  base_delay_ms = 1000;  (* 1 second *)
  max_delay_ms = 30000;  (* 30 seconds *)
  exponential_base = 2.0;
}

(** Check if an error is retryable based on HTTP status and error body *)
let is_retryable_error ~http_status ~error_body =
  (* Always retry on 429 (rate limit) and 5xx (server errors) *)
  match http_status with
  | Some 429 -> true
  | Some status when status >= 500 && status < 600 -> true
  | None ->
      (* No HTTP status - likely a network-level error *)
      (* Retry if error body is empty (connection failed) or contains network patterns *)
      if String.trim error_body = "" then
        true  (* Empty error body suggests network failure *)
      else
        let lower = String.lowercase_ascii error_body in
        let network_patterns = [
          "timeout";
          "timed out";
          "connection.?reset";
          "connection.?refused";
          "network";
          "socket hang up";
          "upstream.*closed";
          "eof";
          "unexpected end";
        ] in
        List.exists (fun pattern ->
          try
            let re = Str.regexp pattern in
            ignore (Str.search_forward re lower 0);
            true
          with Not_found -> false
        ) network_patterns
  | Some _ ->
      (* Client error - only retry if body contains retryable patterns *)
      let lower = String.lowercase_ascii error_body in
      let patterns = [
        "rate.?limit";
        "rate.?limited";
        "too many requests";
        "overload";
        "service.?unavailable";
        "bad gateway";
        "gateway.?timeout";
        "connection.?reset";
        "connection.?refused";
        "network";
        "timeout";
        "timed out";
        "5[0-9][0-9]";  (* Match 500, 502, 503, etc. in error body *)
      ] in
      List.exists (fun pattern ->
        try
          let re = Str.regexp pattern in
          ignore (Str.search_forward re lower 0);
          true
        with Not_found -> false
      ) patterns

(** Calculate delay for a given attempt number using exponential backoff *)
let calculate_delay ~config ~attempt =
  let delay_ms = float config.base_delay_ms *. 
                 (config.exponential_base ** float (attempt - 1)) in
  min (int_of_float delay_ms) config.max_delay_ms

(** Execute a function with retry logic.
    The function should return a result type ('a, string) result.
    On Error, checks if the error is retryable.
*)
let with_retry ~config f =
  let rec loop attempt last_error =
    if attempt > config.max_retries then
      Failed (last_error, attempt - 1)
    else
      match f () with
      | Ok result -> Success result
      | Error error_msg ->
          (* Try to extract HTTP status from error message *)
          (* Look for patterns like "HTTP 429" or "status: 500" in error *)
          let http_status =
            try
              let re = Str.regexp "\\b\\(4[0-9][0-9]\\|5[0-9][0-9]\\)\\b" in
              let _ = Str.search_forward re error_msg 0 in
              let matched = Str.matched_string error_msg in
              Some (int_of_string matched)
            with Not_found | Failure _ -> None
          in
          
          (* Check if retryable: either has retryable HTTP status OR contains retryable patterns in body *)
          let is_retryable =
            match http_status with
            | Some 429 -> true
            | Some status when status >= 500 && status < 600 -> true
            | None ->
                (* No status code - check error body for retryable patterns *)
                is_retryable_error ~http_status:None ~error_body:error_msg
            | Some _ ->
                (* Client error - only retry if body contains retryable patterns *)
                is_retryable_error ~http_status:(Some 400) ~error_body:error_msg
          in
          
          if is_retryable then
            begin
              Log.warn (fun m -> m "API call failed (attempt %d/%d): %s" 
                          attempt config.max_retries error_msg);
              let delay_ms = calculate_delay ~config ~attempt in
              Log.info (fun m -> m "Waiting %dms before retry..." delay_ms);
              Unix.sleepf (float delay_ms /. 1000.0);
              loop (attempt + 1) error_msg
            end
          else
            begin
              Log.err (fun m -> m "Non-retryable error (attempt %d): %s" attempt error_msg);
              Failed (error_msg, attempt)
            end
  in
  loop 1 "Unknown error"
