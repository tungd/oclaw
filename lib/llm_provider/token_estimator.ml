(** Token estimation for LLM context management.
    
    This module provides token counting and context limit management
    using the tiktoken library.
*)

(** Token estimator configuration *)
type t = {
  encoding : Tiktoken.encoding;
  context_limit : int;
  warning_threshold : float;
}

(** Create a token estimator for a model. *)
let for_model ?(warning_threshold = 0.8) model =
  let encoding = Tiktoken.encoding_for_model model in
  let context_limit = Tiktoken.get_context_limit model in
  { encoding; context_limit; warning_threshold }

(** Estimate tokens for a string. *)
let count_tokens estimator text =
  Tiktoken.count_tokens estimator.encoding text

(** Estimate tokens for a list of messages. *)
let count_messages estimator messages =
  let message_pairs = List.map (fun (role, content) ->
    (role, content)
  ) messages in
  Tiktoken.estimate_messages_tokens estimator.encoding message_pairs

(** Estimate tokens for system prompt. *)
let count_system_prompt estimator prompt =
  Tiktoken.count_tokens estimator.encoding prompt

(** Estimate tokens for tool definitions (as JSON). *)
let count_tools estimator tools =
  Tiktoken.count_tokens estimator.encoding tools

(** Context usage information. *)
type usage = {
  prompt_tokens : int;
  system_tokens : int;
  tool_tokens : int;
  response_tokens : int;
  total_tokens : int;
  context_limit : int;
  remaining : int;
  percentage : float;
}

(** Calculate context usage. *)
let calculate_usage ?prompt_tokens ?system_tokens ?tool_tokens ?response_tokens (estimator : t) =
  let pt = match prompt_tokens with 
    | Some n -> n 
    | None -> 0 
  in
  let st = match system_tokens with 
    | Some n -> n 
    | None -> 0 
  in
  let tt = match tool_tokens with 
    | Some n -> n 
    | None -> 0 
  in
  let rt = match response_tokens with 
    | Some n -> n 
    | None -> 0 
  in
  let total = pt + st + tt + rt in
  let remaining = max 0 (estimator.context_limit - total) in
  let percentage = 
    Float.min 100.0 (float_of_int total /. float_of_int estimator.context_limit *. 100.0)
  in
  { 
    prompt_tokens = pt;
    system_tokens = st;
    tool_tokens = tt;
    response_tokens = rt;
    total_tokens = total;
    context_limit = estimator.context_limit;
    remaining;
    percentage;
  }

(** Check context status. *)
let check_context (est : t) (usage : usage) =
  if usage.total_tokens >= est.context_limit then `Exceeded
  else if float_of_int usage.total_tokens >= float_of_int est.context_limit *. est.warning_threshold then `Warning
  else `Ok

(** Format usage as a human-readable string. *)
let format_usage usage =
  Printf.sprintf "%d/%d tokens (%.1f%%) - %d remaining"
    usage.total_tokens usage.context_limit usage.percentage usage.remaining

(** Format a short usage string for TUI display. *)
let format_short usage =
  Printf.sprintf "[%d/%d]" usage.total_tokens usage.context_limit

(** Check if we should warn about context usage. *)
let should_warn est usage =
  match check_context est usage with
  | `Warning | `Exceeded -> true
  | `Ok -> false

(** Get warning message for context usage. *)
let warning_message usage =
  if usage.percentage >= 100.0 then
    Some "⚠️  Context limit exceeded! Consider starting a new conversation."
  else if usage.percentage >= 90.0 then
    Some (Printf.sprintf "⚠️  Token usage at %.0f%%. Consider starting a new conversation." usage.percentage)
  else if usage.percentage >= 80.0 then
    Some (Printf.sprintf "⚠️  Token usage at %.0f%%." usage.percentage)
  else
    None