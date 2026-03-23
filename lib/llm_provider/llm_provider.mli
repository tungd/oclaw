(** OpenAI-compatible transport adapter over shared LLM message/tool types. *)

type provider_config = {
  api_base : string;
  api_key : string;
  model_name : string;
  max_tokens : int;
  timeout : int;
}

val build_request_json :
  provider_config ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  stream:bool ->
  Yojson.Safe.t

type stream_parser

val create_stream_parser : ?on_text_delta:(string -> unit) -> unit -> stream_parser
val feed_parser : stream_parser -> string -> (unit, string) result
val finalize_parser : stream_parser -> (Llm_types.messages_response, string) result

val send_message :
  provider_config ->
  ?on_text_delta:(string -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result

(** Retry module for automatic API retry on transient failures *)
module Retry : sig
  type retry_config = {
    max_retries : int;
    base_delay_ms : int;
    max_delay_ms : int;
    exponential_base : float;
  }

  type retry_error = {
    message : string;
    http_status : int option;
  }
  
  type 'a retry_result =
    | Success of 'a
    | Failed of retry_error * int
  
  val default_config : retry_config
  val is_retryable_error : http_status:int option -> error_body:string -> bool
  val calculate_delay : config:retry_config -> attempt:int -> int
  val with_retry : config:retry_config -> (unit -> ('a, retry_error) result) -> 'a retry_result
end

(** Token estimation for context management *)
module Token_estimator = Token_estimator
