(** LLM provider for structured tool-use messages on top of OpenAI-compatible APIs. *)

type llm_model = {
  id : string;
  name : string;
  reasoning : bool;
  input_types : string list;
  cost : float * float * float * float;
  context_window : int;
  max_tokens : int;
}

type provider_config = {
  api_base : string;
  api_key : string;
  model : llm_model;
  max_tokens : int;
  timeout : int;
}

val qwen35_plus_model : llm_model
val create_dashscope_provider : ?max_tokens:int -> unit -> provider_config

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
