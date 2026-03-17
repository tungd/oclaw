(** LLM provider for structured rayclaw-style messages on top of OpenAI-compatible APIs. *)

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
  temperature : float;
  max_tokens : int;
  timeout : int;
}

val qwen35_plus_model : llm_model
val create_dashscope_provider : ?temperature:float -> ?max_tokens:int -> unit -> provider_config

val build_request_json :
  provider_config ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  stream:bool ->
  Yojson.Safe.t

val parse_stream_chunks :
  ?on_text_delta:(string -> unit) ->
  string list ->
  (Llm_types.messages_response, string) result

val send_message :
  provider_config ->
  ?on_text_delta:(string -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result
