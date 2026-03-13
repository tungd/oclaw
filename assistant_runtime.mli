(** Minimal in-process assistant runtime for the CLI. *)

type llm_call =
  Llm_provider.provider_config ->
  Llm_provider.message list ->
  tools:Yojson.Safe.t ->
  Llm_provider.llm_result

type t

val default_system_prompt : string
val default_llm_call : llm_call

val create :
  ?llm_call:llm_call ->
  ?system_prompt:string ->
  ?history_limit:int ->
  provider_config:Llm_provider.provider_config ->
  unit ->
  t

val query : t -> string -> (string, string) result
val history : t -> Llm_provider.message list
