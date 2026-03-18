(** Compatibility wrapper over the new persistent runtime. *)

type t

type llm_call =
  Runtime.llm_call

val create :
  ?llm_call:llm_call ->
  ?system_prompt:string ->
  ?chat_id:int ->
  ?data_dir:string ->
  provider_config:Llm_provider.provider_config ->
  unit ->
  t

val query : t -> string -> (string, string) result
val history : t -> Llm_types.message list
