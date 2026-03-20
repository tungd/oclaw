(** Compatibility wrapper over the persistent runtime facade. *)

type t

type llm_call =
  App.llm_call

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
val get_active_branch : t -> Llm_types.message list
val get_current_node_id : t -> int option
