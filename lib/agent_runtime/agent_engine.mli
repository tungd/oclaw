(** Single agent loop over structured messages, tools, and persistent session state. *)

val build_system_prompt : Runtime.app_state -> chat_id:int -> string

val process :
  emit:(Acp.Message.t -> unit) ->
  Runtime.app_state ->
  chat_id:int ->
  ?persistent:bool ->
  string ->
  (unit, string) result

val resolve_permission :
  Runtime.app_state ->
  chat_id:int ->
  Acp.Message.permission_outcome ->
  (unit, string) result
