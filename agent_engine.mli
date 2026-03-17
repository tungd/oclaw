(** Single agent loop over structured messages, tools, and persistent session state. *)

val build_system_prompt : Runtime.app_state -> chat_id:int -> string

val process :
  Runtime.app_state ->
  chat_id:int ->
  string ->
  (string, string) result
