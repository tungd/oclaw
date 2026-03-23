val process :
  emit:(Acp.Message.t -> unit) ->
  App.t ->
  chat_id:int ->
  ?persistent:bool ->
  string ->
  (unit, string) result

val resolve_permission :
  App.t ->
  chat_id:int ->
  Acp.Message.permission_outcome ->
  (unit, string) result

val history : App.t -> chat_id:int -> Llm_types.message list
val latest_node_id : App.t -> chat_id:int -> int option

val approve_read : App.t -> string -> (string, string) result
val approve_write : App.t -> string -> (string, string) result
val approve_exec : App.t -> string -> (string, string) result
val approve_install : App.t -> string -> (string, string) result
val list_permissions : ?scope:Tools.approval_scope -> App.t -> string

val trust_project : ?path:string -> App.t -> (string, string) result
val available_skills : App.t -> string list
val list_skills : ?include_untrusted:bool -> App.t -> string
val activate_skill : App.t -> chat_id:int -> string -> (string, string) result
