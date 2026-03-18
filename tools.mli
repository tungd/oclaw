(** Registry-driven CLI tool layer inspired by rayclaw. *)

type sandbox_config = {
  workspace_root : string;
  restrict_to_workspace : bool;
  allow_read_paths : string list;
  allow_write_paths : string list;
  exec_timeout_seconds : int;
  exec_enable_deny_patterns : bool;
  exec_custom_deny_patterns : string list;
  exec_custom_allow_patterns : string list;
}

type web_config = {
  request_timeout_seconds : int;
  fetch_max_bytes : int;
  search_max_results : int;
}

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
}

type t

val default_sandbox_config : sandbox_config
val default_web_config : web_config
val success : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result
val failure : ?status_code:int -> ?duration_ms:int -> ?error_type:string -> string -> tool_result

val create_default_registry :
  ?sandbox_config:sandbox_config ->
  ?web_config:web_config ->
  data_dir:string ->
  skills_dir:string ->
  db:Db.t ->
  unit ->
  t

val definitions : t -> Llm_types.tool_definition list
val execute : t -> chat_id:int -> string -> Yojson.Safe.t -> tool_result

val init_default_tools :
  ?sandbox_config:sandbox_config ->
  ?web_config:web_config ->
  ?data_dir:string ->
  ?skills_dir:string ->
  ?db:Db.t ->
  unit ->
  unit

val get_all_tools : unit -> (string * string) list
val execute_tool : ?chat_id:int -> string -> Yojson.Safe.t -> string
val tools_to_json : unit -> Yojson.Safe.t
