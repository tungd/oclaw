(** Primitive filesystem and shell tools. *)

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

val default_sandbox_config : sandbox_config
val set_sandbox_config : sandbox_config -> unit

type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Safe.t) list;
  execute : Yojson.Safe.t -> string;
}

val file_read_tool : tool_definition
val shell_tool : tool_definition
val register_tool : tool_definition -> unit
val get_tool : string -> tool_definition option
val get_all_tools : unit -> (string * string) list
val execute_tool : string -> Yojson.Safe.t -> string
val init_default_tools : ?sandbox_config:sandbox_config -> unit -> unit
val tools_to_json : unit -> Yojson.Safe.t
val parse_tool_calls : Yojson.Safe.t -> Yojson.Safe.t list option
val extract_tool_arguments : Yojson.Safe.t -> string option
