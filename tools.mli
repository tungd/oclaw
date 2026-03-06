(** Tools module for OClaw - filesystem, shell, and task operations. *)

(** Sandbox and safety configuration for tools. *)
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

(** Tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Safe.t) list;
  execute : Yojson.Safe.t -> string;
}

(** File read tool *)
val file_read_tool : tool_definition

(** Shell command tool *)
val shell_tool : tool_definition

(** Register a tool *)
val register_tool : tool_definition -> unit

(** Get tool by name *)
val get_tool : string -> tool_definition option

(** Get all tools as (name, description) list *)
val get_all_tools : unit -> (string * string) list

(** Execute a tool call *)
val execute_tool : string -> Yojson.Safe.t -> string

(** Initialize default tools *)
val init_default_tools : ?sandbox_config:sandbox_config -> unit -> unit

(** Convert tools to JSON for LLM API *)
val tools_to_json : unit -> Yojson.Safe.t

(** Parse tool calls from LLM response *)
val parse_tool_calls : Yojson.Safe.t -> Yojson.Safe.t list option

(** Extract tool call arguments *)
val extract_tool_arguments : Yojson.Safe.t -> string option
