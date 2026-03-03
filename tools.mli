(** Tools module for OClaw - web search, file operations, etc. *)

(** Tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Basic.t) list;
  execute : string -> string -> string
}

(** Web search tool *)
val web_search_tool : tool_definition

(** File read tool *)
val file_read_tool : tool_definition

(** Shell command tool *)
val shell_tool : tool_definition

(** Register a tool *)
val register_tool : tool_definition -> unit

(** Get tool by name *)
val get_tool : string -> tool_definition option

(** Execute a tool call *)
val execute_tool : string -> Yojson.Basic.t -> string

(** Initialize default tools *)
val init_default_tools : unit -> unit

(** Convert tools to JSON for LLM API *)
val tools_to_json : unit -> Yojson.Basic.t

(** Parse tool calls from LLM response *)
val parse_tool_calls : Yojson.Basic.t -> Yojson.Basic.t list option

(** Extract tool call arguments *)
val extract_tool_arguments : Yojson.Basic.t -> string option