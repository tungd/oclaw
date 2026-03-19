(** Tree-structured conversation storage with materialized paths. *)

module Json = Protocol_conv_json.Json

type node_id = int
type chat_id = int

type node_kind =
  | UserPrompt
  | LLMResponse  
  | ToolCall
  | ToolResult

type node_metadata = {
  tool_name : string option;
  tool_result_status : string option;
  fork_point : bool;
}
[@@deriving protocol ~driver:(module Json)]

type tree_node = private {
  id : node_id;
  path : string;
  chat_id : chat_id;
  kind : node_kind;
  content : Llm_types.message_content;
  model : string option;
  metadata : node_metadata;
  timestamp : float;
}

type conversation_info = {
  id : chat_id;
  title : string option;
  parent_chat_id : chat_id option;
  parent_node_id : node_id option;
  timestamp : float;
}

val node_metadata_to_yojson : node_metadata -> Yojson.Safe.t
val node_metadata_of_yojson : Yojson.Safe.t -> node_metadata

val conversation_info_to_yojson : conversation_info -> Yojson.Safe.t
val conversation_info_of_yojson : Yojson.Safe.t -> conversation_info

type t

(** Lifecycle *)
val create : data_dir:string -> runtime_dir:string -> t
val close : t -> unit

(** Conversation Management *)
val create_conversation : t -> ?title:string -> ?parent_chat_id:chat_id -> ?parent_node_id:node_id -> unit -> chat_id
val get_conversation : t -> chat_id -> conversation_info option
val get_all_conversations : t -> conversation_info list
val delete_conversation : t -> chat_id -> unit

(** Node Creation - returns new node ID *)
val add_user_prompt : t -> chat_id:int -> ?parent_id:node_id -> content:string -> unit -> node_id
(** Add a user prompt. If [parent_id] is provided, this prompt branches from that node.
    If not provided, this is a new root node for the conversation. *)

val add_llm_response : t -> parent_id:node_id -> ?model:string -> content:Llm_types.message_content -> unit -> node_id
val add_tool_call : t -> parent_id:node_id -> tool_name:string -> input:Yojson.Safe.t -> node_id
val add_tool_result : t -> parent_id:node_id -> tool_use_id:string -> content:string -> is_error:bool -> node_id

(** Tree Navigation *)
val get_node : t -> node_id -> tree_node option
val get_children : t -> node_id -> tree_node list
val get_parent : t -> node_id -> tree_node option
val get_path_to_root : t -> node_id -> tree_node list
(** Returns path from node to root, including the node itself *)

(** Conversation Queries *)
val get_roots : t -> chat_id:int -> node_id list
(** Get all root nodes (user prompts) for a chat *)

val get_branch : t -> node_id -> Llm_types.message list
(** Get linear message sequence from root to this node (for LLM context).
    Uses materialized path for efficient single-query retrieval. *)

val get_subtree : t -> node_id -> tree_node list
(** Get all descendants of a node (DFS order via path sorting). *)

val get_latest_node : t -> chat_id:int -> node_id option
(** Get the most recently created node in a conversation.
    Used to resume a conversation after restart.
    Implementation: SELECT id FROM transcripts WHERE chat_id = ? ORDER BY timestamp DESC LIMIT 1 *)

(** Forking *)
val fork_conversation : t -> chat_id:int -> node_id -> ?title:string -> unit -> chat_id
(** Creates a new conversation branching from the specified node.
    Performs a deep copy of all ancestor nodes to the new chat_id.
    Returns the new conversation ID. *)

(** Export *)
val export_json : t -> chat_id:int -> string
(** Export full conversation tree as JSON *)

val export_html : t -> chat_id:int -> out_path:string -> unit
(** Export as static HTML file with collapsible tree visualization.
    Uses tyxml for type-safe HTML generation. *)

(** Utility *)
val to_linear_messages : t -> chat_id:int -> Llm_types.message list
(** Convert tree to linear message list (takes first/main branch).
    For backward compatibility with existing code. *)
