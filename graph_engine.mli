(** Graph-based execution engine for OClaw.
    
    Inspired by Koog's agent architecture, this module provides:
    - Node-based execution graph
    - Edge-based transitions with conditions
    - Shared memory across nodes
    - Parallel node execution support
*)

(** Unique identifier for nodes and edges *)
type node_id = string
type edge_id = string

(** Execution context passed between nodes - defined first for mutual recursion *)
type memory_context = {
  working_memory : (string * Yojson.Safe.t) list;  (* Short-term KV store *)
  conversation_history : Llm_types.message list;
  execution_trace : execution_step list;
  variables : (string * Yojson.Safe.t) list;  (* Node-to-node data passing *)
}

(** Single step in execution trace *)
and execution_step = {
  step_id : int;
  node_id : node_id;
  timestamp : float;
  input : Yojson.Safe.t option;
  output : Yojson.Safe.t option;
  error : string option;
  duration_ms : int;
}

(** Memory query types for retrieval *)
type memory_query = {
  query_type : memory_query_type;
  keys : string list;
  limit : int option;
  filters : (string * string) list;
}

(** Types of memory queries *)
and memory_query_type =
  | ExactMatch  (* Get by exact key *)
  | SemanticSearch  (* Similarity search *)
  | TemporalRange  (* Time-based range *)
  | TagBased  (* Filter by tags *)

(** Memory write operations *)
and memory_write = {
  write_type : memory_write_type;
  key : string;
  value : Yojson.Safe.t;
  metadata : memory_metadata;
}

(** Types of memory writes *)
and memory_write_type =
  | Set  (* Set/overwrite value *)
  | Append  (* Append to existing value *)
  | Merge  (* Merge with existing JSON *)
  | Delete  (* Remove key *)

(** Memory metadata for organization and retrieval *)
and memory_metadata = {
  created_at : float;
  updated_at : float;
  importance : float;  (* 0.0 - 1.0 *)
  tags : string list;
  source : string;  (* Which node created this *)
  ttl : float option;  (* Time-to-live in seconds *)
  summary : string option;  (* Pre-computed summary *)
}

(** Node types defining execution behavior *)
type node_type =
  | LLMNode of Llm_provider.provider_config  (* LLM call with tool access *)
  | ToolNode of string  (* Execute a specific tool by name *)
  | MemoryReadNode of memory_query  (* Read from shared memory *)
  | MemoryWriteNode of memory_write  (* Write to shared memory *)
  | MemorySummarizeNode  (* Summarize and compress memory *)
  | ConditionNode of (unit -> bool)  (* Conditional branching *)
  | ParallelNode of node_id list  (* Execute multiple nodes in parallel *)
  | EndNode  (* Terminal node *)

(** Edge defining transitions between nodes *)
type edge = {
  edge_id : edge_id;
  from_node : node_id;
  to_node : node_id;
  condition : (memory_context -> bool) option;  (* Optional guard condition *)
  priority : int;  (* For multiple outgoing edges *)
}

(** Node definition in the graph *)
type node = {
  node_id : node_id;
  node_type : node_type;
  description : string;
  input_schema : Yojson.Safe.t option;
  output_schema : Yojson.Safe.t option;
  retry_count : int;
  timeout_seconds : int option;
}

(** The execution graph *)
type execution_graph = {
  graph_id : string;
  nodes : (node_id * node) list;
  edges : (edge_id * edge) list;
  start_node : node_id;
  end_nodes : node_id list;
  metadata : (string * string) list;
}

(** Execution result *)
type execution_result =
  | Success of Yojson.Safe.t * memory_context
  | Failure of string * memory_context
  | Partial of Yojson.Safe.t * memory_context * node_id  (* Stopped at intermediate node *)

(** Graph builder for fluent API *)
type graph_builder = {
  mutable nodes : (node_id * node) list;
  mutable edges : (edge_id * edge) list;
  mutable start_node : node_id option;
  mutable end_nodes : node_id list;
  mutable next_id : int;
}

(** Memory store interface for different backends *)
module type MEMORY_STORE = sig
  type t
  
  val create : unit -> t
  val get : t -> string -> Yojson.Safe.t option
  val set : t -> string -> Yojson.Safe.t -> memory_metadata -> unit
  val delete : t -> string -> unit
  val search : t -> memory_query -> (string * Yojson.Safe.t * memory_metadata) list
  val summarize : t -> string list -> Yojson.Safe.t
  val export : t -> Yojson.Safe.t
  val import : t -> Yojson.Safe.t -> unit
  val clear : t -> unit
end

(** Execution engine signature *)
module type EXECUTION_ENGINE = sig
  type t
  
  val create : execution_graph -> unit -> t
  val execute : t -> node_id -> Yojson.Safe.t option -> memory_context -> tools:Tools.t -> chat_id:int -> execution_result * t
  val execute_from_start : t -> Yojson.Safe.t option -> memory_context -> tools:Tools.t -> chat_id:int -> execution_result * t
  val step : t -> node_id -> Yojson.Safe.t option -> memory_context -> tools:Tools.t -> chat_id:int -> execution_result * t
  val get_current_node : t -> node_id option
  val get_execution_trace : t -> execution_step list
end
