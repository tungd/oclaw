(** Memory and conversation history system for OClaw *)

(** Conversation message type *)
type conversation_message = {
  role : string;
  content : string;
  timestamp : float;
  importance : float;
  estimated_tokens : int;
  metadata : (string * string) list
}

(** Conversation session type *)
type conversation_session = {
  session_id : string;
  created_at : float;
  messages : conversation_message list;
  current_context : string list
}

(** Memory cleanup policy *)
type cleanup_policy = {
  max_age_seconds : float;
  max_tokens : int;
  max_messages : int;
  importance_threshold : float
}

(** Default cleanup policy *)
val default_cleanup_policy : cleanup_policy

(** Memory statistics *)
type memory_stats = {
  total_sessions : int;
  total_messages : int;
  avg_messages_per_session : int;
  estimated_tokens : int
}

(** Create a new conversation session *)
val create_session : string -> conversation_session

(** Add a message to conversation history with cleanup policy *)
val add_message : conversation_session -> string -> string -> cleanup_policy -> conversation_session

(** Get conversation history *)
val get_history : string -> int -> conversation_message list option

(** Get current context *)
val get_context : string -> string list option

(** Clear conversation history *)
val clear_history : string -> unit

(** Build conversation context for LLM with token limit *)
val build_context : string -> string -> int -> Yojson.Basic.t option

(** Save memory to file *)
val save_memory : string -> bool

(** Load memory from file *)
val load_memory : string -> bool

(** Get session summary *)
val get_session_summary : string -> string option

(** List all active sessions *)
val list_sessions : unit -> (string * int) list

(** Get memory statistics *)
val get_stats : unit -> memory_stats