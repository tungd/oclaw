(** File-backed global and chat-specific memory with vector search. *)

type t

val create : data_dir:string -> runtime_dir:string -> unit -> t
val read_global_memory : t -> string option
val read_chat_memory : t -> int -> string option
val write_global_memory : t -> string -> (unit, string) result
val write_chat_memory : t -> int -> string -> (unit, string) result
val build_memory_context : t -> int -> string
val global_memory_path : t -> string
val chat_memory_path : t -> int -> string

(* Vector search integration - requires DB parameter *)
val search_similar_memories : Sqlite3.db -> chat_id:int -> query:string -> limit:int -> threshold:float -> (int * int * string * float) list
val auto_embed_memory : Sqlite3.db -> chat_id:int -> memory_id:int -> content:string -> (unit, string) result
val get_vector_stats : Sqlite3.db -> chat_id:int -> (int * float * float option * float option) option
