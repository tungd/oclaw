(** File-backed global and chat-specific memory. *)

type t

val create : data_dir:string -> runtime_dir:string -> t
val read_global_memory : t -> string option
val read_chat_memory : t -> int -> string option
val write_global_memory : t -> string -> (unit, string) result
val write_chat_memory : t -> int -> string -> (unit, string) result
val build_memory_context : t -> int -> string
val global_memory_path : t -> string
val chat_memory_path : t -> int -> string
