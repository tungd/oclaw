(** Stub memory module - file-backed memory removed. *)

type t

val create : data_dir:string -> runtime_dir:string -> unit -> t
val build_memory_context : t -> int -> string
