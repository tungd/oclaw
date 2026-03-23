(** Layoutz-backed append-only interactive REPL frontend for OClaw. *)

(** Start the interactive frontend *)
val run : state:Agent_runtime.App.t -> chat_id:int -> persistent:bool -> unit
