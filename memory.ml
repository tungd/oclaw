(** Stub memory module - file-backed memory removed. *)

type t = unit

let create ~data_dir:_ ~runtime_dir:_ () =
  ()

let build_memory_context _t _chat_id =
  ""
