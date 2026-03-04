(** TUI client for OClaw *)

(** TUI client *)
module Tui : sig
  val run : server_address:string -> session_id:string -> unit
  (** Run the TUI client *)

  val send_message : server_address:string -> session_id:string -> content:string -> (string, string) result
  (** Send a single message to the server *)
end
