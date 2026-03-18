(** Background scheduler for chat-scoped scheduled tasks. *)

type handle

val start : Runtime.app_state -> handle option
val stop : handle -> unit
