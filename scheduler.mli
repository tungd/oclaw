(** Background scheduler for chat-scoped scheduled tasks. *)

type handle

val start : Oclaw_config.Config.config -> handle option
val stop : handle -> unit
