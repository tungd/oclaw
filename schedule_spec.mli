(** Parsing and computing supported schedule specifications. *)

val parse_once : string -> (float, string) result
val validate_cron : string -> (unit, string) result
val next_cron_after : string -> after:float -> (float, string) result
val format_timestamp_local : float -> string
