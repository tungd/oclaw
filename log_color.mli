(* Colored logging reporter interface *)

val setup_auto : ?level:Logs.level option -> ?format_time:bool -> unit -> unit
val setup : ?level:Logs.level option -> ?format_time:bool -> unit -> unit
val supports_color : unit -> bool
