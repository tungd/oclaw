(** TUI frontend for OClaw using Mosaic.

    {1:architecture Architecture}

    The TUI runs on the main domain using Mosaic's Elm Architecture loop. 
    The agent backend runs on a separate domain, communicating with the 
    TUI via Domainslib.Chan.
*)

(** Start the TUI application *)
val run : state:Agent_runtime.App.t -> chat_id:int -> persistent:bool -> unit
