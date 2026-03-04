(** Session management for OClaw server *)

(** {1 Session types} *)

(** A message in a conversation session *)
module Message : sig
  type t = {
    role : string;  (** "user", "assistant", or "tool" *)
    content : string;  (** Message content *)
    timestamp : float;  (** Unix timestamp *)
  }

  val create : role:string -> content:string -> t
  (** Create a new message *)

  val to_json : t -> Yojson.Basic.t
  (** Convert message to JSON *)

  val of_json : Yojson.Basic.t -> t
  (** Convert JSON to message *)
end

(** A conversation session *)
module Session : sig
  type t

  val create : id:string -> directory:string -> t
  (** Create a new session *)

  val id : t -> string
  (** Get session ID *)

  val directory : t -> string
  (** Get session directory *)

  val add_message : t -> Message.t -> unit
  (** Add a message to the session *)

  val get_history : t -> limit:int -> Message.t list
  (** Get message history, limited by the given limit *)

  val touch : t -> unit
  (** Update the last active timestamp *)

  val created_at : t -> float
  (** Get creation timestamp *)

  val last_active : t -> float
  (** Get last active timestamp *)

  val message_count : t -> int
  (** Get total message count *)
end

(** {1 Session Manager} *)

(** Manages multiple sessions *)
module Manager : sig
  type t

  val create : unit -> t
  (** Create a new session manager *)

  val create_session : t -> id:string -> directory:string -> Session.t
  (** Create a new session *)

  val get_session : t -> id:string -> Session.t option
  (** Get a session by ID *)

  val list_sessions : t -> Session.t list
  (** List all sessions *)

  val remove_session : t -> id:string -> unit
  (** Remove a session by ID *)

  val cleanup_old_sessions : t -> max_age:float -> unit
  (** Remove sessions inactive for more than max_age seconds *)
end
