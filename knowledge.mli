(** Knowledge store for OClaw *)

(** {1 Knowledge types} *)

(** Knowledge store holding shared identity, skills, and workspace files *)
module Knowledge : sig
  type t

  val empty : unit -> t
  (** Create an empty knowledge store *)

  val load : workspace_dir:string -> t
  (** Load knowledge from workspace directory *)

  val refresh : t -> workspace_dir:string -> unit
  (** Refresh knowledge from workspace directory *)

  val get_identity : t -> Yojson.Basic.t
  (** Get identity JSON *)

  val get_agents : t -> Yojson.Basic.t option
  (** Get agents instructions JSON *)

  val get_soul : t -> Yojson.Basic.t option
  (** Get soul/personality JSON *)

  val get_user : t -> Yojson.Basic.t option
  (** Get user preferences JSON *)

  val get_skills : t -> Yojson.Basic.t list
  (** Get skills as list of JSON objects *)

  val get_system_prompt : t -> string
  (** Build complete system prompt from knowledge *)

  val last_updated : t -> float
  (** Get last update timestamp *)
end
