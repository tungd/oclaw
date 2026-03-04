(** OClaw server module *)

(** OClaw server configuration *)
type config = {
  host : string;
  port : int;
  llm_config : Llm_provider.provider_config;
  telegram_enabled : bool;
  telegram_token : string;
  telegram_allow_from : int64 list;
  workspace_dir : string;
}

(** OClaw server *)
module OclawServer : sig
  type t

  val create : config -> t
  (** Create a new OClaw server *)

  val start : t -> unit
  (** Start the server *)

  val stop : t -> unit
  (** Stop the server *)

  val is_running : t -> bool
  (** Check if server is running *)

  val get_http_server : t -> Http_server.Server.t
  (** Get the HTTP server instance *)

  val get_session_manager : t -> Session.Manager.t
  (** Get the session manager *)

  val get_knowledge : t -> Knowledge.Knowledge.t
  (** Get the knowledge store *)
end
