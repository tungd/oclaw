(** Telegram channel for OClaw server *)

(** Telegram configuration *)
type config = {
  enabled : bool;
  token : string;
  allow_from : int64 list;  (** List of allowed user IDs (empty = allow all) *)
}

(** Telegram channel *)
module Channel : sig
  type t

  val create : Telegram_api.Bot.t -> config -> Session.Manager.t -> Knowledge.Knowledge.t -> Llm_provider.provider_config -> t
  (** Create a new Telegram channel *)

  val start : t -> unit
  (** Start the channel (runs in a domain) *)

  val stop : t -> unit
  (** Stop the channel *)

  val is_running : t -> bool
  (** Check if channel is running *)
end
