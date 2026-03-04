(** Telegram Bot API client for OClaw *)

(** {1 Telegram types} *)

module User : sig
  type t = {
    id : int64;
    is_bot : bool;
    first_name : string;
    last_name : string;
    username : string option;
    language_code : string option;
  }

  val of_json : Yojson.Basic.t -> t
  val to_json : t -> Yojson.Basic.t
end

module Chat : sig
  type t = {
    id : int64;
    type_ : string;
    title : string option;
    username : string option;
    first_name : string option;
    last_name : string option;
  }

  val of_json : Yojson.Basic.t -> t
  val to_json : t -> Yojson.Basic.t
end

module Message : sig
  type t = {
    message_id : int;
    from : User.t option;
    chat : Chat.t;
    date : int;
    text : string option;
  }

  val of_json : Yojson.Basic.t -> t
  val to_json : t -> Yojson.Basic.t
end

module Update : sig
  type t = {
    update_id : int;
    message : Message.t option;
  }

  val of_json : Yojson.Basic.t -> t
  val to_json : t -> Yojson.Basic.t
end

(** {1 Bot API client} *)

module Bot : sig
  type t = {
    token : string;
    base_url : string;
  }

  val create : ?base_url:string -> string -> t
  (** Create a bot API client with the given token *)

  val get_updates : t -> offset:int -> timeout:int -> (Update.t list, string) result
  (** Get updates via long polling *)

  val send_message : t -> chat_id:int64 -> text:string -> (unit, string) result
  (** Send a text message to a chat *)

  val get_me : t -> (User.t, string) result
  (** Get bot information *)
end
