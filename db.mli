(** Persistent runtime storage for sessions, message history, memory, and todos. *)

type t

type stored_message = {
  chat_id : int;
  role : string;
  content : string;
  created_at : float;
}

val create : string -> (t, string) result
val close : t -> unit

val store_message : t -> chat_id:int -> role:string -> content:string -> (unit, string) result
val get_recent_messages : t -> chat_id:int -> limit:int -> (stored_message list, string) result
val get_all_messages : t -> chat_id:int -> (stored_message list, string) result

val save_session : t -> chat_id:int -> messages_json:string -> (unit, string) result
val load_session : t -> chat_id:int -> ((string * float) option, string) result
val delete_session : t -> chat_id:int -> (bool, string) result

val save_todo : t -> chat_id:int -> todo_json:string -> (unit, string) result
val load_todo : t -> chat_id:int -> (string option, string) result

val insert_memory :
  t ->
  chat_id:int option ->
  scope:string ->
  content:string ->
  source:string ->
  (unit, string) result
