(** Persistent runtime storage for sessions, message history, memory, and todos. *)

type t

type scheduled_task = {
  id : int;
  chat_id : int;
  prompt : string;
  schedule_type : string;
  schedule_value : string;
  next_run_at : float option;
  status : string;
  created_at : float;
  updated_at : float;
  last_run_at : float option;
}

type scheduled_task_run = {
  id : int;
  task_id : int;
  chat_id : int;
  started_at : float;
  finished_at : float;
  success : bool;
  summary : string;
}

val create : string -> (t, string) result
val close : t -> unit

val store_message : t -> chat_id:int -> message:Llm_types.message -> (unit, string) result
val get_recent_messages : t -> chat_id:int -> limit:int -> (Llm_types.message list, string) result
val get_all_messages : t -> chat_id:int -> (Llm_types.message list, string) result

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

val insert_scheduled_task :
  t ->
  chat_id:int ->
  prompt:string ->
  schedule_type:string ->
  schedule_value:string ->
  next_run_at:float ->
  (int, string) result

val list_scheduled_tasks :
  t ->
  chat_id:int ->
  include_inactive:bool ->
  (scheduled_task list, string) result

val get_scheduled_task :
  t ->
  chat_id:int ->
  task_id:int ->
  (scheduled_task option, string) result

val update_scheduled_task_status :
  t ->
  chat_id:int ->
  task_id:int ->
  status:string ->
  next_run_at:float option ->
  (bool, string) result

val get_due_scheduled_tasks :
  t ->
  now:float ->
  limit:int ->
  (scheduled_task list, string) result

val insert_scheduled_task_run :
  t ->
  task_id:int ->
  chat_id:int ->
  started_at:float ->
  finished_at:float ->
  success:bool ->
  summary:string ->
  (unit, string) result

val update_scheduled_task_after_run :
  t ->
  task_id:int ->
  next_run_at:float option ->
  status:string ->
  last_run_at:float ->
  (unit, string) result

val get_scheduled_task_history :
  t ->
  chat_id:int ->
  task_id:int ->
  limit:int ->
  (scheduled_task_run list, string) result
