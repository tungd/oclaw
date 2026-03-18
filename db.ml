open Sqlite3

module Log = (val Logs.src_log (Logs.Src.create "db") : Logs.LOG)

type t = {
  db : Sqlite3.db;
  mutex : Mutex.t;
}

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

let ensure_parent_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path)

let exec db sql =
  try
    match Sqlite3.exec db sql with
    | Rc.OK -> Ok ()
    | rc -> Error (Printf.sprintf "sqlite exec failed (%s): %s" (Rc.to_string rc) (errmsg db))
  with Sqlite3.SqliteError msg ->
    Error (Printf.sprintf "sqlite exec exception: %s" msg)

let timestamp () = Unix.gettimeofday ()

let prepare db sql =
  try
    Sqlite3.prepare db sql
  with Sqlite3.SqliteError msg ->
    failwith (Printf.sprintf "sqlite prepare exception: %s" msg)

let finalize stmt =
  try
    match Sqlite3.finalize stmt with
    | Rc.OK -> Ok ()
    | rc -> Error (Printf.sprintf "sqlite finalize failed: %s" (Rc.to_string rc))
  with Sqlite3.SqliteError msg ->
    Error (Printf.sprintf "sqlite finalize exception: %s" msg)

let step_expect_done db stmt =
  try
    match Sqlite3.step stmt with
    | Rc.DONE -> Ok ()
    | rc -> Error (Printf.sprintf "sqlite step failed (%s): %s" (Rc.to_string rc) (errmsg db))
  with Sqlite3.SqliteError msg ->
    Error (Printf.sprintf "sqlite step exception: %s" msg)

let bind_text stmt index value =
  Sqlite3.bind stmt index (Data.TEXT value)

let bind_int stmt index value =
  Sqlite3.bind stmt index (Data.INT (Int64.of_int value))

let bind_float stmt index value =
  Sqlite3.bind stmt index (Data.FLOAT value)

let bind_opt_int stmt index = function
  | None -> Sqlite3.bind stmt index Data.NULL
  | Some value -> bind_int stmt index value

let bind_opt_float stmt index = function
  | None -> Sqlite3.bind stmt index Data.NULL
  | Some value -> bind_float stmt index value

let with_statement db sql f =
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (finalize stmt))
    (fun () -> f stmt)

let create path =
  let db = ref None in
  try
    ensure_parent_dir path;
    let opened_db = Sqlite3.db_open path in
    db := Some opened_db;
    let setup =
      [
        "PRAGMA journal_mode = WAL";
        "PRAGMA synchronous = NORMAL";
        "CREATE TABLE IF NOT EXISTS messages (\
         id INTEGER PRIMARY KEY AUTOINCREMENT,\
         chat_id INTEGER NOT NULL,\
         role TEXT NOT NULL,\
         content TEXT NOT NULL,\
         created_at REAL NOT NULL\
         )";
        "CREATE INDEX IF NOT EXISTS idx_messages_chat_time ON messages(chat_id, created_at)";
        "CREATE TABLE IF NOT EXISTS todos (\
         chat_id INTEGER PRIMARY KEY,\
         todo_json TEXT NOT NULL,\
         updated_at REAL NOT NULL\
         )";
        "CREATE TABLE IF NOT EXISTS memories (\
         id INTEGER PRIMARY KEY AUTOINCREMENT,\
         chat_id INTEGER NULL,\
         scope TEXT NOT NULL,\
         content TEXT NOT NULL,\
         source TEXT NOT NULL,\
         created_at REAL NOT NULL\
         )";
        "CREATE TABLE IF NOT EXISTS scheduled_tasks (\
         id INTEGER PRIMARY KEY AUTOINCREMENT,\
         chat_id INTEGER NOT NULL,\
         prompt TEXT NOT NULL,\
         schedule_type TEXT NOT NULL,\
         schedule_value TEXT NOT NULL,\
         next_run_at REAL NULL,\
         status TEXT NOT NULL,\
         created_at REAL NOT NULL,\
         updated_at REAL NOT NULL,\
         last_run_at REAL NULL\
         )";
        "CREATE INDEX IF NOT EXISTS idx_scheduled_tasks_due \
         ON scheduled_tasks(status, next_run_at)";
        "CREATE INDEX IF NOT EXISTS idx_scheduled_tasks_chat \
         ON scheduled_tasks(chat_id, status, updated_at DESC)";
        "CREATE TABLE IF NOT EXISTS scheduled_task_runs (\
         id INTEGER PRIMARY KEY AUTOINCREMENT,\
         task_id INTEGER NOT NULL,\
         chat_id INTEGER NOT NULL,\
         started_at REAL NOT NULL,\
         finished_at REAL NOT NULL,\
         success INTEGER NOT NULL,\
         summary TEXT NOT NULL,\
         FOREIGN KEY(task_id) REFERENCES scheduled_tasks(id)\
         )";
        "CREATE INDEX IF NOT EXISTS idx_scheduled_task_runs_task \
         ON scheduled_task_runs(task_id, started_at DESC)";
      ]
    in
    let result =
      List.fold_left
        (fun (acc : (unit, string) result) sql ->
          match acc with
          | Error _ as err -> err
          | Ok () -> exec opened_db sql)
        (Ok ()) setup
    in
    begin
      match result with
      | Ok () -> 
          Ok { db = opened_db; mutex = Mutex.create () }
      | Error err ->
          Sqlite3.db_close opened_db |> ignore;
          Error err
    end
  with
  | Sqlite3.SqliteError msg ->
      begin
        match !db with
        | Some db -> Sqlite3.db_close db |> ignore
        | None -> ()
      end;
      Error (Printf.sprintf "sqlite create exception: %s" msg)
  | exn ->
      begin
        match !db with
        | Some db -> Sqlite3.db_close db |> ignore
        | None -> ()
      end;
      Error (Printexc.to_string exn)

let close t =
  ignore (Sqlite3.db_close t.db)

let with_lock t f =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

let store_message t ~chat_id ~message =
  with_lock t (fun () ->
    with_statement t.db
      "INSERT INTO messages (chat_id, role, content, created_at) VALUES (?1, ?2, ?3, ?4)"
      (fun stmt ->
        let content_json = Llm_types.message_content_to_yojson (message : Llm_types.message).content |> Yojson.Safe.to_string in
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_text stmt 2 message.Llm_types.role);
        ignore (bind_text stmt 3 content_json);
        ignore (bind_float stmt 4 (timestamp ()));
        step_expect_done t.db stmt))


let read_message_row stmt =
  let role =
    match Sqlite3.column stmt 1 with
    | Data.TEXT text -> text
    | _ -> ""
  in
  let content_json =
    match Sqlite3.column stmt 2 with
    | Data.TEXT text -> text
    | _ -> ""
  in
  let content =
    match Llm_types.message_content_of_yojson (Yojson.Safe.from_string content_json) with
    | Ok content -> content
    | Error _ -> Llm_types.Text_content content_json
  in
  { Llm_types.role; content }

let collect_messages stmt =
  let rec loop acc =
    try
      match Sqlite3.step stmt with
      | Rc.ROW -> loop (read_message_row stmt :: acc)
      | Rc.DONE -> Ok (List.rev acc)
      | rc -> Error (Printf.sprintf "sqlite read failed: %s" (Rc.to_string rc))
    with Sqlite3.SqliteError msg ->
      Error (Printf.sprintf "sqlite read exception: %s" msg)
  in
  loop []

let get_recent_messages t ~chat_id ~limit =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT chat_id, role, content, created_at \
       FROM messages WHERE chat_id = ?1 ORDER BY created_at DESC LIMIT ?2"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_int stmt 2 limit);
        match collect_messages stmt with
        | Ok messages -> Ok (List.rev messages)
        | Error err -> Error err))

let get_all_messages t ~chat_id =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT chat_id, role, content, created_at \
       FROM messages WHERE chat_id = ?1 ORDER BY created_at ASC"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        collect_messages stmt))

let delete_session t ~chat_id =
  with_lock t (fun () ->
    with_statement t.db
      "DELETE FROM messages WHERE chat_id = ?1"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        match Sqlite3.step stmt with
        | Rc.DONE ->
            let changed = Sqlite3.changes t.db > 0 in
            Ok changed
        | rc -> Error (Printf.sprintf "sqlite delete session failed: %s" (Rc.to_string rc))))

let save_todo t ~chat_id ~todo_json =
  with_lock t (fun () ->
    with_statement t.db
      "INSERT INTO todos (chat_id, todo_json, updated_at) VALUES (?1, ?2, ?3) \
       ON CONFLICT(chat_id) DO UPDATE SET todo_json = excluded.todo_json, updated_at = excluded.updated_at"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_text stmt 2 todo_json);
        ignore (bind_float stmt 3 (timestamp ()));
        step_expect_done t.db stmt))

let load_todo t ~chat_id =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT todo_json FROM todos WHERE chat_id = ?1"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        match Sqlite3.step stmt with
        | Rc.ROW ->
            begin
              match Sqlite3.column stmt 0 with
              | Data.TEXT text -> Ok (Some text)
              | _ -> Ok None
            end
        | Rc.DONE -> Ok None
        | rc -> Error (Printf.sprintf "sqlite load todo failed: %s" (Rc.to_string rc))))

let insert_memory t ~chat_id ~scope ~content ~source =
  with_lock t (fun () ->
    with_statement t.db
      "INSERT INTO memories (chat_id, scope, content, source, created_at) VALUES (?1, ?2, ?3, ?4, ?5)"
      (fun stmt ->
        ignore (bind_opt_int stmt 1 chat_id);
        ignore (bind_text stmt 2 scope);
        ignore (bind_text stmt 3 content);
        ignore (bind_text stmt 4 source);
        ignore (bind_float stmt 5 (timestamp ()));
        step_expect_done t.db stmt))

let float_column stmt index =
  match Sqlite3.column stmt index with
  | Data.FLOAT value -> Some value
  | Data.INT value -> Some (Int64.to_float value)
  | _ -> None

let required_int_column stmt index =
  match Sqlite3.column stmt index with
  | Data.INT value -> Int64.to_int value
  | _ -> 0

let required_text_column stmt index =
  match Sqlite3.column stmt index with
  | Data.TEXT value -> value
  | _ -> ""

let required_float_column stmt index =
  match float_column stmt index with
  | Some value -> value
  | None -> 0.0

let read_scheduled_task_row stmt =
  {
    id = required_int_column stmt 0;
    chat_id = required_int_column stmt 1;
    prompt = required_text_column stmt 2;
    schedule_type = required_text_column stmt 3;
    schedule_value = required_text_column stmt 4;
    next_run_at = float_column stmt 5;
    status = required_text_column stmt 6;
    created_at = required_float_column stmt 7;
    updated_at = required_float_column stmt 8;
    last_run_at = float_column stmt 9;
  }

let collect_scheduled_tasks stmt =
  let rec loop acc =
    try
      match Sqlite3.step stmt with
      | Rc.ROW -> loop (read_scheduled_task_row stmt :: acc)
      | Rc.DONE -> Ok (List.rev acc)
      | rc -> Error (Printf.sprintf "sqlite read scheduled tasks failed: %s" (Rc.to_string rc))
    with Sqlite3.SqliteError msg ->
      Error (Printf.sprintf "sqlite read scheduled tasks exception: %s" msg)
  in
  loop []

let insert_scheduled_task t ~chat_id ~prompt ~schedule_type ~schedule_value ~next_run_at =
  with_lock t (fun () ->
    with_statement t.db
      "INSERT INTO scheduled_tasks \
       (chat_id, prompt, schedule_type, schedule_value, next_run_at, status, created_at, updated_at, last_run_at) \
       VALUES (?1, ?2, ?3, ?4, ?5, 'active', ?6, ?7, NULL)"
      (fun stmt ->
        let now = timestamp () in
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_text stmt 2 prompt);
        ignore (bind_text stmt 3 schedule_type);
        ignore (bind_text stmt 4 schedule_value);
        ignore (bind_float stmt 5 next_run_at);
        ignore (bind_float stmt 6 now);
        ignore (bind_float stmt 7 now);
        match step_expect_done t.db stmt with
        | Error _ as err -> err
        | Ok () -> Ok (Int64.to_int (Sqlite3.last_insert_rowid t.db))))

let list_scheduled_tasks t ~chat_id ~include_inactive =
  with_lock t (fun () ->
    let sql =
      if include_inactive then
        "SELECT id, chat_id, prompt, schedule_type, schedule_value, next_run_at, status, created_at, updated_at, last_run_at \
         FROM scheduled_tasks WHERE chat_id = ?1 ORDER BY updated_at DESC"
      else
        "SELECT id, chat_id, prompt, schedule_type, schedule_value, next_run_at, status, created_at, updated_at, last_run_at \
         FROM scheduled_tasks WHERE chat_id = ?1 AND status IN ('active', 'paused') ORDER BY updated_at DESC"
    in
    with_statement t.db sql (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        collect_scheduled_tasks stmt))

let get_scheduled_task t ~chat_id ~task_id =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT id, chat_id, prompt, schedule_type, schedule_value, next_run_at, status, created_at, updated_at, last_run_at \
       FROM scheduled_tasks WHERE chat_id = ?1 AND id = ?2"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_int stmt 2 task_id);
        match Sqlite3.step stmt with
        | Rc.ROW -> Ok (Some (read_scheduled_task_row stmt))
        | Rc.DONE -> Ok None
        | rc -> Error (Printf.sprintf "sqlite get scheduled task failed: %s" (Rc.to_string rc))))

let update_scheduled_task_status t ~chat_id ~task_id ~status ~next_run_at =
  with_lock t (fun () ->
    with_statement t.db
      "UPDATE scheduled_tasks SET status = ?3, next_run_at = ?4, updated_at = ?5 \
       WHERE chat_id = ?1 AND id = ?2"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_int stmt 2 task_id);
        ignore (bind_text stmt 3 status);
        ignore (bind_opt_float stmt 4 next_run_at);
        ignore (bind_float stmt 5 (timestamp ()));
        match step_expect_done t.db stmt with
        | Error _ as err -> err
        | Ok () -> Ok (Sqlite3.changes t.db > 0)))

let get_due_scheduled_tasks t ~now ~limit =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT id, chat_id, prompt, schedule_type, schedule_value, next_run_at, status, created_at, updated_at, last_run_at \
       FROM scheduled_tasks \
       WHERE status = 'active' AND next_run_at IS NOT NULL AND next_run_at <= ?1 \
       ORDER BY next_run_at ASC LIMIT ?2"
      (fun stmt ->
        ignore (bind_float stmt 1 now);
        ignore (bind_int stmt 2 limit);
        collect_scheduled_tasks stmt))

let insert_scheduled_task_run t ~task_id ~chat_id ~started_at ~finished_at ~success ~summary =
  with_lock t (fun () ->
    with_statement t.db
      "INSERT INTO scheduled_task_runs \
       (task_id, chat_id, started_at, finished_at, success, summary) \
       VALUES (?1, ?2, ?3, ?4, ?5, ?6)"
      (fun stmt ->
        ignore (bind_int stmt 1 task_id);
        ignore (bind_int stmt 2 chat_id);
        ignore (bind_float stmt 3 started_at);
        ignore (bind_float stmt 4 finished_at);
        ignore (bind_int stmt 5 (if success then 1 else 0));
        ignore (bind_text stmt 6 summary);
        step_expect_done t.db stmt))

let update_scheduled_task_after_run t ~task_id ~next_run_at ~status ~last_run_at =
  with_lock t (fun () ->
    with_statement t.db
      "UPDATE scheduled_tasks \
       SET next_run_at = ?2, status = ?3, last_run_at = ?4, updated_at = ?5 \
       WHERE id = ?1"
      (fun stmt ->
        ignore (bind_int stmt 1 task_id);
        ignore (bind_opt_float stmt 2 next_run_at);
        ignore (bind_text stmt 3 status);
        ignore (bind_float stmt 4 last_run_at);
        ignore (bind_float stmt 5 (timestamp ()));
        step_expect_done t.db stmt))

let read_scheduled_task_run_row stmt =
  {
    id = required_int_column stmt 0;
    task_id = required_int_column stmt 1;
    chat_id = required_int_column stmt 2;
    started_at = required_float_column stmt 3;
    finished_at = required_float_column stmt 4;
    success = required_int_column stmt 5 <> 0;
    summary = required_text_column stmt 6;
  }

let collect_scheduled_task_runs stmt =
  let rec loop acc =
    try
      match Sqlite3.step stmt with
      | Rc.ROW -> loop (read_scheduled_task_run_row stmt :: acc)
      | Rc.DONE -> Ok (List.rev acc)
      | rc -> Error (Printf.sprintf "sqlite read scheduled task history failed: %s" (Rc.to_string rc))
    with Sqlite3.SqliteError msg ->
      Error (Printf.sprintf "sqlite read scheduled task history exception: %s" msg)
  in
  loop []

let get_scheduled_task_history t ~chat_id ~task_id ~limit =
  with_lock t (fun () ->
    with_statement t.db
      "SELECT id, task_id, chat_id, started_at, finished_at, success, summary \
       FROM scheduled_task_runs \
       WHERE chat_id = ?1 AND task_id = ?2 \
       ORDER BY started_at DESC LIMIT ?3"
      (fun stmt ->
        ignore (bind_int stmt 1 chat_id);
        ignore (bind_int stmt 2 task_id);
        ignore (bind_int stmt 3 limit);
        collect_scheduled_task_runs stmt))
