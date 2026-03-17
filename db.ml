open Sqlite3

module Log = (val Logs.src_log (Logs.Src.create "db") : Logs.LOG)

type t = {
  db : Sqlite3.db;
}

type stored_message = {
  chat_id : int;
  role : string;
  content : string;
  created_at : float;
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
  match Sqlite3.exec db sql with
  | Rc.OK -> Ok ()
  | rc -> Error (Printf.sprintf "sqlite exec failed (%s): %s" (Rc.to_string rc) (errmsg db))

let timestamp () = Unix.gettimeofday ()

let prepare db sql =
  Sqlite3.prepare db sql

let finalize stmt =
  match Sqlite3.finalize stmt with
  | Rc.OK -> Ok ()
  | rc -> Error (Printf.sprintf "sqlite finalize failed: %s" (Rc.to_string rc))

let step_expect_done db stmt =
  match Sqlite3.step stmt with
  | Rc.DONE -> Ok ()
  | rc -> Error (Printf.sprintf "sqlite step failed (%s): %s" (Rc.to_string rc) (errmsg db))

let bind_text stmt index value =
  Sqlite3.bind stmt index (Data.TEXT value)

let bind_int stmt index value =
  Sqlite3.bind stmt index (Data.INT (Int64.of_int value))

let bind_float stmt index value =
  Sqlite3.bind stmt index (Data.FLOAT value)

let bind_opt_int stmt index = function
  | None -> Sqlite3.bind stmt index Data.NULL
  | Some value -> bind_int stmt index value

let with_statement db sql f =
  let stmt = prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (finalize stmt))
    (fun () -> f stmt)

let create path =
  try
    ensure_parent_dir path;
    let db = Sqlite3.db_open path in
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
        "CREATE TABLE IF NOT EXISTS sessions (\
         chat_id INTEGER PRIMARY KEY,\
         messages_json TEXT NOT NULL,\
         updated_at REAL NOT NULL\
         )";
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
      ]
    in
    let result =
      List.fold_left
        (fun (acc : (unit, string) result) sql ->
          match acc with
          | Error _ as err -> err
          | Ok () -> exec db sql)
        (Ok ()) setup
    in
    begin
      match result with
      | Ok () -> 
          (* Register vector search UDFs *)
          (match Vector_search.register_udfs db with
           | Ok () -> ()
           | Error msg -> Log.warn (fun m -> m "Vector UDF registration failed: %s" msg));
          (* Initialize vector schema *)
          Vector_search.init_schema db;
          Ok { db }
      | Error err ->
          Sqlite3.db_close db |> ignore;
          Error err
    end
  with exn ->
    Error (Printexc.to_string exn)

let close t =
  ignore (Sqlite3.db_close t.db)

let store_message t ~chat_id ~role ~content =
  with_statement t.db
    "INSERT INTO messages (chat_id, role, content, created_at) VALUES (?1, ?2, ?3, ?4)"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      ignore (bind_text stmt 2 role);
      ignore (bind_text stmt 3 content);
      ignore (bind_float stmt 4 (timestamp ()));
      step_expect_done t.db stmt)

let read_message_row stmt =
  let chat_id =
    match Sqlite3.column stmt 0 with
    | Data.INT i -> Int64.to_int i
    | _ -> 0
  in
  let role =
    match Sqlite3.column stmt 1 with
    | Data.TEXT text -> text
    | _ -> ""
  in
  let content =
    match Sqlite3.column stmt 2 with
    | Data.TEXT text -> text
    | _ -> ""
  in
  let created_at =
    match Sqlite3.column stmt 3 with
    | Data.FLOAT value -> value
    | Data.INT value -> Int64.to_float value
    | _ -> 0.0
  in
  { chat_id; role; content; created_at }

let collect_messages stmt =
  let rec loop acc =
    match Sqlite3.step stmt with
    | Rc.ROW -> loop (read_message_row stmt :: acc)
    | Rc.DONE -> Ok (List.rev acc)
    | rc -> Error (Printf.sprintf "sqlite read failed: %s" (Rc.to_string rc))
  in
  loop []

let get_recent_messages t ~chat_id ~limit =
  with_statement t.db
    "SELECT chat_id, role, content, created_at \
     FROM messages WHERE chat_id = ?1 ORDER BY created_at DESC LIMIT ?2"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      ignore (bind_int stmt 2 limit);
      match collect_messages stmt with
      | Ok messages -> Ok (List.rev messages)
      | Error err -> Error err)

let get_all_messages t ~chat_id =
  with_statement t.db
    "SELECT chat_id, role, content, created_at \
     FROM messages WHERE chat_id = ?1 ORDER BY created_at ASC"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      collect_messages stmt)

let save_session t ~chat_id ~messages_json =
  with_statement t.db
    "INSERT INTO sessions (chat_id, messages_json, updated_at) VALUES (?1, ?2, ?3) \
     ON CONFLICT(chat_id) DO UPDATE SET messages_json = excluded.messages_json, updated_at = excluded.updated_at"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      ignore (bind_text stmt 2 messages_json);
      ignore (bind_float stmt 3 (timestamp ()));
      step_expect_done t.db stmt)

let load_session t ~chat_id =
  with_statement t.db
    "SELECT messages_json, updated_at FROM sessions WHERE chat_id = ?1"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      match Sqlite3.step stmt with
      | Rc.ROW ->
          let json =
            match Sqlite3.column stmt 0 with
            | Data.TEXT text -> text
            | _ -> "[]"
          in
          let updated_at =
            match Sqlite3.column stmt 1 with
            | Data.FLOAT value -> value
            | Data.INT value -> Int64.to_float value
            | _ -> 0.0
          in
          Ok (Some (json, updated_at))
      | Rc.DONE -> Ok None
      | rc -> Error (Printf.sprintf "sqlite load session failed: %s" (Rc.to_string rc)))

let delete_session t ~chat_id =
  with_statement t.db
    "DELETE FROM sessions WHERE chat_id = ?1"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      match Sqlite3.step stmt with
      | Rc.DONE ->
          let changed = Sqlite3.changes t.db > 0 in
          Ok changed
      | rc -> Error (Printf.sprintf "sqlite delete session failed: %s" (Rc.to_string rc)))

let save_todo t ~chat_id ~todo_json =
  with_statement t.db
    "INSERT INTO todos (chat_id, todo_json, updated_at) VALUES (?1, ?2, ?3) \
     ON CONFLICT(chat_id) DO UPDATE SET todo_json = excluded.todo_json, updated_at = excluded.updated_at"
    (fun stmt ->
      ignore (bind_int stmt 1 chat_id);
      ignore (bind_text stmt 2 todo_json);
      ignore (bind_float stmt 3 (timestamp ()));
      step_expect_done t.db stmt)

let load_todo t ~chat_id =
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
      | rc -> Error (Printf.sprintf "sqlite load todo failed: %s" (Rc.to_string rc)))

let insert_memory t ~chat_id ~scope ~content ~source =
  with_statement t.db
    "INSERT INTO memories (chat_id, scope, content, source, created_at) VALUES (?1, ?2, ?3, ?4, ?5)"
    (fun stmt ->
      ignore (bind_opt_int stmt 1 chat_id);
      ignore (bind_text stmt 2 scope);
      ignore (bind_text stmt 3 content);
      ignore (bind_text stmt 4 source);
      ignore (bind_float stmt 5 (timestamp ()));
      step_expect_done t.db stmt)
