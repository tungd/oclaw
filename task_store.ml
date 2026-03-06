(** SQLite-backed task store with Domainslib wrappers *)

open Task_types

module Sql = Sqlite3
module Task = Domainslib.Task
module Log = (val Logs.src_log (Logs.Src.create "task_store") : Logs.LOG)

type config = {
  db_path : string;
  busy_timeout_ms : int;
  default_limit : int;
  max_limit : int;
  event_retention_days : int;
}

type t = {
  cfg : config;
  pool : Task.pool;
}

let rec mkdir_p path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  )

let with_db (store : t) f =
  Task.run store.pool (fun () ->
    let dir = Filename.dirname store.cfg.db_path in
    mkdir_p dir;
    let db = Sql.db_open store.cfg.db_path in
    Fun.protect
      ~finally:(fun () ->
        ignore (Sql.db_close db))
      (fun () ->
        Sql.busy_timeout db store.cfg.busy_timeout_ms;
        Sql.Rc.check (Sql.exec db "PRAGMA foreign_keys = ON;");
        Sql.Rc.check (Sql.exec db "PRAGMA journal_mode = WAL;");
        f db))

let sql_quote s =
  let escaped = String.concat "''" (String.split_on_char '\'' s) in
  Printf.sprintf "'%s'" escaped

let sql_opt = function
  | Some s -> sql_quote s
  | None -> "NULL"

let sql_opt_float = function
  | Some f -> string_of_float f
  | None -> "NULL"

let sql_opt_int = function
  | Some i -> string_of_int i
  | None -> "NULL"

let row_get row idx =
  if idx < Array.length row then row.(idx) else None

let row_text row idx =
  match row_get row idx with
  | Some s -> s
  | None -> ""

let row_opt_text row idx = row_get row idx

let row_float row idx =
  match row_get row idx with
  | Some s -> (try float_of_string s with _ -> 0.0)
  | None -> 0.0

let row_opt_float row idx =
  match row_get row idx with
  | Some s -> (try Some (float_of_string s) with _ -> None)
  | None -> None

let row_int row idx =
  match row_get row idx with
  | Some s -> (try int_of_string s with _ -> 0)
  | None -> 0

let row_to_task row : task =
  {
    id = row_text row 0;
    kind = row_text row 1;
    title = row_text row 2;
    description = row_text row 3;
    status = row_text row 4;
    priority = row_int row 5;
    assignee = row_opt_text row 6;
    created_by = row_opt_text row 7;
    created_at = row_float row 8;
    updated_at = row_float row 9;
    started_at = row_opt_float row 10;
    closed_at = row_opt_float row 11;
    close_reason = row_opt_text row 12;
    result = row_opt_text row 13;
    error = row_opt_text row 14;
    parent_id = row_opt_text row 15;
    session_id = row_opt_text row 16;
  }

let row_to_dependency row : dependency =
  {
    from_task_id = row_text row 0;
    to_task_id = row_text row 1;
    dep_type = row_text row 2;
  }

let row_to_event row : task_event =
  {
    seq = row_int row 0;
    task_id = row_text row 1;
    event_type = row_text row 2;
    status = row_opt_text row 3;
    actor = row_text row 4;
    payload_json = row_text row 5;
    created_at = row_float row 6;
  }

let query_rows db sql row_conv =
  let rows = ref [] in
  Sql.Rc.check
    (Sql.exec db ~cb:(fun row _headers -> rows := row_conv row :: !rows) sql);
  List.rev !rows

let query_one db sql row_conv =
  match query_rows db sql row_conv with
  | [] -> None
  | x :: _ -> Some x

let execute db sql =
  try
    Sql.Rc.check (Sql.exec db sql);
    Ok ()
  with
  | Sql.SqliteError err -> Error err
  | exn -> Error (Printexc.to_string exn)

let with_tx db f =
  match execute db "BEGIN IMMEDIATE;" with
  | Error e -> Error e
  | Ok () ->
      (match f () with
       | Ok v ->
           (match execute db "COMMIT;" with
            | Ok () -> Ok v
            | Error e ->
                ignore (execute db "ROLLBACK;");
                Error e)
       | Error e ->
           ignore (execute db "ROLLBACK;");
           Error e)

let insert_event_sql ~task_id ~event_type ~status ~actor ~payload_json ~created_at =
  Printf.sprintf
    "INSERT INTO task_events (task_id, event_type, status, actor, payload_json, created_at) VALUES (%s, %s, %s, %s, %s, %f);"
    (sql_quote task_id)
    (sql_quote event_type)
    (sql_opt status)
    (sql_quote actor)
    (sql_quote payload_json)
    created_at

let get_task_sql id =
  Printf.sprintf
    "SELECT id, kind, title, description, status, priority, assignee, created_by, created_at, updated_at, started_at, closed_at, close_reason, result, error, parent_id, session_id FROM tasks WHERE id = %s LIMIT 1;"
    (sql_quote id)

let create (cfg : config) =
  let num_domains = max 0 (Domain.recommended_domain_count () - 1) in
  let pool = Task.setup_pool ~name:"task-store" ~num_domains () in
  { cfg; pool }

let initialize (store : t) =
  with_db store (fun db ->
    let schema_sql =
      [
        "CREATE TABLE IF NOT EXISTS tasks ("
        ^ "id TEXT PRIMARY KEY,"
        ^ "kind TEXT NOT NULL,"
        ^ "title TEXT NOT NULL,"
        ^ "description TEXT NOT NULL DEFAULT '',"
        ^ "status TEXT NOT NULL,"
        ^ "priority INTEGER NOT NULL DEFAULT 2,"
        ^ "assignee TEXT,"
        ^ "created_by TEXT,"
        ^ "created_at REAL NOT NULL,"
        ^ "updated_at REAL NOT NULL,"
        ^ "started_at REAL,"
        ^ "closed_at REAL,"
        ^ "close_reason TEXT,"
        ^ "result TEXT,"
        ^ "error TEXT,"
        ^ "parent_id TEXT,"
        ^ "session_id TEXT"
        ^ ");";
        "CREATE TABLE IF NOT EXISTS task_dependencies ("
        ^ "from_task_id TEXT NOT NULL,"
        ^ "to_task_id TEXT NOT NULL,"
        ^ "dep_type TEXT NOT NULL,"
        ^ "PRIMARY KEY (from_task_id, to_task_id, dep_type)"
        ^ ");";
        "CREATE TABLE IF NOT EXISTS task_events ("
        ^ "seq INTEGER PRIMARY KEY AUTOINCREMENT,"
        ^ "task_id TEXT NOT NULL,"
        ^ "event_type TEXT NOT NULL,"
        ^ "status TEXT,"
        ^ "actor TEXT NOT NULL,"
        ^ "payload_json TEXT NOT NULL,"
        ^ "created_at REAL NOT NULL"
        ^ ");";
        "CREATE INDEX IF NOT EXISTS idx_tasks_status_priority_updated ON tasks (status, priority, updated_at DESC);";
        "CREATE INDEX IF NOT EXISTS idx_task_deps_to_type ON task_dependencies (to_task_id, dep_type);";
        "CREATE INDEX IF NOT EXISTS idx_task_events_task_seq ON task_events (task_id, seq);";
      ]
    in
    let rec run = function
      | [] -> Ok ()
      | sql :: rest ->
          (match execute db sql with
           | Ok () -> run rest
           | Error e -> Error e)
    in
    match run schema_sql with
    | Error e -> Error e
    | Ok () ->
        let cutoff = Unix.gettimeofday () -. (float_of_int store.cfg.event_retention_days *. 86400.0) in
        let cleanup =
          Printf.sprintf "DELETE FROM task_events WHERE created_at < %f;" cutoff
        in
        execute db cleanup)

let get_task (store : t) id =
  with_db store (fun db ->
    try Ok (query_one db (get_task_sql id) row_to_task)
    with
    | Sql.SqliteError err -> Error err
    | exn -> Error (Printexc.to_string exn))

let insert_task
    (store : t)
    ~(kind : string)
    ~(title : string)
    ~(description : string)
    ~(status : string)
    ~(priority : int)
    ~(assignee : string option)
    ~(created_by : string option)
    ~(parent_id : string option)
    ~(session_id : string option)
    ~(actor : string)
  =
  with_db store (fun db ->
    let created_at = now () in
    let id = make_task_id () in
    let started_at = if status = "in_progress" then Some created_at else None in
    let closed_at = if is_terminal_status status then Some created_at else None in
    let close_reason = if status = "closed" then Some "created_as_closed" else None in
    with_tx db (fun () ->
      let insert_sql =
        Printf.sprintf
          "INSERT INTO tasks (id, kind, title, description, status, priority, assignee, created_by, created_at, updated_at, started_at, closed_at, close_reason, result, error, parent_id, session_id) VALUES (%s, %s, %s, %s, %s, %d, %s, %s, %f, %f, %s, %s, %s, NULL, NULL, %s, %s);"
          (sql_quote id)
          (sql_quote kind)
          (sql_quote title)
          (sql_quote description)
          (sql_quote status)
          priority
          (sql_opt assignee)
          (sql_opt created_by)
          created_at
          created_at
          (sql_opt_float started_at)
          (sql_opt_float closed_at)
          (sql_opt close_reason)
          (sql_opt parent_id)
          (sql_opt session_id)
      in
      match execute db insert_sql with
      | Error e -> Error e
      | Ok () ->
          let event_type = if status = "in_progress" then "started" else "created" in
          let payload =
            `Assoc [
              ("title", `String title);
              ("status", `String status);
              ("kind", `String kind);
            ]
            |> Yojson.Safe.to_string
          in
          let event_sql =
            insert_event_sql
              ~task_id:id
              ~event_type
              ~status:(Some status)
              ~actor
              ~payload_json:payload
              ~created_at
          in
          (match execute db event_sql with
           | Error e -> Error e
           | Ok () ->
               match query_one db (get_task_sql id) row_to_task with
               | Some task -> Ok task
               | None -> Error "Task was created but could not be fetched")))

let list_tasks (store : t) (req : task_list_request) =
  with_db store (fun db ->
    try
      let conditions = ref [] in
      Option.iter (fun s -> conditions := Printf.sprintf "status = %s" (sql_quote (normalize_status s)) :: !conditions) req.status;
      Option.iter (fun s -> conditions := Printf.sprintf "assignee = %s" (sql_quote s) :: !conditions) req.assignee;
      Option.iter (fun s -> conditions := Printf.sprintf "kind = %s" (sql_quote s) :: !conditions) req.kind;
      Option.iter (fun c -> conditions := Printf.sprintf "created_at < %f" c :: !conditions) req.cursor;
      let where = if !conditions = [] then "" else " WHERE " ^ String.concat " AND " (List.rev !conditions) in
      let limit =
        req.limit
        |> Option.value ~default:store.cfg.default_limit
        |> max 1
        |> min store.cfg.max_limit
      in
      let sql =
        "SELECT id, kind, title, description, status, priority, assignee, created_by, created_at, updated_at, started_at, closed_at, close_reason, result, error, parent_id, session_id"
        ^ " FROM tasks"
        ^ where
        ^ " ORDER BY created_at DESC"
        ^ Printf.sprintf " LIMIT %d;" limit
      in
      Ok (query_rows db sql row_to_task)
    with
    | Sql.SqliteError err -> Error err
    | exn -> Error (Printexc.to_string exn))

let list_ready_tasks (store : t) ?limit () =
  with_db store (fun db ->
    try
      let limit =
        Option.value ~default:store.cfg.default_limit limit
        |> max 1
        |> min store.cfg.max_limit
      in
      let sql =
        "SELECT t.id, t.kind, t.title, t.description, t.status, t.priority, t.assignee, t.created_by, t.created_at, t.updated_at, t.started_at, t.closed_at, t.close_reason, t.result, t.error, t.parent_id, t.session_id"
        ^ " FROM tasks t"
        ^ " WHERE t.status IN ('open', 'in_progress')"
        ^ " AND NOT EXISTS ("
        ^ "   SELECT 1 FROM task_dependencies d"
        ^ "   JOIN tasks blocker ON blocker.id = d.from_task_id"
        ^ "   WHERE d.to_task_id = t.id"
        ^ "   AND d.dep_type = 'blocks'"
        ^ "   AND blocker.status NOT IN ('closed', 'canceled', 'failed')"
        ^ " )"
        ^ " ORDER BY t.priority ASC, t.updated_at DESC"
        ^ Printf.sprintf " LIMIT %d;" limit
      in
      Ok (query_rows db sql row_to_task)
    with
    | Sql.SqliteError err -> Error err
    | exn -> Error (Printexc.to_string exn))

let list_dependencies (store : t) task_id =
  with_db store (fun db ->
    try
      let sql =
        Printf.sprintf
          "SELECT from_task_id, to_task_id, dep_type FROM task_dependencies WHERE to_task_id = %s ORDER BY from_task_id ASC;"
          (sql_quote task_id)
      in
      Ok (query_rows db sql row_to_dependency)
    with
    | Sql.SqliteError err -> Error err
    | exn -> Error (Printexc.to_string exn))

let list_events (store : t) task_id ?after_seq ?limit () =
  with_db store (fun db ->
    try
      let limit =
        Option.value ~default:store.cfg.default_limit limit
        |> max 1
        |> min store.cfg.max_limit
      in
      let after_clause =
        match after_seq with
        | Some seq when seq >= 0 -> Printf.sprintf " AND seq > %d" seq
        | _ -> ""
      in
      let sql =
        Printf.sprintf
          "SELECT seq, task_id, event_type, status, actor, payload_json, created_at FROM task_events WHERE task_id = %s%s ORDER BY seq ASC LIMIT %d;"
          (sql_quote task_id)
          after_clause
          limit
      in
      Ok (query_rows db sql row_to_event)
    with
    | Sql.SqliteError err -> Error err
    | exn -> Error (Printexc.to_string exn))

let add_dependency (store : t) ~from_task_id ~to_task_id ~dep_type ~actor =
  with_db store (fun db ->
    let dep_type = normalize_dep_type dep_type in
    with_tx db (fun () ->
      let exists_sql id = Printf.sprintf "SELECT id, kind, title, description, status, priority, assignee, created_by, created_at, updated_at, started_at, closed_at, close_reason, result, error, parent_id, session_id FROM tasks WHERE id = %s LIMIT 1;" (sql_quote id) in
      let from_exists = query_one db (exists_sql from_task_id) row_to_task in
      let to_exists = query_one db (exists_sql to_task_id) row_to_task in
      if from_exists = None then Error (Printf.sprintf "from_task_id not found: %s" from_task_id)
      else if to_exists = None then Error (Printf.sprintf "to_task_id not found: %s" to_task_id)
      else if from_task_id = to_task_id then Error "Cannot add self dependency"
      else
        let insert_sql =
          Printf.sprintf
            "INSERT OR IGNORE INTO task_dependencies (from_task_id, to_task_id, dep_type) VALUES (%s, %s, %s);"
            (sql_quote from_task_id)
            (sql_quote to_task_id)
            (sql_quote dep_type)
        in
        match execute db insert_sql with
        | Error e -> Error e
        | Ok () ->
            let created_at = now () in
            let payload =
              `Assoc [
                ("from_task_id", `String from_task_id);
                ("to_task_id", `String to_task_id);
                ("dep_type", `String dep_type);
              ]
              |> Yojson.Safe.to_string
            in
            let event_sql =
              insert_event_sql
                ~task_id:to_task_id
                ~event_type:"dependency_added"
                ~status:None
                ~actor
                ~payload_json:payload
                ~created_at
            in
            (match execute db event_sql with
             | Error e -> Error e
             | Ok () ->
                 let sql =
                   Printf.sprintf
                     "SELECT from_task_id, to_task_id, dep_type FROM task_dependencies WHERE from_task_id = %s AND to_task_id = %s AND dep_type = %s LIMIT 1;"
                     (sql_quote from_task_id)
                     (sql_quote to_task_id)
                     (sql_quote dep_type)
                 in
                 (match query_one db sql row_to_dependency with
                  | Some dep -> Ok dep
                  | None -> Error "Dependency not found after insert"))))

let transition_allowed ~from_status ~to_status =
  let from_status = normalize_status from_status in
  let to_status = normalize_status to_status in
  match from_status with
  | "open" -> List.mem to_status ["in_progress"; "blocked"; "deferred"; "closed"; "canceled"]
  | "in_progress" -> List.mem to_status ["blocked"; "deferred"; "closed"; "failed"; "canceled"]
  | "blocked" -> List.mem to_status ["open"; "in_progress"; "deferred"; "canceled"]
  | "deferred" -> List.mem to_status ["open"; "in_progress"; "canceled"]
  | "closed" | "failed" | "canceled" -> false
  | _ -> false

let update_task
    (store : t)
    ~(id : string)
    ~(req : task_update_request)
    ~(actor : string)
    ~(event_type : string)
  =
  with_db store (fun db ->
    with_tx db (fun () ->
      match query_one db (get_task_sql id) row_to_task with
      | None -> Error (Printf.sprintf "Task not found: %s" id)
      | Some current ->
          let next_status = Option.map normalize_status req.status in
          (match next_status with
           | Some s when not (is_valid_status s) -> Error (Printf.sprintf "Invalid status: %s" s)
           | Some s when not (transition_allowed ~from_status:current.status ~to_status:s) ->
               Error (Printf.sprintf "Invalid transition: %s -> %s" current.status s)
           | _ ->
               let now_ts = now () in
               let title = Option.value ~default:current.title req.title in
               let description = Option.value ~default:current.description req.description in
               let status = Option.value ~default:current.status next_status in
               let priority = Option.value ~default:current.priority req.priority in
               let assignee = if req.assignee <> None then req.assignee else current.assignee in
               let close_reason = if req.close_reason <> None then req.close_reason else current.close_reason in
               let result = if req.result <> None then req.result else current.result in
               let error = if req.error <> None then req.error else current.error in
               let started_at =
                 if status = "in_progress" && current.started_at = None then Some now_ts
                 else current.started_at
               in
               let closed_at = if is_terminal_status status then Some now_ts else current.closed_at in
               let update_sql =
                 Printf.sprintf
                   "UPDATE tasks SET title = %s, description = %s, status = %s, priority = %d, assignee = %s, updated_at = %f, started_at = %s, closed_at = %s, close_reason = %s, result = %s, error = %s WHERE id = %s;"
                   (sql_quote title)
                   (sql_quote description)
                   (sql_quote status)
                   priority
                   (sql_opt assignee)
                   now_ts
                   (sql_opt_float started_at)
                   (sql_opt_float closed_at)
                   (sql_opt close_reason)
                   (sql_opt result)
                   (sql_opt error)
                   (sql_quote id)
               in
               (match execute db update_sql with
                | Error e -> Error e
                | Ok () ->
                    let payload =
                      `Assoc [
                        ("title", `String title);
                        ("status", `String status);
                        ("priority", `Int priority);
                      ]
                      |> Yojson.Safe.to_string
                    in
                    let event_sql =
                      insert_event_sql
                        ~task_id:id
                        ~event_type
                        ~status:(Some status)
                        ~actor
                        ~payload_json:payload
                        ~created_at:now_ts
                    in
                    (match execute db event_sql with
                     | Error e -> Error e
                     | Ok () ->
                         match query_one db (get_task_sql id) row_to_task with
                         | Some task -> Ok task
                         | None -> Error "Task not found after update")))))

let claim_task (store : t) ~(id : string) ~(assignee : string option) ~(actor : string) =
  let req =
    {
      title = None;
      description = None;
      status = Some "in_progress";
      priority = None;
      assignee;
      close_reason = None;
      result = None;
      error = None;
    }
  in
  update_task store ~id ~req ~actor ~event_type:"claimed"

let close_task (store : t) ~(id : string) ~(reason : string option) ~(actor : string) =
  let req =
    {
      title = None;
      description = None;
      status = Some "closed";
      priority = None;
      assignee = None;
      close_reason = reason;
      result = None;
      error = None;
    }
  in
  update_task store ~id ~req ~actor ~event_type:"closed"

let cancel_task (store : t) ~(id : string) ~(actor : string) =
  let req =
    {
      title = None;
      description = None;
      status = Some "canceled";
      priority = None;
      assignee = None;
      close_reason = Some "Canceled";
      result = None;
      error = Some "Canceled by request";
    }
  in
  update_task store ~id ~req ~actor ~event_type:"canceled"

let complete_task (store : t) ~(id : string) ~(result : string option) ~(actor : string) =
  let req =
    {
      title = None;
      description = None;
      status = Some "closed";
      priority = None;
      assignee = None;
      close_reason = Some "completed";
      result;
      error = None;
    }
  in
  update_task store ~id ~req ~actor ~event_type:"completed"

let fail_task (store : t) ~(id : string) ~(error : string) ~(actor : string) =
  let req =
    {
      title = None;
      description = None;
      status = Some "failed";
      priority = None;
      assignee = None;
      close_reason = Some "failed";
      result = None;
      error = Some error;
    }
  in
  update_task store ~id ~req ~actor ~event_type:"failed"
