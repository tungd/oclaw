(** OClaw server - integrates HTTP server with agent, tasks, and effect handlers *)

open Http_server
open Session_types
open Agent
open Yojson.Safe.Util

module Log = (val Logs.src_log (Logs.Src.create "oclaw_server") : Logs.LOG)

(* {1 Server Configuration } *)

type config = {
  host : string;
  port : int;
  llm_config : Llm_provider.provider_config;
  model : string;
  max_connections : int;
  tasks_db_path : string;
  tasks_default_limit : int;
  tasks_max_limit : int;
  tasks_busy_timeout_ms : int;
  tasks_event_retention_days : int;
}

type t = {
  http_server : Http_server.t;
  agent : Agent.t;
  task_service : Task_service.t;
  config : config;
  mutable running : bool;
  mutex : Mutex.t;
}

let starts_with ~prefix s =
  let p_len = String.length prefix in
  String.length s >= p_len && String.sub s 0 p_len = prefix

let split_target target =
  match String.index_opt target '?' with
  | None -> (target, "")
  | Some idx ->
      let path = String.sub target 0 idx in
      let query = String.sub target (idx + 1) (String.length target - idx - 1) in
      (path, query)

let parse_query_string query =
  if String.trim query = "" then []
  else
    query
    |> String.split_on_char '&'
    |> List.filter_map (fun pair ->
         match String.split_on_char '=' pair with
         | [k; v] -> Some (k, v)
         | [k] -> Some (k, "")
         | _ -> None)

let query_param query key =
  query
  |> parse_query_string
  |> List.find_map (fun (k, v) -> if String.equal k key then Some v else None)

let int_of_string_opt s =
  try Some (int_of_string (String.trim s)) with _ -> None

let float_of_string_opt s =
  try Some (float_of_string (String.trim s)) with _ -> None

let read_json_body reqd on_json =
  let body_reader = Reqd.request_body reqd in
  read_body body_reader (function
    | Error (`Too_large max_bytes) ->
        respond_json ~status:`Payload_too_large reqd (`Assoc [
          ("error", `String "Request body too large");
          ("max_bytes", `Int max_bytes);
        ])
    | Error (`Exception exn) ->
        Log.err (fun m -> m "Body read error: %s" (Printexc.to_string exn));
        respond_json ~status:`Bad_request reqd (`Assoc [
          ("error", `String "Invalid request body");
        ])
    | Ok body ->
        (match Yojson.Safe.from_string body with
         | exception _ ->
             respond_json ~status:`Bad_request reqd (`Assoc [
               ("error", `String "Invalid JSON");
             ])
         | json -> on_json json))

let status_for_task_error err =
  if starts_with ~prefix:"Task not found:" err || starts_with ~prefix:"Subagent not found:" err then `Not_found
  else if starts_with ~prefix:"Invalid" err then `Bad_request
  else `Internal_server_error

let respond_task_result reqd = function
  | Ok json -> respond_json reqd json
  | Error err ->
      respond_json ~status:(status_for_task_error err) reqd (`Assoc [
        ("status", `String "error");
        ("error", `String err);
      ])

(* {1 API Handlers } *)

let health_handler reqd =
  respond_json reqd (`Assoc [
    ("status", `String "healthy");
    ("timestamp", `Float (Unix.gettimeofday ()));
    ("version", `String "1.0.0");
  ])

let chat_handler server reqd =
  let body_reader = Reqd.request_body reqd in
  read_body body_reader (function
    | Error (`Too_large max_bytes) ->
        respond_json ~status:`Payload_too_large reqd (`Assoc [
          ("error", `String "Request body too large");
          ("max_bytes", `Int max_bytes);
        ])
    | Error (`Exception exn) ->
        Log.err (fun m -> m "Chat body read error: %s" (Printexc.to_string exn));
        respond_json ~status:`Bad_request reqd (`Assoc [
          ("error", `String "Invalid request body");
        ])
    | Ok body ->
        begin
          match Yojson.Safe.from_string body with
          | exception _ ->
              respond_json ~status:`Bad_request reqd (`Assoc [
                ("error", `String "Invalid JSON");
              ])
          | json ->
              (try
                 let session_id =
                   match member "session_id" json with
                   | `String s -> s
                   | _ -> raise Not_found
                 in
                 let content =
                   match member "content" json with
                   | `String s -> s
                   | _ -> raise Not_found
                 in
                 submit_job server.http_server reqd (fun () ->
                   match Agent.process_query server.agent ~session_id ~content with
                   | Ok response ->
                       Async_response.json (`Assoc [
                         ("response", `String response);
                         ("session_id", `String session_id);
                         ("created_at", `Float (Unix.gettimeofday ()));
                       ])
                   | Error err ->
                       Async_response.json ~status:`Internal_server_error (`Assoc [
                         ("error", `String err);
                       ]))
               with
               | Not_found ->
                   respond_json ~status:`Bad_request reqd (`Assoc [
                     ("error", `String "Missing required fields: session_id, content");
                   ])
               | exn ->
                   Log.err (fun m -> m "Chat handler error: %s" (Printexc.to_string exn));
                   respond_json ~status:`Internal_server_error reqd (`Assoc [
                     ("error", `String "Internal server error");
                   ]))
        end)

let sessions_handler agent reqd =
  let sessions = Agent.list_sessions agent in
  let session_list = List.map (fun (_id, info) ->
    `Assoc [
      ("id", `String info.id);
      ("created_at", `Float info.created_at);
      ("last_active", `Float info.last_active);
      ("message_count", `Int info.message_count);
    ]
  ) sessions in
  respond_json reqd (`Assoc [
    ("sessions", `List session_list);
  ])

let knowledge_handler agent reqd =
  let knowledge = Agent.get_knowledge agent in
  respond_json reqd (`Assoc [
    ("identity", `String knowledge.identity);
    ("skills", `List (List.map (fun (s : Skills.Skill.t) ->
      `Assoc [
        ("name", `String s.name);
        ("description", `String s.description);
      ]
    ) knowledge.skills));
    ("last_updated", `Float knowledge.last_updated);
  ])

let tools_handler reqd =
  let all_tools = Tools.get_all_tools () in
  respond_json reqd (`List (List.map (fun (name, desc) ->
    `Assoc [
      ("name", `String name);
      ("description", `String desc);
    ]
  ) all_tools))

let task_create_handler server reqd =
  read_json_body reqd (fun json ->
    match Task_types.parse_create_request json with
    | Error err -> respond_json ~status:`Bad_request reqd (`Assoc [
        ("status", `String "error");
        ("error", `String err);
      ])
    | Ok req ->
        let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
        let result =
          Task_service.create_task server.task_service ~actor req
          |> Result.map Task_service.task_success_json
        in
        respond_task_result reqd result)

let tasks_list_handler server reqd =
  let request = Reqd.request reqd in
  let _path, query = split_target request.target in
  let status = query_param query "status" in
  let assignee = query_param query "assignee" in
  let kind = query_param query "kind" in
  let limit = Option.bind (query_param query "limit") int_of_string_opt in
  let cursor = Option.bind (query_param query "cursor") float_of_string_opt in
  let req = Task_types.parse_list_request ?status ?assignee ?kind ?limit ?cursor () in
  let result =
    Task_service.list_tasks server.task_service req
    |> Result.map Task_service.list_success_json
  in
  respond_task_result reqd result

let task_ready_handler server reqd =
  let request = Reqd.request reqd in
  let _path, query = split_target request.target in
  let limit = Option.bind (query_param query "limit") int_of_string_opt in
  let result =
    Task_service.ready_tasks server.task_service ?limit ()
    |> Result.map Task_service.list_success_json
  in
  respond_task_result reqd result

let task_get_handler server reqd id =
  let result =
    match Task_service.get_task server.task_service id with
    | Error e -> Error e
    | Ok None -> Error (Printf.sprintf "Task not found: %s" id)
    | Ok (Some task) -> Ok (Task_service.task_success_json task)
  in
  respond_task_result reqd result

let task_patch_handler server reqd id =
  read_json_body reqd (fun json ->
    match Task_types.parse_update_request json with
    | Error err -> respond_json ~status:`Bad_request reqd (`Assoc [
        ("status", `String "error");
        ("error", `String err);
      ])
    | Ok req ->
        let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
        let result =
          Task_service.update_task server.task_service ~id ~req ~actor
          |> Result.map Task_service.task_success_json
        in
        respond_task_result reqd result)

let task_claim_handler server reqd id =
  read_json_body reqd (fun json ->
    let assignee =
      match json with
      | `Assoc fields -> Task_types.opt_assoc_string fields "assignee"
      | _ -> None
    in
    let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
    let result =
      Task_service.claim_task server.task_service ~id ~assignee ~actor
      |> Result.map Task_service.task_success_json
    in
    respond_task_result reqd result)

let task_close_handler server reqd id =
  read_json_body reqd (fun json ->
    let reason = Task_types.parse_close_reason json in
    let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
    let result =
      Task_service.close_task server.task_service ~id ~reason ~actor
      |> Result.map Task_service.task_success_json
    in
    respond_task_result reqd result)

let task_cancel_handler server reqd id =
  read_json_body reqd (fun json ->
    let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
    let result =
      Task_service.cancel_task server.task_service ~id ~actor
      |> Result.map Task_service.task_success_json
    in
    respond_task_result reqd result)

let task_add_dependency_handler server reqd id =
  read_json_body reqd (fun json ->
    match Task_types.parse_dependency_request json with
    | Error err ->
        respond_json ~status:`Bad_request reqd (`Assoc [
          ("status", `String "error");
          ("error", `String err);
        ])
    | Ok (from_task_id, dep_type) ->
        let actor = Option.value ~default:"api" (Task_types.parse_actor json) in
        let result =
          Task_service.add_dependency server.task_service ~from_task_id ~to_task_id:id ~dep_type ~actor
          |> Result.map Task_service.dep_success_json
        in
        respond_task_result reqd result)

let task_dependencies_handler server reqd id =
  let result =
    Task_service.list_dependencies server.task_service id
    |> Result.map (Task_service.deps_success_json id)
  in
  respond_task_result reqd result

let task_events_handler server reqd id =
  let request = Reqd.request reqd in
  let _path, query = split_target request.target in
  let after_seq = Option.bind (query_param query "after_seq") int_of_string_opt in
  let limit = Option.bind (query_param query "limit") int_of_string_opt in
  let result =
    Task_service.list_events server.task_service id ?after_seq ?limit ()
    |> Result.map (Task_service.events_success_json id)
  in
  respond_task_result reqd result

let route_task_prefix_get server reqd =
  let request = Reqd.request reqd in
  let path, _query = split_target request.target in
  let prefix = "/api/tasks/" in
  if not (starts_with ~prefix path) then
    respond_json ~status:`Not_found reqd (`Assoc [("error", `String "Not found")])
  else
    let suffix = String.sub path (String.length prefix) (String.length path - String.length prefix) in
    let parts = suffix |> String.split_on_char '/' |> List.filter (fun s -> s <> "") in
    match parts with
    | [id] -> task_get_handler server reqd id
    | [id; "dependencies"] -> task_dependencies_handler server reqd id
    | [id; "events"] -> task_events_handler server reqd id
    | _ -> respond_json ~status:`Not_found reqd (`Assoc [("error", `String "Not found")])

let route_task_prefix_post server reqd =
  let request = Reqd.request reqd in
  let path, _query = split_target request.target in
  let prefix = "/api/tasks/" in
  if not (starts_with ~prefix path) then
    respond_json ~status:`Not_found reqd (`Assoc [("error", `String "Not found")])
  else
    let suffix = String.sub path (String.length prefix) (String.length path - String.length prefix) in
    let parts = suffix |> String.split_on_char '/' |> List.filter (fun s -> s <> "") in
    match parts with
    | [id; "claim"] -> task_claim_handler server reqd id
    | [id; "close"] -> task_close_handler server reqd id
    | [id; "cancel"] -> task_cancel_handler server reqd id
    | [id; "update"] -> task_patch_handler server reqd id
    | [id; "dependencies"] -> task_add_dependency_handler server reqd id
    | _ -> respond_json ~status:`Not_found reqd (`Assoc [("error", `String "Not found")])

(* {1 Server Creation and Lifecycle } *)

let create (config : config) =
  let http_server = Http_server.create
    ~host:config.host
    ~port:config.port
    ~max_connections:config.max_connections
    () in

  let task_store_cfg : Task_store.config = {
    db_path = config.tasks_db_path;
    busy_timeout_ms = config.tasks_busy_timeout_ms;
    default_limit = config.tasks_default_limit;
    max_limit = config.tasks_max_limit;
    event_retention_days = config.tasks_event_retention_days;
  } in
  let task_service = Task_service.create task_store_cfg in
  (match Task_service.initialize task_service with
   | Ok () -> ()
   | Error err -> failwith ("Failed to initialize task service: " ^ err));
  Task_service.set_default task_service;

  let agent = Agent.create
    ~llm_config:config.llm_config
    ~model:config.model
    ~temperature:0.7
    ~max_tokens:4096
    () in

  Log.info (fun m -> m "OClaw server created");

  {
    http_server;
    agent;
    task_service;
    config;
    running = false;
    mutex = Mutex.create ();
  }

let setup_routes server =
  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/health"
    health_handler;

  Http_server.add_route server.http_server
    ~method_:(Some `POST)
    ~match_type:Exact
    "/api/chat"
    (chat_handler server);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/sessions"
    (sessions_handler server.agent);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/knowledge"
    (knowledge_handler server.agent);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/tools"
    tools_handler;

  Http_server.add_route server.http_server
    ~method_:(Some `POST)
    ~match_type:Exact
    "/api/tasks"
    (task_create_handler server);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/tasks"
    (tasks_list_handler server);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Exact
    "/api/tasks/ready"
    (task_ready_handler server);

  Http_server.add_route server.http_server
    ~method_:(Some `GET)
    ~match_type:Prefix
    "/api/tasks/"
    (route_task_prefix_get server);

  Http_server.add_route server.http_server
    ~method_:(Some `POST)
    ~match_type:Prefix
    "/api/tasks/"
    (route_task_prefix_post server);

  Log.info (fun m -> m "Routes configured")

let start server =
  if server.running then
    Log.warn (fun m -> m "Server already running")
  else begin
    server.running <- true;
    setup_routes server;
    ignore (Http_server.start server.http_server);
    Log.info (fun m -> m "OClaw server started on http://%s:%d"
      server.config.host server.config.port)
  end

let stop server =
  if not server.running then
    Log.warn (fun m -> m "Server not running")
  else begin
    Log.info (fun m -> m "Stopping OClaw server");
    server.running <- false;
    Http_server.stop server.http_server
  end

let is_running server =
  server.running
