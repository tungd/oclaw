(** OClaw server - integrates HTTP server with agent and effect handlers *)

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
}

type t = {
  http_server : Http_server.t;
  agent : Agent.t;
  config : config;
  mutable running : bool;
  mutex : Mutex.t;
}

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
  let session_list = List.map (fun (id, info) ->
    `Assoc [
      ("id", `String id);
      ("created_at", `Float info.created_at);
      ("last_active", `Float info.last_active);
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

(* {1 Server Creation and Lifecycle } *)

let create (config : config) =
  let http_server = Http_server.create
    ~host:config.host
    ~port:config.port
    ~max_connections:config.max_connections
    () in

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
