(** ACP server for OClaw.

    The transport is custom TCP framing, while the messages follow ACP JSON-RPC. *)

open Yojson.Safe.Util

module Log = (val Logs.src_log (Logs.Src.create "acp_server") : Logs.LOG)
module Transport = Acp_transport

type config = {
  host : string;
  port : int;
  llm_config : Llm_provider.provider_config;
  model : string;
  tasks_db_path : string;
  tasks_default_limit : int;
  tasks_max_limit : int;
  tasks_busy_timeout_ms : int;
  tasks_event_retention_days : int;
}

type prompt_state = {
  mutable cancelled : bool;
  mutex : Mutex.t;
}

type session = {
  id : string;
  cwd : string;
  mutable prompt_state : prompt_state option;
  mutex : Mutex.t;
}

type client = {
  transport : Transport.connection;
  mutable initialized : bool;
}

type t = {
  config : config;
  agent : Agent.t;
  task_service : Task_service.t;
  sessions : (string, session) Hashtbl.t;
  sessions_mutex : Mutex.t;
  mutable listener : Unix.file_descr option;
  mutable acceptor : unit Domain.t option;
  mutable running : bool;
}

let jsonrpc_result id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ]

let jsonrpc_error ?(data=`Null) id code message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
      ("data", data);
    ]);
  ]

let session_notification session_id update =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "session/update");
    ("params", `Assoc [
      ("sessionId", `String session_id);
      ("update", update);
    ]);
  ]

let text_content text =
  `Assoc [
    ("type", `String "text");
    ("text", `String text);
  ]

let send_json client json =
  try Transport.send_json client.transport json
  with exn ->
    Log.warn (fun m -> m "Failed to send ACP message: %s" (Printexc.to_string exn))

let send_error client id code message =
  send_json client (jsonrpc_error id code message)

let make_session_id () =
  Random.self_init ();
  Printf.sprintf "sess_%08x%08x" (Random.bits ()) (Random.bits ())

let with_sessions_lock server f =
  Mutex.lock server.sessions_mutex;
  try
    let value = f () in
    Mutex.unlock server.sessions_mutex;
    value
  with exn ->
    Mutex.unlock server.sessions_mutex;
    raise exn

let find_session server session_id =
  with_sessions_lock server (fun () -> Hashtbl.find_opt server.sessions session_id)

let create_session server cwd =
  let session = {
    id = make_session_id ();
    cwd;
    prompt_state = None;
    mutex = Mutex.create ();
  } in
  with_sessions_lock server (fun () -> Hashtbl.replace server.sessions session.id session);
  session

let start_prompt session =
  Mutex.lock session.mutex;
  let result =
    match session.prompt_state with
    | Some _ -> Error "A prompt is already running for this session"
    | None ->
        let state = { cancelled = false; mutex = Mutex.create () } in
        session.prompt_state <- Some state;
        Ok state
  in
  Mutex.unlock session.mutex;
  result

let finish_prompt session state =
  Mutex.lock session.mutex;
  begin
    match session.prompt_state with
    | Some active when active == state -> session.prompt_state <- None
    | _ -> ()
  end;
  Mutex.unlock session.mutex

let cancel_prompt session =
  Mutex.lock session.mutex;
  let result =
    match session.prompt_state with
    | None -> false
    | Some state ->
        Mutex.lock state.mutex;
        state.cancelled <- true;
        Mutex.unlock state.mutex;
        true
  in
  Mutex.unlock session.mutex;
  result

let prompt_cancelled (state : prompt_state) =
  Mutex.lock state.mutex;
  let cancelled = state.cancelled in
  Mutex.unlock state.mutex;
  cancelled

let json_list = function
  | `List items -> items
  | _ -> []

let is_absolute_directory path =
  (not (Filename.is_relative path))
  &&
  try (Unix.stat path).Unix.st_kind = Unix.S_DIR
  with _ -> false

let prompt_text_of_block = function
  | `Assoc _ as block ->
      begin
        match block |> member "type" with
        | `String "text" ->
            block |> member "text" |> to_string_option
        | `String "resource_link" ->
            let name = block |> member "name" |> to_string_option |> Option.value ~default:"resource" in
            let uri = block |> member "uri" |> to_string_option |> Option.value ~default:"" in
            Some (Printf.sprintf "Resource: %s (%s)" name uri)
        | _ -> None
      end
  | _ -> None

let prompt_text params =
  params
  |> member "prompt"
  |> json_list
  |> List.filter_map prompt_text_of_block
  |> String.concat "\n\n"

let process_prompt server client ~request_id ~session ~state ~content =
  Fun.protect
    ~finally:(fun () -> finish_prompt session state)
    (fun () ->
      if prompt_cancelled state then
        send_json client (jsonrpc_result request_id (`Assoc [("stopReason", `String "cancelled")]))
      else
        let stop_reason =
          match Agent.process_query server.agent ~session_id:session.id ~content with
          | Ok response ->
              if prompt_cancelled state then
                "cancelled"
              else (
                send_json client
                  (session_notification session.id
                     (`Assoc [
                       ("sessionUpdate", `String "agent_message_chunk");
                       ("content", text_content response);
                     ]));
                "end_turn"
              )
          | Error err ->
              if prompt_cancelled state then
                "cancelled"
              else (
                send_json client
                  (session_notification session.id
                     (`Assoc [
                       ("sessionUpdate", `String "agent_message_chunk");
                       ("content", text_content ("Error: " ^ err));
                     ]));
                "refusal"
              )
        in
        send_json client (jsonrpc_result request_id (`Assoc [("stopReason", `String stop_reason)])))

let handle_initialize client id =
  client.initialized <- true;
  send_json client
    (jsonrpc_result id (`Assoc [
      ("protocolVersion", `Int 1);
      ("agentCapabilities", `Assoc [
        ("loadSession", `Bool false);
        ("mcpCapabilities", `Assoc [
          ("http", `Bool false);
          ("sse", `Bool false);
        ]);
        ("promptCapabilities", `Assoc [
          ("audio", `Bool false);
          ("embeddedContext", `Bool false);
          ("image", `Bool false);
        ]);
        ("sessionCapabilities", `Assoc []);
      ]);
      ("agentInfo", `Assoc [
        ("name", `String "oclaw");
        ("version", `String "1.0.0");
      ]);
      ("authMethods", `List []);
    ]))

let require_initialized client id =
  if client.initialized then Ok ()
  else (
    send_error client id (-32002) "Connection must be initialized first";
    Error ())

let handle_session_new server client id params =
  match require_initialized client id with
  | Error () -> ()
  | Ok () ->
      let cwd = params |> member "cwd" |> to_string_option in
      begin
        match cwd with
        | None -> send_error client id (-32602) "session/new requires cwd"
        | Some _ when json_list (params |> member "mcpServers") <> [] ->
            send_error client id (-32602) "MCP servers are not supported by this agent yet"
        | Some cwd when not (is_absolute_directory cwd) ->
            send_error client id (-32602) "cwd must be an absolute existing directory"
        | Some cwd ->
            let session = create_session server cwd in
            send_json client (jsonrpc_result id (`Assoc [("sessionId", `String session.id)]))
      end

let handle_session_prompt server client id params =
  match require_initialized client id with
  | Error () -> ()
  | Ok () ->
      begin
        match params |> member "sessionId" |> to_string_option with
        | None -> send_error client id (-32602) "session/prompt requires sessionId"
        | Some session_id ->
            begin
              match find_session server session_id with
              | None -> send_error client id (-32602) "Unknown sessionId"
              | Some session ->
                  let content = prompt_text params in
                  match start_prompt session with
                  | Error err -> send_error client id (-32003) err
                  | Ok state ->
                      ignore (Domain.spawn (fun () ->
                        process_prompt server client ~request_id:id ~session ~state ~content))
            end
      end

let handle_session_cancel server client id_opt params =
  let session_id = params |> member "sessionId" |> to_string_option in
  let result =
    match session_id with
    | None -> Error "session/cancel requires sessionId"
    | Some session_id ->
        begin
          match find_session server session_id with
          | None -> Error "Unknown sessionId"
          | Some session ->
              ignore (cancel_prompt session);
              Ok ()
        end
  in
  match id_opt with
  | None -> ()
  | Some id ->
      begin
        match result with
        | Ok () -> send_json client (jsonrpc_result id (`Assoc []))
        | Error err -> send_error client id (-32602) err
      end

let handle_request server client msg =
  let id_opt =
    match msg |> member "id" with
    | `Null -> None
    | (`Int _ | `Intlit _ | `String _) as id -> Some id
    | _ -> None
  in
  match msg |> member "method" with
  | `String "initialize" ->
      begin
        match id_opt with
        | Some id -> handle_initialize client id
        | None -> send_error client `Null (-32600) "initialize must be a request"
      end
  | `String "session/new" ->
      begin
        match id_opt with
        | Some id -> handle_session_new server client id (msg |> member "params")
        | None -> send_error client `Null (-32600) "session/new must be a request"
      end
  | `String "session/prompt" ->
      begin
        match id_opt with
        | Some id -> handle_session_prompt server client id (msg |> member "params")
        | None -> send_error client `Null (-32600) "session/prompt must be a request"
      end
  | `String "session/cancel" ->
      handle_session_cancel server client id_opt (msg |> member "params")
  | `String method_name ->
      let id = Option.value ~default:`Null id_opt in
      send_error client id (-32601) ("Unsupported ACP method: " ^ method_name)
  | _ ->
      let id = Option.value ~default:`Null id_opt in
      send_error client id (-32600) "Invalid ACP request"

let handle_client server client_fd =
  let client = {
    transport = Transport.create_connection client_fd;
    initialized = false;
  } in
  Fun.protect
    ~finally:(fun () -> Transport.close client.transport)
    (fun () ->
      let rec loop () =
        if server.running then
          try
            let msg = Transport.recv_json client.transport in
            handle_request server client msg;
            loop ()
          with
          | End_of_file -> ()
          | Yojson.Json_error err ->
              send_error client `Null (-32700) ("Invalid JSON: " ^ err);
              loop ()
          | exn ->
              Log.err (fun m -> m "ACP client handler failed: %s" (Printexc.to_string exn));
              ()
      in
      loop ())

let accept_loop server listener =
  while server.running do
    try
      let client_fd, _ = Unix.accept listener in
      ignore (Domain.spawn (fun () -> handle_client server client_fd))
    with
    | Unix.Unix_error ((Unix.EBADF | Unix.EINVAL | Unix.ECONNABORTED), _, _) -> ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | exn ->
        Log.err (fun m -> m "ACP accept loop failed: %s" (Printexc.to_string exn))
  done

let create config =
  let task_store_cfg = {
    Task_store.db_path = config.tasks_db_path;
    default_limit = config.tasks_default_limit;
    max_limit = config.tasks_max_limit;
    busy_timeout_ms = config.tasks_busy_timeout_ms;
    event_retention_days = config.tasks_event_retention_days;
  } in
  let task_service = Task_service.create task_store_cfg in
  begin
    match Task_service.initialize task_service with
    | Ok () -> ()
    | Error err -> failwith err
  end;
  Task_service.set_default task_service;
  {
    config;
    agent =
      Agent.create
        ~llm_config:config.llm_config
        ~model:config.model
        ();
    task_service;
    sessions = Hashtbl.create 32;
    sessions_mutex = Mutex.create ();
    listener = None;
    acceptor = None;
    running = false;
  }

let start server =
  if server.running then ()
  else (
    let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt listener Unix.SO_REUSEADDR true;
    Unix.bind listener (Unix.ADDR_INET (Transport.resolve_host server.config.host, server.config.port));
    Unix.listen listener 16;
    server.listener <- Some listener;
    server.running <- true;
    server.acceptor <- Some (Domain.spawn (fun () -> accept_loop server listener));
    Log.info (fun m ->
      m "ACP server listening on %s"
        (Transport.endpoint_to_string { Transport.host = server.config.host; port = server.config.port })))

let stop server =
  if server.running then (
    server.running <- false;
    (match server.listener with
     | Some listener ->
         server.listener <- None;
         (try Unix.close listener with _ -> ())
     | None -> ());
    server.acceptor <- None;
    Log.info (fun m -> m "ACP server stopped")
  )

let is_running server =
  server.running
