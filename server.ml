(** OClaw server module *)

module Log = (val Logs.src_log (Logs.Src.create "server") : Logs.LOG)

(* OClaw server configuration *)
type config = {
  host : string;
  port : int;
  llm_config : Llm_provider.provider_config;
  telegram_enabled : bool;
  telegram_token : string;
  telegram_allow_from : int64 list;
  workspace_dir : string;
}

(* OClaw server *)
module OclawServer = struct
  type t = {
    http_server : Http_server.Server.t;
    session_manager : Session.Manager.t;
    knowledge : Knowledge.Knowledge.t;
    telegram_channel : Telegram_channel.Channel.t option;
    llm_config : Llm_provider.provider_config;
    mutable running : bool;
  }

  let create config =
    Log.info (fun m -> m "Creating OClaw server");

    (* Ensure workspace directory exists *)
    if not (Sys.file_exists config.workspace_dir) then begin
      Log.info (fun m -> m "Creating workspace directory: %s" config.workspace_dir);
      Unix.mkdir config.workspace_dir 0o755;
    end;

    (* Load knowledge from workspace *)
    let knowledge = Knowledge.Knowledge.load ~workspace_dir:config.workspace_dir in

    (* Create session manager *)
    let session_manager = Session.Manager.create () in

    (* Create HTTP server *)
    let http_server = Http_server.Server.create ~host:config.host ~port:config.port () in

    (* Create telegram channel if enabled *)
    let telegram_channel =
      if config.telegram_enabled then begin
        Log.info (fun m -> m "Creating Telegram channel");
        let bot = Telegram_api.Bot.create config.telegram_token in
        let telegram_config = {
          Telegram_channel.enabled = true;
          token = config.telegram_token;
          allow_from = config.telegram_allow_from;
        } in
        Some (Telegram_channel.Channel.create bot telegram_config session_manager knowledge config.llm_config)
      end else begin
        Log.info (fun m -> m "Telegram channel disabled");
        None
      end in

    {
      http_server;
      session_manager;
      knowledge;
      telegram_channel;
      llm_config = config.llm_config;
      running = false;
    }

  let start server =
    if server.running then begin
      Log.warn (fun m -> m "Server already running");
      exit 1
    end;

    Log.info (fun m -> m "Starting OClaw server");
    server.running <- true;

    (* Create API handler config *)
    let handler_config = {
      Api.llm_config = server.llm_config;
      knowledge = server.knowledge;
      session_manager = server.session_manager;
    } in

    (* Register API routes *)
    Api.register_routes server.http_server handler_config;

    (* Start HTTP server *)
    Http_server.Server.start server.http_server;
    Log.info (fun m -> m "HTTP server started on %s:%d"
      (let host, port = server.http_server |> Obj.magic |> fun _ -> ("", 0) in (* Placeholder *) host)
      (let host, port = server.http_server |> Obj.magic |> fun _ -> ("", 0) in (* Placeholder *) port));

    (* Start Telegram channel if configured *)
    Option.iter (fun ch ->
      Telegram_channel.Channel.start ch
    ) server.telegram_channel;

    Log.info (fun m -> m "OClaw server started successfully")

  let stop server =
    if not server.running then begin
      Log.warn (fun m -> m "Server not running");
      exit 1
    end;

    Log.info (fun m -> m "Stopping OClaw server");
    server.running <- false;

    (* Stop Telegram channel *)
    Option.iter (fun ch ->
      Telegram_channel.Channel.stop ch
    ) server.telegram_channel;

    (* Stop HTTP server *)
    Http_server.Server.stop server.http_server;

    Log.info (fun m -> m "OClaw server stopped")

  let is_running server =
    server.running

  let get_http_server server =
    server.http_server

  let get_session_manager server =
    server.session_manager

  let get_knowledge server =
    server.knowledge
end
