module Config = Agent_runtime.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)

type cli_options = {
  config_path : string option;
  persistent : bool;
  chat_id : int;
  use_acp : bool;
  debug : bool;
  export_chat : int option;
  export_output : string option;
}

let default_options () =
  {
    config_path = None;
    persistent = false;
    chat_id = 1;
    use_acp = false;
    debug = false;
    export_chat = None;
    export_output = None;
  }

let run_tui state chat_id persistent =
  Agent_tui.Tui.run ~state ~chat_id ~persistent;
  0

let run_acp state chat_id persistent =
  let frontend = Acp.Stdio_frontend.create () in
  let permission_requests : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let next_permission_id = ref 1000 in
  let id_key = function
    | `String value -> "s:" ^ value
    | `Int value -> "i:" ^ string_of_int value
    | `Null -> "null"
  in
  let fresh_permission_id () =
    let id = !next_permission_id in
    incr next_permission_id;
    `Int id
  in
  let send_runtime_message ?id message =
    let packet =
      match message with
      | Acp.Message.Request_permission _ ->
          let permission_id = Option.value id ~default:(fresh_permission_id ()) in
          let key = id_key permission_id in
          Hashtbl.replace permission_requests key chat_id;
          Acp.Message.to_jsonrpc ~id:permission_id message
      | Acp.Message.Agent_message _ | Acp.Message.Error _ ->
          Acp.Message.to_jsonrpc ?id message
      | _ ->
          Acp.Message.to_jsonrpc message
    in
    Acp.Stdio_frontend.send frontend packet
  in
  let rec loop () : int =
    match Acp.Stdio_frontend.recv frontend with
    | None -> loop ()
    | Some packet ->
        (match Acp.Message.of_jsonrpc_packet packet with
         | Some (Acp.Message.Initialize _) ->
             let id = match packet with Jsonrpc.Packet.Request r -> r.Jsonrpc.Request.id | _ -> `Int 0 in
             Acp.Stdio_frontend.send frontend (Acp.Message.to_jsonrpc ~id Acp.Message.Initialized);
             loop ()
         | Some (Acp.Message.Agent_message { content; _ }) ->
             let id = match packet with Jsonrpc.Packet.Request r -> r.Jsonrpc.Request.id | _ -> `Int 0 in
             let emit message = send_runtime_message ~id message in
             ignore (Agent_runtime.Session.process ~emit state ~chat_id ~persistent content);
             loop ()
         | _ ->
             begin
               match Acp.Message.permission_outcome_of_packet packet with
               | Some (id, outcome) ->
                   begin
                     match Hashtbl.find_opt permission_requests (id_key id) with
                     | Some pending_chat_id ->
                         Hashtbl.remove permission_requests (id_key id);
                         ignore (Agent_runtime.Session.resolve_permission state ~chat_id:pending_chat_id outcome);
                         loop ()
                     | None -> loop ()
                   end
               | None -> loop ()
             end)
  in
  loop ()

let () =
  let options = ref (default_options ()) in
  let config_args = ref [] in
  let spec = [
    ("--acp", Arg.Unit (fun () -> options := { !options with use_acp = true }), "Run in ACP JSON-RPC mode via stdio");
    ("--persistent", Arg.Unit (fun () -> options := { !options with persistent = true }), "Enable persistent chat history/memory");
    ("--chat-id", Arg.Int (fun value -> options := { !options with chat_id = value }), "Use this persistent chat/session id (default: 1)");
    ("--config", Arg.String (fun path -> options := { !options with config_path = Some path }), "Load configuration from this YAML file");
    ("--model", Arg.String (fun value -> config_args := !config_args @ ["--model"; value]), "Override the model name");
    ("--api-key", Arg.String (fun value -> config_args := !config_args @ ["--api-key"; value]), "Override the API key");
    ("--api-base", Arg.String (fun value -> config_args := !config_args @ ["--api-base"; value]), "Override the API base URL");
    ("--data-dir", Arg.String (fun value -> config_args := !config_args @ ["--data-dir"; value]), "Set the runtime data root");
    ("--max-tool-iterations", Arg.Int (fun value -> config_args := !config_args @ ["--max-tool-iterations"; string_of_int value]), "Set max tool iterations");
    ("--debug", Arg.Unit (fun () -> options := { !options with debug = true }), "Enable debug logging");
    ("--export", Arg.Int (fun value -> options := { !options with export_chat = Some value }), "<chat_id> Export chat to HTML");
    ("-o", Arg.String (fun value -> options := { !options with export_output = Some value }), "<path> Output path for export");
  ] in
  Arg.parse spec (fun _ -> ()) "Usage: oclaw [OPTIONS]";
  
  (* Load config with priority: file < env < args *)
  match Config.load ?config_file:!options.config_path ~cli_args:!config_args () with
  | Error errors ->
      List.iter (fun err -> prerr_endline ("Configuration error: " ^ Config.string_of_error err)) errors;
      exit 2
  | Ok config ->
      (* Setup logging - default to Info level, Debug if --debug flag is set *)
      let log_level = if !options.debug || config.debug then Logs.Debug else Logs.Info in
      Logs.set_level (Some log_level);
      Logs.set_reporter (Logs.reporter ());
      match Config.validate config with
      | Error errors ->
          List.iter (fun err -> prerr_endline ("Configuration error: " ^ err)) errors;
          exit 2
      | Ok config ->
          begin
            match Agent_runtime.App.create config with
            | Error err ->
                prerr_endline ("OClaw error: " ^ err);
                exit 1
            | Ok state ->
                (* Handle export mode *)
                (match !options.export_chat with
                | Some chat_id ->
                    let out_path = Option.value ~default:(Printf.sprintf "transcript_%d.html" chat_id) !options.export_output in
                    Agent_runtime.Export.html state ~chat_id ~out_path;
                    Printf.printf "Exported to %s\n" out_path;
                    Agent_runtime.App.close state;
                    exit 0
                | None ->
                    let exit_code =
                      if !options.use_acp then
                        run_acp state !options.chat_id !options.persistent
                      else
                        run_tui state !options.chat_id !options.persistent
                    in
                    Log.info (fun m -> m "OClaw exiting with code %d" exit_code);
                    exit exit_code)
          end
