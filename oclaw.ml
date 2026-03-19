module Config = Oclaw_config.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)

type cli_options = {
  config_path : string option;
  single_shot : bool;
  persistent : bool;
  chat_id : int;
  use_acp : bool;
  debug : bool;
  prompt_parts : string list;
  export_chat : int option;
  export_output : string option;
}

let default_options () =
  {
    config_path = None;
    single_shot = false;
    persistent = false;
    chat_id = 1;
    use_acp = false;
    debug = false;
    prompt_parts = [];
    export_chat = None;
    export_output = None;
  }

let read_stdin () =
  let lines = ref [] in
  try
    while true do
      lines := read_line () :: !lines
    done;
    assert false
  with End_of_file ->
    String.concat "\n" (List.rev !lines)

let run_single_shot state chat_id persistent prompt =
  let streamed = ref false in
  let on_text_delta delta =
    if not !streamed then (
      streamed := true;
      print_string "\r\027[K"
    );
    print_string delta;
    flush stdout
  in
  match Agent_engine.process ~on_text_delta state ~chat_id ~persistent prompt with
  | Ok response ->
      if !streamed then print_newline ()
      else print_endline response;
      0
  | Error err ->
      if !streamed then print_newline ();
      prerr_endline ("OClaw error: " ^ err);
      1

let run_tui state chat_id persistent =
  Tui.run ~state ~chat_id ~persistent;
  0

let run_acp state chat_id persistent =
  let frontend = Acp.Stdio_frontend.create () in
  let rec loop () =
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
             let on_text_delta delta =
               Acp.Stdio_frontend.send frontend (Acp.Message.to_jsonrpc (Acp.Message.Agent_delta { content = delta }))
             in
             (match Agent_engine.process ~on_text_delta state ~chat_id ~persistent content with
              | Ok response ->
                  Acp.Stdio_frontend.send frontend (Acp.Message.to_jsonrpc ~id (Acp.Message.Agent_message { content = response; chat_id = Some chat_id }));
                  Acp.Stdio_frontend.send frontend (Acp.Message.to_jsonrpc (Acp.Message.Done));
              | Error err ->
                  Acp.Stdio_frontend.send frontend (Acp.Message.to_jsonrpc ~id (Acp.Message.Error { message = err; code = 0 })))
             ; loop ()
         | _ -> loop ())
  in
  loop ();
  0

let () =
  let options = ref (default_options ()) in
  let config_args = ref [] in
  let spec = [
    ("--acp", Arg.Unit (fun () -> options := { !options with use_acp = true }), "Run in ACP JSON-RPC mode via stdio");
    ("--single-shot", Arg.Unit (fun () -> options := { !options with single_shot = true }), "Run one prompt and exit (default: TUI interactive mode)");
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
  Arg.parse spec (fun arg -> options := { !options with prompt_parts = !options.prompt_parts @ [arg] }) "Usage: oclaw [OPTIONS] [prompt]";
  
  (* Load config with priority: file < env < args *)
  let config = Config.load ?config_file:!options.config_path ~cli_args:!config_args () in
  
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
        match Runtime.create_app_state config with
        | Error err ->
            prerr_endline ("OClaw error: " ^ err);
            exit 1
        | Ok state ->
            (* Handle export mode *)
            (match !options.export_chat with
            | Some chat_id ->
                let out_path = Option.value ~default:(Printf.sprintf "transcript_%d.html" chat_id) !options.export_output in
                Transcript.export_html state.Runtime.transcript ~chat_id ~out_path;
                Printf.printf "Exported to %s\n" out_path;
                Runtime.close_app_state state;
                exit 0
            | None ->
                let positional_prompt = String.concat " " !options.prompt_parts in
                let exit_code =
                  if !options.use_acp then
                    run_acp state !options.chat_id !options.persistent
                  else if !options.single_shot || positional_prompt <> "" then
                    let prompt =
                      if positional_prompt <> "" then positional_prompt
                      else read_stdin ()
                    in
                    if String.trim prompt = "" then 0 else run_single_shot state !options.chat_id !options.persistent prompt
                  else
                    run_tui state !options.chat_id !options.persistent
                in
                Log.info (fun m -> m "OClaw exiting with code %d" exit_code);
                exit exit_code)
      end
