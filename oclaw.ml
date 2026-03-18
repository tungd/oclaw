module Config = Oclaw_config.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)

type cli_options = {
  config_path : string option;
  single_shot : bool;
  persistent : bool;
  chat_id : int;
  use_tui : bool;
  use_acp : bool;
  debug : bool;
  prompt_parts : string list;
}

let default_options () =
  {
    config_path = None;
    single_shot = false;
    persistent = false;
    chat_id = 1;
    use_tui = false;
    use_acp = false;
    debug = false;
    prompt_parts = [];
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

let run_repl state chat_id persistent =
  let scheduler_handle = Scheduler.start state in
  let history_file = Filename.concat (Filename.concat state.Runtime.config.data_dir "runtime") "linenoise_history" in
  (match LNoise.history_set ~max_length:1000 with Error e -> Log.warn (fun m -> m "Failed to set history length: %s" e) | Ok () -> ());
  (match LNoise.history_load ~filename:history_file with Error e -> Log.debug (fun m -> m "No history file yet: %s" e) | Ok () -> ());
  LNoise.set_multiline true;
  
  let ansi_reset = "\027[0m" in
  let ansi_bold = "\027[1m" in
  let ansi_orange = "\027[38;5;208m" in
  let repl_prompt = ansi_bold ^ ansi_orange ^ "┃" ^ ansi_reset ^ " " in
  
  print_endline "OClaw REPL";
  print_endline "Type /exit or /quit to quit.";
  print_endline "";
  
  Fun.protect
    ~finally:(fun () ->
      Option.iter Scheduler.stop scheduler_handle)
    (fun () ->
      let rec loop () =
        match LNoise.linenoise repl_prompt with
        | None ->
            print_endline "\nGoodbye!";
            0
        | Some input ->
            let trimmed = String.trim input in
            if trimmed = "" then
              loop ()
            else if trimmed = "/exit" || trimmed = "/quit" then (
              (match LNoise.history_save ~filename:history_file with Error e -> Log.warn (fun m -> m "Failed to save history: %s" e) | Ok () -> ());
              print_endline "Goodbye!";
              0
            ) else (
              (match LNoise.history_add trimmed with Error e -> Log.debug (fun m -> m "Failed to add to history: %s" e) | Ok () -> ());

              let streamed = ref false in
              let on_text_delta delta =
                if not !streamed then (
                  streamed := true;
                  print_string "\r\027[K"
                );
                print_string delta;
                flush stdout
              in
              match Agent_engine.process ~on_text_delta state ~chat_id ~persistent input with
              | Ok response ->
                  if !streamed then print_newline ()
                  else print_endline response;
                  print_endline "";
                  loop ()
              | Error err ->
                  if !streamed then print_newline ();
                  prerr_endline ("OClaw error: " ^ err);
                  print_endline "";
                  loop ()
            )
      in
      loop ())

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
    ("--tui", Arg.Unit (fun () -> options := { !options with use_tui = true }), "Run with TUI interface");
    ("--acp", Arg.Unit (fun () -> options := { !options with use_acp = true }), "Run in ACP JSON-RPC mode via stdio");
    ("--single-shot", Arg.Unit (fun () -> options := { !options with single_shot = true }), "Run one prompt and exit");
    ("--persistent", Arg.Unit (fun () -> options := { !options with persistent = true }), "Enable persistent chat history/memory");
    ("--chat-id", Arg.Int (fun value -> options := { !options with chat_id = value }), "Use this persistent chat/session id (default: 1)");
    ("--config", Arg.String (fun path -> options := { !options with config_path = Some path }), "Load configuration from this YAML file");
    ("--model", Arg.String (fun value -> config_args := !config_args @ ["--model"; value]), "Override the model name");
    ("--api-key", Arg.String (fun value -> config_args := !config_args @ ["--api-key"; value]), "Override the API key");
    ("--api-base", Arg.String (fun value -> config_args := !config_args @ ["--api-base"; value]), "Override the API base URL");
    ("--data-dir", Arg.String (fun value -> config_args := !config_args @ ["--data-dir"; value]), "Set the runtime data root");
    ("--max-tool-iterations", Arg.Int (fun value -> config_args := !config_args @ ["--max-tool-iterations"; string_of_int value]), "Set max tool iterations");
    ("--debug", Arg.Unit (fun () -> options := { !options with debug = true }), "Enable debug logging");
  ] in
  Arg.parse spec (fun arg -> options := { !options with prompt_parts = !options.prompt_parts @ [arg] }) "Usage: oclaw [--single-shot] [prompt]";
  
  (* Load config with priority: file < env < args *)
  let config = Config.load ?config_file:!options.config_path ~cli_args:!config_args () in
  
  (* Setup logging - default to Info level, Debug if --debug flag is set *)
  let log_level = if !options.debug || config.debug then Logs.Debug else Logs.Info in
  LogColor.setup_auto ~level:(Some log_level) ~format_time:true ();
  Logs.set_level (Some log_level);
  
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
            let positional_prompt = String.concat " " !options.prompt_parts in
            let exit_code =
              if !options.use_tui then
                run_tui state !options.chat_id !options.persistent
              else if !options.use_acp then
                run_acp state !options.chat_id !options.persistent
              else if !options.single_shot || positional_prompt <> "" then
                let prompt =
                  if positional_prompt <> "" then positional_prompt
                  else read_stdin ()
                in
                if String.trim prompt = "" then 0 else run_single_shot state !options.chat_id !options.persistent prompt
              else
                run_repl state !options.chat_id !options.persistent
            in
            Log.info (fun m -> m "OClaw exiting with code %d" exit_code);
            exit exit_code
      end
