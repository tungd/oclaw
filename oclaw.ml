module Config = Oclaw_config.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)
module LogColor = Log_color
module LNoise = LNoise

type cli_overrides = {
  mutable config_path : string option;
  mutable single_shot : bool;
  mutable persistent : bool;
  mutable chat_id : int;
  mutable model : string option;
  mutable api_key : string option;
  mutable api_base : string option;
  mutable timeout : int option;
  mutable data_dir : string option;
  mutable skills_dir : string option;
  mutable workspace : string option;
  mutable allow_read_paths : string list;
  mutable allow_write_paths : string list;
  mutable no_workspace_restriction : bool;
  mutable exec_timeout_seconds : int option;
  mutable debug : bool;
}

let default_overrides () =
  {
    config_path = None;
    single_shot = false;
    persistent = false;
    chat_id = 1;
    model = None;
    api_key = None;
    api_base = None;
    timeout = None;
    data_dir = None;
    skills_dir = None;
    workspace = None;
    allow_read_paths = [];
    allow_write_paths = [];
    no_workspace_restriction = false;
    exec_timeout_seconds = None;
    debug = false;
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

let apply_cli_overrides (config : Config.config) (overrides : cli_overrides) =
  {
    config with
    llm_model = Option.value ~default:config.llm_model overrides.model;
    llm_api_key = Option.value ~default:config.llm_api_key overrides.api_key;
    llm_api_base = Option.value ~default:config.llm_api_base overrides.api_base;
    llm_timeout = Option.value ~default:config.llm_timeout overrides.timeout;
    data_dir = Option.value ~default:config.data_dir overrides.data_dir;
    skills_dir = Option.value ~default:config.skills_dir overrides.skills_dir;
    tools_workspace = Option.value ~default:config.tools_workspace overrides.workspace;
    tools_allow_read_paths =
      if overrides.allow_read_paths = [] then config.tools_allow_read_paths
      else overrides.allow_read_paths;
    tools_allow_write_paths =
      if overrides.allow_write_paths = [] then config.tools_allow_write_paths
      else overrides.allow_write_paths;
    tools_restrict_to_workspace =
      if overrides.no_workspace_restriction then false else config.tools_restrict_to_workspace;
    tools_exec_timeout_seconds =
      Option.value ~default:config.tools_exec_timeout_seconds overrides.exec_timeout_seconds;
    debug = config.debug || overrides.debug;
  }

let sandbox_config_of_config (config : Config.config) =
  {
    Tools.workspace_root = config.tools_workspace;
    restrict_to_workspace = config.tools_restrict_to_workspace;
    allow_read_paths = config.tools_allow_read_paths;
    allow_write_paths = config.tools_allow_write_paths;
    exec_timeout_seconds = config.tools_exec_timeout_seconds;
    exec_enable_deny_patterns = config.tools_exec_enable_deny_patterns;
    exec_custom_deny_patterns = config.tools_exec_custom_deny_patterns;
    exec_custom_allow_patterns = config.tools_exec_custom_allow_patterns;
  }

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
  (* Setup linenoise with history file *)

  let history_file = Filename.concat (Filename.concat state.Runtime.config.data_dir "runtime") "linenoise_history" in
  (match LNoise.history_set ~max_length:1000 with Error e -> Log.warn (fun m -> m "Failed to set history length: %s" e) | Ok () -> ());
  (match LNoise.history_load ~filename:history_file with Error e -> Log.debug (fun m -> m "No history file yet: %s" e) | Ok () -> ());
  LNoise.set_multiline true;
  
  (* ANSI color codes *)
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
              (* Save history before exiting *)
              (match LNoise.history_save ~filename:history_file with Error e -> Log.warn (fun m -> m "Failed to save history: %s" e) | Ok () -> ());
              print_endline "Goodbye!";
              0
            ) else (
              (* Add to history if not empty *)
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

let () =
  let overrides = default_overrides () in
  let prompt_parts = ref [] in
  let spec = [
    ("--single-shot", Arg.Unit (fun () -> overrides.single_shot <- true), "Run one prompt and exit");
    ("--persistent", Arg.Unit (fun () -> overrides.persistent <- true), "Enable persistent chat history/memory");
    ("--chat-id", Arg.Int (fun value -> overrides.chat_id <- value), "Use this persistent chat/session id (default: 1)");
    ("--config", Arg.String (fun path -> overrides.config_path <- Some path), "Load configuration from this YAML file");
    ("--model", Arg.String (fun value -> overrides.model <- Some value), "Override the model name");
    ("--api-key", Arg.String (fun value -> overrides.api_key <- Some value), "Override the API key");
    ("--api-base", Arg.String (fun value -> overrides.api_base <- Some value), "Override the API base URL");
    ("--timeout", Arg.Int (fun value -> overrides.timeout <- Some value), "Override HTTP timeout in seconds");
    ("--data-dir", Arg.String (fun value -> overrides.data_dir <- Some value), "Set the runtime data root (default: workspace)");
    ("--skills-dir", Arg.String (fun value -> overrides.skills_dir <- Some value), "Override the skills directory");
    ("--workspace", Arg.String (fun value -> overrides.workspace <- Some value), "Set the tools workspace root");
    ("--allow-read-path", Arg.String (fun value -> overrides.allow_read_paths <- overrides.allow_read_paths @ [value]), "Allow reads outside the workspace for this path");
    ("--allow-write-path", Arg.String (fun value -> overrides.allow_write_paths <- overrides.allow_write_paths @ [value]), "Allow writes outside the workspace for this path");
    ("--no-workspace-restriction", Arg.Unit (fun () -> overrides.no_workspace_restriction <- true), "Disable workspace path restriction for tools");
    ("--exec-timeout", Arg.Int (fun value -> overrides.exec_timeout_seconds <- Some value), "Set shell tool timeout in seconds");
    ("--debug", Arg.Unit (fun () -> overrides.debug <- true), "Enable debug logging");
  ] in
  Arg.parse spec (fun arg -> prompt_parts := !prompt_parts @ [arg]) "Usage: oclaw [--single-shot] [prompt]";
  (* Setup logging - default to Info level, Debug if --debug flag is set *)
  let log_level = if overrides.debug then Logs.Debug else Logs.Info in
  LogColor.setup_auto ~level:(Some log_level) ~format_time:true ();
  (* Set level for all known log sources to ensure consistency *)
  Logs.set_level (Some log_level);
  let config_path = Option.value ~default:"config.yaml" overrides.config_path in
  let config =
    let base =
      if Sys.file_exists config_path then Config.load_config config_path
      else Config.default_config
    in
    base
    |> Config.apply_env_overrides
    |> fun cfg -> apply_cli_overrides cfg overrides
  in
  if config.debug then (
    LogColor.setup_auto ~level:(Some Logs.Debug) ~format_time:true ();
    Logs.set_level (Some Logs.Debug)
  );
  match Config.validate_config config with
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
            let positional_prompt = String.concat " " !prompt_parts in
            let exit_code =
              if overrides.single_shot || positional_prompt <> "" then
                let prompt =
                  if positional_prompt <> "" then positional_prompt
                  else read_stdin ()
                in
                if String.trim prompt = "" then 0 else run_single_shot state overrides.chat_id overrides.persistent prompt
              else
                run_repl state overrides.chat_id overrides.persistent
            in
            Log.info (fun m -> m "OClaw exiting with code %d" exit_code);
            exit exit_code
      end
