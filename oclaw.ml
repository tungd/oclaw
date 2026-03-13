module Config = Oclaw_config.Config
module Log = (val Logs.src_log Logs.default : Logs.LOG)
module LogColor = Log_color

type cli_overrides = {
  mutable config_path : string option;
  mutable single_shot : bool;
  mutable model : string option;
  mutable api_key : string option;
  mutable api_base : string option;
  mutable temperature : float option;
  mutable max_tokens : int option;
  mutable timeout : int option;
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
    model = None;
    api_key = None;
    api_base = None;
    temperature = None;
    max_tokens = None;
    timeout = None;
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
    llm_temperature = Option.value ~default:config.llm_temperature overrides.temperature;
    llm_max_tokens = Option.value ~default:config.llm_max_tokens overrides.max_tokens;
    llm_timeout = Option.value ~default:config.llm_timeout overrides.timeout;
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

let run_single_shot assistant prompt =
  match Assistant_runtime.query assistant prompt with
  | Ok response ->
      print_endline response;
      0
  | Error err ->
      prerr_endline ("OClaw error: " ^ err);
      1

let run_repl assistant =
  print_endline "OClaw REPL";
  print_endline "Type /exit to quit.";
  let rec loop () =
    print_string "> ";
    flush stdout;
    match read_line () with
    | exception End_of_file ->
        print_endline "";
        0
    | line ->
        let input = String.trim line in
        if input = "" then loop ()
        else if input = "/exit" || input = "/quit" then 0
        else (
          match Assistant_runtime.query assistant input with
          | Ok response ->
              print_endline response;
              loop ()
          | Error err ->
              prerr_endline ("OClaw error: " ^ err);
              loop ()
        )
  in
  loop ()

let () =
  let overrides = default_overrides () in
  let prompt_parts = ref [] in
  let spec = [
    ("--single-shot", Arg.Unit (fun () -> overrides.single_shot <- true), "Run one prompt and exit");
    ("--config", Arg.String (fun path -> overrides.config_path <- Some path), "Load configuration from this YAML file");
    ("--model", Arg.String (fun value -> overrides.model <- Some value), "Override the model name");
    ("--api-key", Arg.String (fun value -> overrides.api_key <- Some value), "Override the API key");
    ("--api-base", Arg.String (fun value -> overrides.api_base <- Some value), "Override the API base URL");
    ("--temperature", Arg.Float (fun value -> overrides.temperature <- Some value), "Override sampling temperature");
    ("--max-tokens", Arg.Int (fun value -> overrides.max_tokens <- Some value), "Override max completion tokens");
    ("--timeout", Arg.Int (fun value -> overrides.timeout <- Some value), "Override HTTP timeout in seconds");
    ("--workspace", Arg.String (fun value -> overrides.workspace <- Some value), "Set the tools workspace root");
    ("--allow-read-path", Arg.String (fun value -> overrides.allow_read_paths <- overrides.allow_read_paths @ [value]), "Allow reads outside the workspace for this path");
    ("--allow-write-path", Arg.String (fun value -> overrides.allow_write_paths <- overrides.allow_write_paths @ [value]), "Allow writes outside the workspace for this path");
    ("--no-workspace-restriction", Arg.Unit (fun () -> overrides.no_workspace_restriction <- true), "Disable workspace path restriction for tools");
    ("--exec-timeout", Arg.Int (fun value -> overrides.exec_timeout_seconds <- Some value), "Set shell tool timeout in seconds");
    ("--debug", Arg.Unit (fun () -> overrides.debug <- true), "Enable debug logging");
  ] in
  Arg.parse spec (fun arg -> prompt_parts := !prompt_parts @ [arg]) "Usage: oclaw [--single-shot] [prompt]";
  LogColor.setup_auto ~level:(Some Logs.Info) ~format_time:true ();
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
  if config.debug then LogColor.setup_auto ~level:(Some Logs.Debug) ~format_time:true ();
  match Config.validate_config config with
  | Error errors ->
      List.iter (fun err -> prerr_endline ("Configuration error: " ^ err)) errors;
      exit 2
  | Ok config ->
      Tools.init_default_tools ~sandbox_config:(sandbox_config_of_config config) ();
      let provider = Config.to_llm_provider_config config in
      let assistant = Assistant_runtime.create ~provider_config:provider () in
      let positional_prompt = String.concat " " !prompt_parts in
      let exit_code =
        if overrides.single_shot || positional_prompt <> "" then
          let prompt =
            if positional_prompt <> "" then positional_prompt
            else read_stdin ()
          in
          if String.trim prompt = "" then 0 else run_single_shot assistant prompt
        else
          run_repl assistant
      in
      Log.info (fun m -> m "OClaw exiting with code %d" exit_code);
      exit exit_code
