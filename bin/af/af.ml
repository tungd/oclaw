module Config = Agent_runtime.Config

let trim = String.trim

type mode =
  | Interactive of string
  | Batch of string * string

type cli_options = {
  config_path : string option;
  debug : bool;
  data_dir_explicit : bool;
}

let default_options () =
  {
    config_path = None;
    debug = false;
    data_dir_explicit = false;
  }

let file_dir path =
  let absolute =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  Filename.dirname absolute

let load_agent path =
  match Af_agent.load ~path with
  | Ok agent -> Ok agent
  | Error err -> Error ("Agent file error: " ^ err)

let build_app ?llm_call options config_args path =
  match Config.load ?config_file:options.config_path ~cli_args:config_args () with
  | Error errors ->
      Error (String.concat "\n" (List.map Config.string_of_error errors))
  | Ok config ->
      let config =
        if options.data_dir_explicit then
          config
        else
          { config with data_dir = file_dir path }
      in
      begin
        match Config.validate config with
        | Error errors -> Error (String.concat "\n" errors)
        | Ok config ->
            begin
              match load_agent path with
              | Error err -> Error err
              | Ok agent -> Af_agent.create_app ?llm_call config agent
            end
      end

let read_all_stdin () =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line stdin);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  trim (Buffer.contents buffer)

let capture_batch app prompt =
  let final_message = ref None in
  let runtime_error = ref None in
  let emit = function
    | Acp.Message.Agent_message { content; _ } -> final_message := Some content
    | Acp.Message.Error { message; _ } -> runtime_error := Some message
    | _ -> ()
  in
  match Agent_runtime.Session.process ~emit app ~chat_id:1 ~persistent:false prompt with
  | Error err -> Error err
  | Ok () ->
      begin
        match !runtime_error, !final_message with
        | Some err, _ -> Error err
        | None, Some text -> Ok text
        | None, None -> Error "Agent completed without a final response"
      end

let parse_mode anon_args =
  match anon_args with
  | [ file ] -> Ok (Interactive file)
  | "run" :: file :: prompt_parts ->
      let prompt =
        match prompt_parts with
        | [] ->
            let stdin_prompt = read_all_stdin () in
            if stdin_prompt = "" then
              ""
            else
              stdin_prompt
        | _ -> String.concat " " prompt_parts
      in
      if prompt = "" then
        Error "af run <file.md> requires a prompt argument or stdin input"
      else
        Ok (Batch (file, prompt))
  | _ ->
      Error "Usage: af [options] <file.md>\n       af [options] run <file.md> <prompt>"

let () =
  let options = ref (default_options ()) in
  let config_args = ref [] in
  let anon_args = ref [] in
  let spec = [
    ("--config", Arg.String (fun path -> options := { !options with config_path = Some path }), "Load configuration from this YAML file");
    ("--model", Arg.String (fun value -> config_args := !config_args @ ["--model"; value]), "Override the model name");
    ("--api-key", Arg.String (fun value -> config_args := !config_args @ ["--api-key"; value]), "Override the API key");
    ("--api-base", Arg.String (fun value -> config_args := !config_args @ ["--api-base"; value]), "Override the API base URL");
    ("--data-dir", Arg.String (fun value ->
         options := { !options with data_dir_explicit = true };
         config_args := !config_args @ ["--data-dir"; value]), "Override the runtime data root");
    ("--max-tool-iterations", Arg.Int (fun value -> config_args := !config_args @ ["--max-tool-iterations"; string_of_int value]), "Set max tool iterations");
    ("--debug", Arg.Unit (fun () -> options := { !options with debug = true }), "Enable debug logging");
  ] in
  Arg.parse spec (fun arg -> anon_args := !anon_args @ [arg]) "Usage: af [options] <file.md> | af [options] run <file.md> <prompt>";
  match parse_mode !anon_args with
  | Error err ->
      prerr_endline err;
      exit 2
  | Ok mode ->
      let log_level = if !options.debug then Logs.Debug else Logs.Info in
      Logs.set_level (Some log_level);
      Logs.set_reporter (Logs.reporter ());
      begin
        match mode with
        | Interactive path ->
            begin
              match build_app !options !config_args path with
              | Error err ->
                  prerr_endline err;
                  exit 1
              | Ok app ->
                  Agent_tui.Tui.run ~state:app ~chat_id:1 ~persistent:false;
                  Agent_runtime.App.close app;
                  exit 0
            end
        | Batch (path, prompt) ->
            begin
              match build_app !options !config_args path with
              | Error err ->
                  prerr_endline err;
                  exit 1
              | Ok app ->
                  let result =
                    Fun.protect
                      ~finally:(fun () -> Agent_runtime.App.close app)
                      (fun () -> capture_batch app prompt)
                  in
                  match result with
                  | Ok text ->
                      print_endline text;
                      exit 0
                  | Error err ->
                      prerr_endline err;
                      exit 1
            end
      end
