type llm_call =
  Llm_provider.provider_config ->
  ?on_text_delta:(string -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result

type app_state = {
  config : Oclaw_config.Config.config;
  provider_config : Llm_provider.provider_config;
  db : Db.t;
  transcript : Transcript.t;
  skills : Skills.t;
  tools : Tools.t;
  llm_call : llm_call;
  system_prompt_override : string option;
}

let default_llm_call provider_config ?on_text_delta ~system_prompt messages ~tools =
  Llm_provider.send_message provider_config ?on_text_delta ~system_prompt messages ~tools

let ensure_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p path

let create_app_state ?(llm_call=default_llm_call) ?system_prompt_override config =
  let runtime_dir = Oclaw_config.Config.runtime_data_dir config in
  let skills_dir = Oclaw_config.Config.skills_data_dir config in
  try
    ensure_dir runtime_dir;
    ensure_dir skills_dir;
    let db_path = Filename.concat runtime_dir "oclaw.db" in
    match Db.create db_path with
    | Error err -> Error err
    | Ok db ->
        let transcript = Transcript.create ~data_dir:runtime_dir ~runtime_dir in
        let skills = Skills.create ~skills_dir in
        let tools =
          Tools.create_default_registry
            ~data_dir:config.data_dir
            ~skills_dir
            ~db
            ()
        in
        Ok {
          config;
          provider_config = Oclaw_config.Config.to_llm_provider_config config;
          db;
          transcript;
          skills;
          tools;
          llm_call;
          system_prompt_override;
        }
  with exn ->
    Error (Printexc.to_string exn)

let close_app_state state =
  Transcript.close state.transcript;
  Db.close state.db
