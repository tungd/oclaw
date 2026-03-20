type llm_call =
  Llm_provider.provider_config ->
  ?on_text_delta:(string -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result

type app_state = {
  config : Config.config;
  provider_config : Llm_provider.provider_config;
  project_root : string;
  db_path : string;
  transcript : Transcript.t;
  skills : Agent_skills.Skills.t;
  tools : Agent_tools.Tools.t;
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
  let layout = Project_paths.discover ~start_dir:config.Config.data_dir () in
  try
    ensure_dir layout.agents_dir;
    ensure_dir layout.skills_dir;
    let transcript = Transcript.create ~db_path:layout.db_path in
    let skills = Agent_skills.Skills.create ~skills_dir:layout.skills_dir in
    let tools =
      Agent_tools.Tools.create_default_registry
        ~db_path:layout.db_path
        ~project_root:layout.project_root
        ()
    in
    Ok {
      config;
      provider_config = Config.to_llm_provider_config config;
      project_root = layout.project_root;
      db_path = layout.db_path;
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
  Agent_tools.Tools.close state.tools
