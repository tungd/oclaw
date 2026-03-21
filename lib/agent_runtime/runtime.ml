type llm_call =
  Llm_provider.provider_config ->
  ?emit:(Acp.Message.t -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result

type pending_permission = {
  tool_call : Acp.Message.tool_call;
  request : Tools.approval_request;
  resume : Acp.Message.permission_outcome -> (unit, string) result;
}

type app_state = {
  config : Config.config;
  provider_config : Llm_provider.provider_config;
  project_root : string;
  db_path : string;
  transcript : Transcript.t;
  skills : Agent_skills.Skills.t;
  tools : Tools.t;
  llm_call : llm_call;
  system_prompt_override : string option;
  pending_permissions : (int, pending_permission) Hashtbl.t;
  pending_permissions_mutex : Mutex.t;
}

let default_llm_call provider_config ?emit ~system_prompt messages ~tools =
  let on_text_delta =
    Option.map
      (fun emit delta -> emit (Acp.Message.Agent_delta { content = delta }))
      emit
  in
  Llm_provider.send_message provider_config ?on_text_delta ~system_prompt messages ~tools

let ensure_dir = Project_paths.ensure_dir

let create_app_state ?(llm_call=default_llm_call) ?system_prompt_override config =
  let layout = Project_paths.discover ~start_dir:config.Config.data_dir () in
  try
    ensure_dir layout.agents_dir;
    ensure_dir layout.project_skills_dir;
    ensure_dir layout.user_agents_dir;
    ensure_dir layout.user_skills_dir;
    let transcript = Transcript.create ~db_path:layout.db_path in
    let skills =
      Agent_skills.Skills.create
        ~project_skills_dir:layout.project_skills_dir
        ~user_skills_dir:layout.user_skills_dir
        ~catalog_cache_path:layout.catalog_cache_path
        ()
    in
    let tools =
      Tools.create_default_registry
        ~db_path:layout.db_path
        ~project_root:layout.project_root
        ~skills
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
      pending_permissions = Hashtbl.create 8;
      pending_permissions_mutex = Mutex.create ();
    }
  with exn ->
    Error (Printexc.to_string exn)

let close_app_state state =
  Transcript.close state.transcript;
  Tools.close state.tools
