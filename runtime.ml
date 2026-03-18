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
        let skills = Skills.create ~skills_dir in
        let tools =
          Tools.create_default_registry
            ~sandbox_config:{
              Tools.workspace_root = config.tools_workspace;
              restrict_to_workspace = config.tools_restrict_to_workspace;
              allow_read_paths = config.tools_allow_read_paths;
              allow_write_paths = config.tools_allow_write_paths;
              exec_timeout_seconds = config.tools_exec_timeout_seconds;
              exec_enable_deny_patterns = config.tools_exec_enable_deny_patterns;
              exec_custom_deny_patterns = config.tools_exec_custom_deny_patterns;
              exec_custom_allow_patterns = config.tools_exec_custom_allow_patterns;
            }
            ~web_config:Tools.{
              request_timeout_seconds = config.web_request_timeout_seconds;
              fetch_max_bytes = config.web_fetch_max_bytes;
              search_max_results = config.web_search_max_results;
            }
            ~data_dir:config.data_dir
            ~skills_dir
            ~db
            ()
        in
        Ok {
          config;
          provider_config = Oclaw_config.Config.to_llm_provider_config config;
          db;
          skills;
          tools;
          llm_call;
          system_prompt_override;
        }
  with exn ->
    Error (Printexc.to_string exn)
