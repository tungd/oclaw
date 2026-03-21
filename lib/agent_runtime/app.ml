type t = Runtime.app_state

type llm_call =
  Runtime.llm_call

let create ?llm_call ?system_prompt_override config =
  Runtime.create_app_state ?llm_call ?system_prompt_override config

let close = Runtime.close_app_state

let project_root (app : t) =
  app.Runtime.project_root

let model_name (app : t) =
  app.Runtime.provider_config.Llm_provider.model_name

let db_path (app : t) =
  app.Runtime.db_path

let tool_definitions (app : t) =
  Tools.definitions app.Runtime.tools

let internal_state (app : t) =
  app
