type llm_call =
  Llm_provider.provider_config ->
  ?emit:(Acp.Message.t -> unit) ->
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
  tools : Tools.t;
  llm_call : llm_call;
  system_prompt_override : string option;
}

val default_llm_call : llm_call
val ensure_dir : string -> unit
val create_app_state : ?llm_call:llm_call -> ?system_prompt_override:string -> Config.config -> (app_state, string) result
val close_app_state : app_state -> unit
