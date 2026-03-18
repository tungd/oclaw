(** Shared runtime state for the CLI agent. *)

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

val default_llm_call : llm_call

val create_app_state :
  ?llm_call:llm_call ->
  ?system_prompt_override:string ->
  Oclaw_config.Config.config ->
  (app_state, string) result
