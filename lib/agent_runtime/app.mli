type t

type llm_call =
  Llm_provider.provider_config ->
  ?emit:(Acp.Message.t -> unit) ->
  system_prompt:string ->
  Llm_types.message list ->
  tools:Llm_types.tool_definition list ->
  (Llm_types.messages_response, string) result

val create :
  ?llm_call:llm_call ->
  ?system_prompt_override:string ->
  Config.config ->
  (t, string) result

val close : t -> unit

val project_root : t -> string
val db_path : t -> string
val tool_definitions : t -> Llm_types.tool_definition list
val internal_state : t -> Runtime.app_state
