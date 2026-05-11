type locator =
  | Local of string
  | Remote of string

type tool_language =
  | Bash
  | Python
  | Javascript

type embedded_tool = {
  name : string;
  description : string;
  language : tool_language;
  code : string;
}

type document = {
  name : string;
  description : string option;
  uses : (string * string) list;
  prompt : string;
  tools : embedded_tool list;
  locator : locator;
}

type resolved = {
  document : document;
  children : (string * resolved) list;
}

val lock_path : string -> string
val parse_string : locator:locator -> string -> (document, string) result
val load : path:string -> (resolved, string) result
val load_locator : locator -> (resolved, string) result
val system_prompt : resolved -> string

val extra_tools :
  ?llm_call:Agent_runtime.App.llm_call ->
  Agent_runtime.Config.config ->
  resolved ->
  Agent_runtime.Tools.custom_tool list

val create_app :
  ?llm_call:Agent_runtime.App.llm_call ->
  Agent_runtime.Config.config ->
  resolved ->
  (Agent_runtime.App.t, string) result
