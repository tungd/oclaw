(** LLM Provider module for OpenAI-compatible APIs *)

(** LLM Model configuration *)
type llm_model = {
  id : string;
  name : string;
  reasoning : bool;
  input_types : string list;
  cost : float * float * float * float;
  context_window : int;
  max_tokens : int;
}

(** Message role types *)
type message_role = System | User | Assistant | Tool

(** Message type for LLM conversation *)
type message = {
  role : message_role;
  content : string;
}

(** LLM Provider configuration *)
type provider_config = {
  api_base : string;
  api_key : string;
  model : llm_model;
  temperature : float;
  max_tokens : int;
  timeout : int;
}

(** Tool definition for function calling *)
type tool_definition = {
  name : string;
  description : string;
  parameters_json : string;
}

(** LLM Response result *)
type llm_result = 
  | Success of llm_response
  | Error of string

and llm_response = {
  id : string;
  object_ : string;
  created : int;
  model : string;
  choices : llm_choice list;
  usage_prompt_tokens : int;
  usage_completion_tokens : int;
  usage_total_tokens : int;
  system_fingerprint : string option;
}

and llm_choice = {
  index : int;
  message : message;
  finish_reason : string;
}

(** Convert between message role and string *)
val role_to_string : message_role -> string
val string_to_role : string -> message_role

(** Default Qwen3.5+ model configuration *)
val qwen35_plus_model : llm_model

(** Create DashScope provider configuration *)
val create_dashscope_provider : ?temperature:float -> ?max_tokens:int -> unit -> provider_config

(** Make LLM API call *)
val call_llm : provider_config -> message list -> ?tools:'a option -> unit -> llm_result

(** Extract assistant message from response *)
val get_assistant_message : llm_result -> message option

(** Check if response contains tool calls *)
val has_tool_calls : llm_result -> bool