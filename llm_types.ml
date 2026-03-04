(** LLM-related types with protocol-conv JSON deriving *)

open Protocol_conv_json

type message_role = System | User | Assistant | Tool
[@@deriving protocol ~driver:(module Json)]

type tool_call = {
  id : string;
  type_ : string;
  function_name : string;
  function_args : string;
}
[@@deriving protocol ~driver:(module Json)]

type llm_message = {
  role : message_role;
  content : string;
  tool_calls : tool_call list option;
  tool_call_id : string option;
}
[@@deriving protocol ~driver:(module Json)]

type tool_definition = {
  type_ : string;
  function_ : function_def;
}
[@@deriving protocol ~driver:(module Json)]

and function_def = {
  name : string;
  description : string;
  parameters : Json.t;
}
[@@deriving protocol ~driver:(module Json)]

type llm_request = {
  model : string;
  messages : llm_message list;
  tools : tool_definition list option;
  temperature : float option;
  max_tokens : int option;
}
[@@deriving protocol ~driver:(module Json)]

type llm_response_choice = {
  index : int;
  message : llm_message;
  finish_reason : string option;
}
[@@deriving protocol ~driver:(module Json)]

type llm_response = {
  id : string;
  object_ : string;
  created : int;
  model : string;
  choices : llm_response_choice list;
  usage : Json.t option;
}
[@@deriving protocol ~driver:(module Json)]
