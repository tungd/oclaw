(** Structured LLM message and tool types for agent workflows. *)

type text_block = {
  text : string;
}

type tool_definition = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
}

type image_source = {
  source_type : string;
  media_type : string;
  data : string;
}

type image_block = {
  source : image_source;
}

type tool_use_block = {
  id : string;
  name : string;
  input : Yojson.Safe.t;
}

type tool_result_block = {
  tool_use_id : string;
  content : string;
  is_error : bool option;
}

type content_block =
  | Text of text_block
  | Image of image_block
  | Tool_use of tool_use_block
  | Tool_result of tool_result_block

type message_content =
  | Text_content of string
  | Blocks of content_block list

type message = {
  role : string;
  content : message_content;
}

type usage = {
  input_tokens : int;
  output_tokens : int;
}

type response_text_block = {
  text : string;
}

type response_tool_use_block = {
  id : string;
  name : string;
  input : Yojson.Safe.t;
}

type response_content_block =
  | Response_text of response_text_block
  | Response_tool_use of response_tool_use_block

type messages_response = {
  content : response_content_block list;
  stop_reason : string option;
  usage : usage option;
}

val tool_definition_to_yojson : tool_definition -> Yojson.Safe.t
val tool_definition_of_yojson : Yojson.Safe.t -> (tool_definition, string) result
val image_source_to_yojson : image_source -> Yojson.Safe.t
val image_source_of_yojson : Yojson.Safe.t -> (image_source, string) result
val content_block_to_yojson : content_block -> Yojson.Safe.t
val content_block_of_yojson : Yojson.Safe.t -> (content_block, string) result
val message_content_to_yojson : message_content -> Yojson.Safe.t
val message_content_of_yojson : Yojson.Safe.t -> (message_content, string) result
val message_to_yojson : message -> Yojson.Safe.t
val message_of_yojson : Yojson.Safe.t -> (message, string) result
val messages_to_yojson : message list -> Yojson.Safe.t
val messages_of_yojson : Yojson.Safe.t -> (message list, string) result
val usage_to_yojson : usage -> Yojson.Safe.t
val usage_of_yojson : Yojson.Safe.t -> (usage, string) result
val response_content_block_to_yojson : response_content_block -> Yojson.Safe.t
val response_content_block_of_yojson : Yojson.Safe.t -> (response_content_block, string) result
val messages_response_to_yojson : messages_response -> Yojson.Safe.t
val messages_response_of_yojson : Yojson.Safe.t -> (messages_response, string) result
