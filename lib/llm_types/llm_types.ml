type text_block = {
  text : string;
}
[@@deriving yojson]

type tool_definition = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t [@key "parameters"];
}
[@@deriving yojson]

type image_source = {
  source_type : string [@key "type"];
  media_type : string;
  data : string;
}
[@@deriving yojson]

type image_block = {
  source : image_source;
}
[@@deriving yojson]

type tool_use_block = {
  id : string;
  name : string;
  input : Yojson.Safe.t;
}
[@@deriving yojson]

type tool_result_block = {
  tool_use_id : string;
  content : string;
  is_error : bool option;
}
[@@deriving yojson]

type content_block =
  | Text of text_block
  | Image of image_block
  | Tool_use of tool_use_block
  | Tool_result of tool_result_block

let content_block_to_yojson = function
  | Text block ->
      `Assoc [
        ("type", `String "text");
        ("text", `String block.text);
      ]
  | Image block ->
      `Assoc [
        ("type", `String "image");
        ("source", image_source_to_yojson block.source);
      ]
  | Tool_use block ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String block.id);
        ("name", `String block.name);
        ("input", block.input);
      ]
  | Tool_result block ->
      let fields = [
        ("type", `String "tool_result");
        ("tool_use_id", `String block.tool_use_id);
        ("content", `String block.content);
      ] in
      let fields =
        match block.is_error with
        | None -> fields
        | Some flag -> fields @ [ ("is_error", `Bool flag) ]
      in
      `Assoc fields

let content_block_of_yojson = function
  | `Assoc fields ->
      let get_field name =
        match List.assoc_opt name fields with
        | Some value -> value
        | None -> `Null
      in
      let get_string_field name =
        match get_field name with
        | `String s -> s
        | _ -> ""
      in
      begin
        match get_field "type" with
        | `String "text" ->
            Ok (Text { text = get_string_field "text" })
        | `String "image" ->
            begin match image_source_of_yojson (get_field "source") with
            | Ok source -> Ok (Image { source })
            | Error err -> Error err
            end
        | `String "tool_use" ->
            Ok (Tool_use {
              id = get_string_field "id";
              name = get_string_field "name";
              input = get_field "input";
            })
        | `String "tool_result" ->
            let is_error =
              match get_field "is_error" with
              | `Bool flag -> Some flag
              | _ -> None
            in
            Ok (Tool_result {
              tool_use_id = get_string_field "tool_use_id";
              content = get_string_field "content";
              is_error;
            })
        | `String other ->
            Error ("invalid content block type: " ^ other)
        | _ ->
            Error "invalid content block"
      end
  | _ -> Error "invalid content block"

type message_content =
  | Text_content of string
  | Blocks of content_block list

let content_block_list_to_yojson blocks =
  `List (List.map content_block_to_yojson blocks)

let content_block_list_of_yojson = function
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            begin match content_block_of_yojson item with
            | Ok block -> collect (block :: acc) rest
            | Error err -> Error err
            end
      in
      collect [] items
  | _ -> Error "expected content block array"

let message_content_to_yojson = function
  | Text_content text -> `String text
  | Blocks blocks -> content_block_list_to_yojson blocks

let message_content_of_yojson = function
  | `String text -> Ok (Text_content text)
  | `List _ as blocks_json ->
      begin match content_block_list_of_yojson blocks_json with
      | Ok blocks -> Ok (Blocks blocks)
      | Error err -> Error err
      end
  | `Null -> Ok (Text_content "")
  | _ -> Error "invalid message content"

type message = {
  role : string;
  content : message_content
    [@to_yojson message_content_to_yojson]
    [@of_yojson message_content_of_yojson];
}
[@@deriving yojson]

let messages_to_yojson messages =
  `List (List.map message_to_yojson messages)

let messages_of_yojson = function
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            begin match message_of_yojson item with
            | Ok msg -> collect (msg :: acc) rest
            | Error err -> Error err
            end
      in
      collect [] items
  | _ -> Error "Expected JSON array for messages"

type usage = {
  input_tokens : int;
  output_tokens : int;
}
[@@deriving yojson]

type response_text_block = {
  text : string;
}
[@@deriving yojson]

type response_tool_use_block = {
  id : string;
  name : string;
  input : Yojson.Safe.t;
}
[@@deriving yojson]

type response_content_block =
  | Response_text of response_text_block
  | Response_tool_use of response_tool_use_block

let response_content_block_to_yojson = function
  | Response_text block ->
      `Assoc [
        ("type", `String "text");
        ("text", `String block.text);
      ]
  | Response_tool_use block ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String block.id);
        ("name", `String block.name);
        ("input", block.input);
      ]

let response_content_block_of_yojson = function
  | `Assoc fields ->
      let get_field name =
        match List.assoc_opt name fields with
        | Some value -> value
        | None -> `Null
      in
      let get_string_field name =
        match get_field name with
        | `String s -> s
        | _ -> ""
      in
      begin
        match get_field "type" with
        | `String "text" ->
            Ok (Response_text { text = get_string_field "text" })
        | `String "tool_use" ->
            Ok (Response_tool_use {
              id = get_string_field "id";
              name = get_string_field "name";
              input = get_field "input";
            })
        | `String other ->
            Error ("invalid response content block type: " ^ other)
        | _ ->
            Error "invalid response content block"
      end
  | _ -> Error "invalid response content block"

let response_content_block_list_to_yojson blocks =
  `List (List.map response_content_block_to_yojson blocks)

let response_content_block_list_of_yojson = function
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            begin match response_content_block_of_yojson item with
            | Ok block -> collect (block :: acc) rest
            | Error err -> Error err
            end
      in
      collect [] items
  | _ -> Error "expected response content block array"

type messages_response = {
  content : response_content_block list
    [@to_yojson response_content_block_list_to_yojson]
    [@of_yojson response_content_block_list_of_yojson];
  stop_reason : string option;
  usage : usage option;
}
[@@deriving yojson]
