open Yojson.Safe
open Yojson.Safe.Util

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

type content_block =
  | Text of { text : string }
  | Image of { source : image_source }
  | Tool_use of {
      id : string;
      name : string;
      input : Yojson.Safe.t;
    }
  | Tool_result of {
      tool_use_id : string;
      content : string;
      is_error : bool option;
    }

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

type response_content_block =
  | Response_text of { text : string }
  | Response_tool_use of {
      id : string;
      name : string;
      input : Yojson.Safe.t;
    }

type messages_response = {
  content : response_content_block list;
  stop_reason : string option;
  usage : usage option;
}

let tool_definition_to_yojson tool =
  `Assoc [
    ("name", `String tool.name);
    ("description", `String tool.description);
    ("input_schema", tool.input_schema);
  ]

let image_source_to_yojson source =
  `Assoc [
    ("type", `String source.source_type);
    ("media_type", `String source.media_type);
    ("data", `String source.data);
  ]

let content_block_to_yojson = function
  | Text { text } -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Image { source } ->
      `Assoc [ ("type", `String "image"); ("source", image_source_to_yojson source) ]
  | Tool_use { id; name; input } ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String id);
        ("name", `String name);
        ("input", input);
      ]
  | Tool_result { tool_use_id; content; is_error } ->
      let fields = [
        ("type", `String "tool_result");
        ("tool_use_id", `String tool_use_id);
        ("content", `String content);
      ] in
      let fields =
        match is_error with
        | None -> fields
        | Some flag -> fields @ [ ("is_error", `Bool flag) ]
      in
      `Assoc fields

let message_content_to_yojson = function
  | Text_content text -> `String text
  | Blocks blocks -> `List (List.map content_block_to_yojson blocks)

let message_to_yojson msg =
  `Assoc [ ("role", `String msg.role); ("content", message_content_to_yojson msg.content) ]

let parse_image_source json =
  try
    Ok {
      source_type = json |> member "type" |> to_string;
      media_type = json |> member "media_type" |> to_string;
      data = json |> member "data" |> to_string;
    }
  with _ ->
    Error "invalid image source"

let parse_content_block json =
  let type_name =
    try json |> member "type" |> to_string
    with _ -> ""
  in
  match type_name with
  | "text" ->
      Ok (Text { text = json |> member "text" |> to_string })
  | "image" ->
      begin
        match parse_image_source (json |> member "source") with
        | Ok source -> Ok (Image { source })
        | Error err -> Error err
      end
  | "tool_use" ->
      Ok
        (Tool_use {
           id = json |> member "id" |> to_string;
           name = json |> member "name" |> to_string;
           input = json |> member "input";
         })
  | "tool_result" ->
      let is_error =
        match json |> member "is_error" with
        | `Bool flag -> Some flag
        | _ -> None
      in
      Ok
        (Tool_result {
           tool_use_id = json |> member "tool_use_id" |> to_string;
           content = json |> member "content" |> to_string;
           is_error;
         })
  | _ ->
      Error "invalid content block"

let message_content_of_yojson json =
  match json with
  | `String text -> Ok (Text_content text)
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (Blocks (List.rev acc))
        | item :: rest ->
            begin
              match parse_content_block item with
              | Ok block -> collect (block :: acc) rest
              | Error err -> Error err
            end
      in
      collect [] items
  | _ -> Error "invalid message content"

let message_of_yojson json =
  try
    let role = json |> member "role" |> to_string in
    let content_json = json |> member "content" in
    match message_content_of_yojson content_json with
    | Ok content -> Ok { role; content }
    | Error err -> Error err
  with _ ->
    Error "invalid message"

let messages_to_yojson messages =
  `List (List.map message_to_yojson messages)

let messages_of_yojson json =
  match json with
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            begin
              match message_of_yojson item with
              | Ok message -> collect (message :: acc) rest
              | Error err -> Error err
            end
      in
      collect [] items
  | _ ->
      Error "invalid messages payload"

let response_content_block_to_yojson = function
  | Response_text { text } ->
      `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Response_tool_use { id; name; input } ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String id);
        ("name", `String name);
        ("input", input);
      ]

let messages_response_to_yojson response =
  let fields = [
    ("content", `List (List.map response_content_block_to_yojson response.content));
  ] in
  let fields =
    match response.stop_reason with
    | None -> fields
    | Some stop_reason -> ("stop_reason", `String stop_reason) :: fields
  in
  let fields =
    match response.usage with
    | None -> fields
    | Some usage ->
        ("usage", `Assoc [
           ("input_tokens", `Int usage.input_tokens);
           ("output_tokens", `Int usage.output_tokens);
         ]) :: fields
  in
  `Assoc fields
