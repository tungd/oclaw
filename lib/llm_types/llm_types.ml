[@@@warning "-32"]  (* Suppress unused value warnings for generated serialization functions *)

open Yojson.Safe
module Json = Protocol_conv_json.Json

(* Tool definition has arbitrary JSON input_schema, keep manual converters *)
type tool_definition = {
  name : string;
  description : string;
  input_schema : t;
}

let tool_definition_to_json t =
  `Assoc [
    ("name", `String t.name);
    ("description", `String t.description);
    ("parameters", t.input_schema);
  ]

let tool_definition_of_json json =
  match json with
  | `Assoc fields ->
      let name = match List.find_opt (fun (k, _) -> k = "name") fields with
        | Some (_, `String s) -> s
        | _ -> ""
      in
      let description = match List.find_opt (fun (k, _) -> k = "description") fields with
        | Some (_, `String s) -> s
        | _ -> ""
      in
      let input_schema = match List.find_opt (fun (k, _) -> k = "parameters") fields with
        | Some (_, v) -> v
        | None -> `Assoc []
      in
      Ok { name; description; input_schema }
  | _ -> Error (Json.make_error "invalid tool definition")

let tool_definition_to_yojson = tool_definition_to_json
let tool_definition_of_yojson json =
  match tool_definition_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* Simple record with only basic types - use protocol deriving *)
type image_source = {
  source_type : string [@key "type"];
  media_type : string;
  data : string;
}
[@@deriving protocol ~driver:(module Json)]

let image_source_to_yojson = image_source_to_json
let image_source_of_yojson json =
  match image_source_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* Content block uses "type" field discriminator format required by LLM APIs.
   ppx_protocol_conv serializes variants as ["Constructor", payload] arrays,
   but we need {"type": "constructor", ...fields} objects. *)
type content_block =
  | Text of { text : string }
  | Image of { source : image_source }
  | Tool_use of {
      id : string;
      name : string;
      input : t;
    }
  | Tool_result of {
      tool_use_id : string;
      content : string;
      is_error : bool option;
    }

let content_block_to_json = function
  | Text { text } -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Image { source } ->
      `Assoc [ ("type", `String "image"); ("source", image_source_to_json source) ]
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
      let fields = match is_error with
        | None -> fields
        | Some flag -> fields @ [ ("is_error", `Bool flag) ]
      in
      `Assoc fields

let content_block_of_json json =
  let type_name =
    match json with
    | `Assoc fields ->
        (match List.find_opt (fun (k, _) -> k = "type") fields with
         | Some (_, `String s) -> s
         | _ -> "")
    | _ -> ""
  in
  match type_name with
  | "text" ->
      let text = match json with
        | `Assoc fields ->
            (match List.find_opt (fun (k, _) -> k = "text") fields with
             | Some (_, `String s) -> s
             | _ -> "")
        | _ -> ""
      in
      Ok (Text { text })
  | "image" ->
      (match json with
       | `Assoc fields ->
           (match List.find_opt (fun (k, _) -> k = "source") fields with
            | Some (_, source_json) ->
                (match image_source_of_json source_json with
                 | Ok source -> Ok (Image { source })
                 | Error err -> Error err)
            | None -> Error (Json.make_error "missing source field"))
       | _ -> Error (Json.make_error "invalid image block"))
  | "tool_use" ->
      let get_field name =
        match json with
        | `Assoc fields ->
            (match List.find_opt (fun (k, _) -> k = name) fields with
             | Some (_, v) -> v
             | None -> `Null)
        | _ -> `Null
      in
      let id = match get_field "id" with `String s -> s | _ -> "" in
      let name = match get_field "name" with `String s -> s | _ -> "" in
      let input = get_field "input" in
      Ok (Tool_use { id; name; input })
  | "tool_result" ->
      let get_field name =
        match json with
        | `Assoc fields ->
            (match List.find_opt (fun (k, _) -> k = name) fields with
             | Some (_, v) -> v
             | None -> `Null)
        | _ -> `Null
      in
      let tool_use_id = match get_field "tool_use_id" with `String s -> s | _ -> "" in
      let content = match get_field "content" with `String s -> s | _ -> "" in
      let is_error = match get_field "is_error" with `Bool b -> Some b | _ -> None in
      Ok (Tool_result { tool_use_id; content; is_error })
  | _ -> Error (Json.make_error "invalid content block")

let content_block_to_yojson = content_block_to_json
let content_block_of_yojson json =
  match content_block_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* message_content is polymorphic: string or array of blocks *)
type message_content =
  | Text_content of string
  | Blocks of content_block list

let message_content_to_json = function
  | Text_content text -> `String text
  | Blocks blocks -> `List (List.map content_block_to_json blocks)

let message_content_of_json json =
  match json with
  | `String text -> Ok (Text_content text)
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (Blocks (List.rev acc))
        | item :: rest ->
            (match content_block_of_json item with
             | Ok block -> collect (block :: acc) rest
             | Error err -> Error err)
      in
      collect [] items
  | _ -> Error (Json.make_error "invalid message content")

let message_content_to_yojson = message_content_to_json
let message_content_of_yojson json =
  match message_content_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* message has message_content field which needs manual handling *)
type message = {
  role : string;
  content : message_content;
}

let message_to_json m =
  `Assoc [
    ("role", `String m.role);
    ("content", message_content_to_json m.content);
  ]

let message_of_json json =
  match json with
  | `Assoc fields ->
      let role = match List.find_opt (fun (k, _) -> k = "role") fields with
        | Some (_, `String s) -> s
        | _ -> ""
      in
      let content_json = match List.find_opt (fun (k, _) -> k = "content") fields with
        | Some (_, v) -> v
        | None -> `String ""
      in
      (match message_content_of_json content_json with
       | Ok content -> Ok { role; content }
       | Error err -> Error err)
  | _ -> Error (Json.make_error "invalid message")

let message_to_yojson = message_to_json
let message_of_yojson json =
  match message_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* Simple record with only basic types - use protocol deriving *)
type usage = {
  input_tokens : int;
  output_tokens : int;
}
[@@deriving protocol ~driver:(module Json)]

let usage_to_yojson = usage_to_json
let usage_of_yojson json =
  match usage_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* Response content block uses "type" field discriminator format required by LLM APIs. *)
type response_content_block =
  | Response_text of { text : string }
  | Response_tool_use of {
      id : string;
      name : string;
      input : t;
    }

let response_content_block_to_json = function
  | Response_text { text } ->
      `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Response_tool_use { id; name; input } ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String id);
        ("name", `String name);
        ("input", input);
      ]

let response_content_block_of_json json =
  let type_name =
    match json with
    | `Assoc fields ->
        (match List.find_opt (fun (k, _) -> k = "type") fields with
         | Some (_, `String s) -> s
         | _ -> "")
    | _ -> ""
  in
  match type_name with
  | "text" ->
      let text = match json with
        | `Assoc fields ->
            (match List.find_opt (fun (k, _) -> k = "text") fields with
             | Some (_, `String s) -> s
             | _ -> "")
        | _ -> ""
      in
      Ok (Response_text { text })
  | "tool_use" ->
      let get_field name =
        match json with
        | `Assoc fields ->
            (match List.find_opt (fun (k, _) -> k = name) fields with
             | Some (_, v) -> v
             | None -> `Null)
        | _ -> `Null
      in
      let id = match get_field "id" with `String s -> s | _ -> "" in
      let name = match get_field "name" with `String s -> s | _ -> "" in
      let input = get_field "input" in
      Ok (Response_tool_use { id; name; input })
  | _ -> Error (Json.make_error "invalid response content block")

let response_content_block_to_yojson = response_content_block_to_json
let response_content_block_of_yojson json =
  match response_content_block_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

(* messages_response has response_content_block list which needs manual handling *)
type messages_response = {
  content : response_content_block list;
  stop_reason : string option;
  usage : usage option;
}

let messages_response_to_json r =
  let fields = [
    ("content", `List (List.map response_content_block_to_json r.content));
  ] in
  let fields = match r.stop_reason with
    | None -> fields
    | Some reason -> ("stop_reason", `String reason) :: fields
  in
  let fields = match r.usage with
    | None -> fields
    | Some u -> ("usage", usage_to_json u) :: fields
  in
  `Assoc fields

let messages_response_of_json json =
  match json with
  | `Assoc fields ->
      let content_json = match List.find_opt (fun (k, _) -> k = "content") fields with
        | Some (_, `List items) -> items
        | _ -> []
      in
      let rec parse_content acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            (match response_content_block_of_json item with
             | Ok block -> parse_content (block :: acc) rest
             | Error err -> Error err)
      in
      let stop_reason = match List.find_opt (fun (k, _) -> k = "stop_reason") fields with
        | Some (_, `String s) -> Some s
        | _ -> None
      in
      let usage = match List.find_opt (fun (k, _) -> k = "usage") fields with
        | Some (_, v) ->
            (match usage_of_json v with
             | Ok u -> Some u
             | Error _ -> None)
        | None -> None
      in
      (match parse_content [] content_json with
       | Ok content -> Ok { content; stop_reason; usage }
       | Error err -> Error err)
  | _ -> Error (Json.make_error "invalid messages_response")

let messages_response_to_yojson = messages_response_to_json
let messages_response_of_yojson json =
  match messages_response_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)

let messages_to_json messages =
  `List (List.map message_to_json messages)

let messages_of_json json =
  match json with
  | `List items ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            (match message_of_json item with
             | Ok msg -> collect (msg :: acc) rest
             | Error err -> Error err)
      in
      collect [] items
  | _ -> Error (Json.make_error "invalid messages payload")

let messages_to_yojson = messages_to_json
let messages_of_yojson json =
  match messages_of_json json with
  | Ok v -> Ok v
  | Error err -> Error (Json.error_to_string_hum err)