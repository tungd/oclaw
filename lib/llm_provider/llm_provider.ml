(** OpenAI-compatible transport adapter over shared LLM message/tool types. *)

open Httpkit
open Yojson.Safe.Util

module Llm = Llm_types
module Log = (val Logs.src_log (Logs.Src.create "llm_provider") : Logs.LOG)

type provider_config = {
  api_base : string;
  api_key : string;
  model_name : string;
  max_tokens : int;
  timeout : int;
}

let tool_definitions_to_json tools =
  `List
    (List.map
       (fun (tool : Llm_types.tool_definition) ->
         `Assoc [
           ("type", `String "function");
           ("function", `Assoc [
              ("name", `String tool.Llm_types.name);
              ("description", `String tool.Llm_types.description);
              ("parameters", tool.Llm_types.input_schema);
            ]);
         ])
       tools)

let image_block_to_json source =
  `Assoc [
    ("type", `String "image_url");
    ("image_url", `Assoc [
       ("url", `String ("data:" ^ source.Llm.media_type ^ ";base64," ^ source.data));
     ]);
  ]

let flatten_message (message : Llm_types.message) =
  match message.Llm.content with
  | Llm.Text_content text ->
      [
        `Assoc [
          ("role", `String message.role);
          ("content", `String text);
        ]
      ]
  | Llm.Blocks blocks ->
      begin
        match message.role with
        | "assistant" ->
            let text =
              blocks
              |> List.filter_map (function
                     | Llm.Text { text } -> Some text
                     | _ -> None)
              |> String.concat ""
            in
            let tool_calls =
              blocks
              |> List.filter_map (function
                     | Llm.Tool_use { id; name; input } ->
                         Some
                           (`Assoc [
                              ("id", `String id);
                              ("type", `String "function");
                              ("function", `Assoc [
                                 ("name", `String name);
                                 ("arguments", `String (Yojson.Safe.to_string input));
                               ]);
                            ])
                     | _ -> None)
            in
            [
              `Assoc [
                ("role", `String "assistant");
                ("content", `String text);
                ("tool_calls", `List tool_calls);
              ]
            ]
        | "user" ->
            let tool_results =
              blocks
              |> List.filter_map (function
                     | Llm.Tool_result { tool_use_id; content; _ } ->
                         Some
                           (`Assoc [
                              ("role", `String "tool");
                              ("tool_call_id", `String tool_use_id);
                              ("content", `String content);
                            ])
                     | _ -> None)
            in
            if tool_results <> [] then
              tool_results
            else
              let content =
                blocks
                |> List.filter_map (function
                       | Llm.Text { text } ->
                           Some (`Assoc [ ("type", `String "text"); ("text", `String text) ])
                       | Llm.Image { source } ->
                           Some (image_block_to_json source)
                       | _ -> None)
              in
              [ `Assoc [ ("role", `String "user"); ("content", `List content) ] ]
        | role ->
            [
              `Assoc [
                ("role", `String role);
                ("content", `String "");
              ]
            ]
      end

let build_request_json provider ~system_prompt messages ~tools ~stream =
  let request_messages =
    (`Assoc [ ("role", `String "system"); ("content", `String system_prompt) ])
    :: List.concat_map flatten_message messages
  in
  `Assoc [
    ("model", `String provider.model_name);
    ("messages", `List request_messages);
    ("max_tokens", `Int provider.max_tokens);
    ("tools", tool_definitions_to_json tools);
    ("tool_choice", `String "auto");
    ("stream", `Bool stream);
  ]

let parse_usage json =
  try
    Some {
      Llm.input_tokens = json |> member "prompt_tokens" |> to_int;
      output_tokens = json |> member "completion_tokens" |> to_int;
    }
  with _ ->
    None

let parse_response json =
  try
    (* Check for error response first *)
    begin
      match json |> member "error" with
      | `Null -> ()
      | `Assoc error_fields ->
          let error_msg = 
            match List.find_opt (fun (k, _) -> k = "message") error_fields with
            | Some (_, `String msg) -> msg
            | _ -> Yojson.Safe.to_string json
          in
          raise (Failure ("LLM API error: " ^ error_msg))
      | _ -> ()
    end;
    let choices =
      match json |> member "choices" with
      | `List choices -> choices
      | `Null | _ -> []
    in
    let choice = match choices with
      | [] -> raise (Failure ("no choices in response: " ^ Yojson.Safe.to_string json))
      | hd :: _ -> hd
    in
    let finish_reason =
      try choice |> member "finish_reason" |> to_string
      with _ -> "stop"
    in
    let message_json = choice |> member "message" in
    let text =
      match message_json |> member "content" with
      | `String value -> value
      | `Null -> ""
      | `List items ->
          items
          |> List.filter_map (fun item ->
                 try Some (item |> member "text" |> to_string)
                 with _ -> None)
          |> String.concat ""
      | _ -> ""
    in
    let tool_uses =
      match message_json |> member "tool_calls" with
      | `List tool_calls ->
          tool_calls
          |> List.map (fun item ->
                 let id = item |> member "id" |> to_string in
                 let name = item |> member "function" |> member "name" |> to_string in
                 let args =
                   item |> member "function" |> member "arguments"
                   |> to_string
                   |> Yojson.Safe.from_string
                 in
                 Llm.Response_tool_use { id; name; input = args })
      | `Null | _ ->
          []
    in
    let content =
      (if String.trim text = "" then [] else [ Llm.Response_text { text } ]) @ tool_uses
    in
    let stop_reason =
      if tool_uses <> [] || String.equal finish_reason "tool_calls" then Some "tool_use"
      else if String.equal finish_reason "length" then Some "max_tokens"
      else Some "end_turn"
    in
    Ok {
      Llm.content;
      stop_reason;
      usage = parse_usage (json |> member "usage");
    }
  with exn ->
    Error (Printf.sprintf "Failed to parse LLM response: %s" (Printexc.to_string exn))

type partial_tool_call = {
  mutable id : string option;
  name : Buffer.t;
  arguments : Buffer.t;
}

let create_partial_tool_call () = {
  id = None;
  name = Buffer.create 32;
  arguments = Buffer.create 128;
}

let strip_carriage_returns value =
  value
  |> String.to_seq
  |> Seq.filter (fun ch -> ch <> '\r')
  |> String.of_seq

type stream_parser = {
  pending : Buffer.t;
  text_buffer : Buffer.t;
  tool_calls : (int, partial_tool_call) Hashtbl.t;
  stop_reason : string option ref;
  usage : Llm.usage option ref;
  on_text_delta : (string -> unit) option;
}

let create_stream_parser ?on_text_delta () = {
  pending = Buffer.create 1024;
  text_buffer = Buffer.create 1024;
  tool_calls = Hashtbl.create 8;
  stop_reason = ref None;
  usage = ref None;
  on_text_delta;
}

let process_stream_event parser raw_event =
  let lines = String.split_on_char '\n' raw_event in
  let data_lines =
    lines
    |> List.filter_map (fun line ->
           if String.starts_with ~prefix:"data:" line then
             Some (String.trim (String.sub line 5 (String.length line - 5)))
           else
             None)
  in
  let payload =
    match String.concat "\n" data_lines with
    | "" when String.trim raw_event <> "" -> String.trim raw_event
    | value -> value
  in
  match payload with
  | "" | "[DONE]" -> Ok ()
  | payload ->
      begin
        try
          let json = Yojson.Safe.from_string payload in
          (* Check for error response in streaming *)
          begin
            match json |> member "error" with
            | `Null -> ()
            | `Assoc error_fields ->
                let error_msg = 
                  match List.find_opt (fun (k, _) -> k = "message") error_fields with
                  | Some (_, `String msg) -> msg
                  | _ -> Yojson.Safe.to_string json
                in
                Log.err (fun m -> m "LLM API error response: %s" error_msg);
                raise (Failure ("LLM API error: " ^ error_msg))
            | _ -> ()
          end;
          if data_lines = [] then
            match parse_response json with
            | Ok response ->
                List.iter
                  (function
                    | Llm.Response_text { text } ->
                        Buffer.add_string parser.text_buffer text;
                        Option.iter (fun f -> f text) parser.on_text_delta
                    | Llm.Response_tool_use { id; name; input } ->
                        let tool_call = create_partial_tool_call () in
                        tool_call.id <- Some id;
                        Buffer.add_string tool_call.name name;
                        Buffer.add_string tool_call.arguments (Yojson.Safe.to_string input);
                        Hashtbl.replace parser.tool_calls (Hashtbl.length parser.tool_calls) tool_call)
                  response.Llm.content;
                parser.stop_reason := response.Llm.stop_reason;
                parser.usage := response.Llm.usage;
                Ok ()
            | Error err -> Error err
          else (
            begin
              match parse_usage (json |> member "usage") with
              | Some usage -> parser.usage := Some usage
              | None -> ()
            end;
            let choices =
              match json |> member "choices" with
              | `List choices -> choices
              | `Null | _ -> []
            in
            List.iter
              (fun choice ->
                begin
                  match choice |> member "finish_reason" with
                  | `String "tool_calls" -> parser.stop_reason := Some "tool_use"
                  | `String "length" -> parser.stop_reason := Some "max_tokens"
                  | `String _ -> parser.stop_reason := Some "end_turn"
                  | _ -> ()
                end;
                let delta = choice |> member "delta" in
                begin
                  match delta |> member "content" with
                  | `String text when text <> "" ->
                      Buffer.add_string parser.text_buffer text;
                      Option.iter (fun f -> f text) parser.on_text_delta
                  | _ -> ()
                end;
                let delta_tool_calls =
                  match delta |> member "tool_calls" with
                  | `List tool_calls -> tool_calls
                  | `Null | _ -> []
                in
                List.iter
                  (fun tool_json ->
                    let index = tool_json |> member "index" |> to_int in
                    let tool_call =
                      match Hashtbl.find_opt parser.tool_calls index with
                      | Some value -> value
                      | None ->
                          let value = create_partial_tool_call () in
                          Hashtbl.replace parser.tool_calls index value;
                          value
                    in
                    begin
                      match tool_json |> member "id" with
                      | `String id when id <> "" -> tool_call.id <- Some id
                      | _ -> ()
                    end;
                    begin
                      match tool_json |> member "function" |> member "name" with
                      | `String name when name <> "" -> Buffer.add_string tool_call.name name
                      | _ -> ()
                    end;
                    begin
                      match tool_json |> member "function" |> member "arguments" with
                      | `String args when args <> "" -> Buffer.add_string tool_call.arguments args
                      | _ -> ()
                    end)
                  delta_tool_calls)
              choices;
            Ok ())
        with exn ->
          Error (Printf.sprintf "Failed to parse streamed LLM chunk: %s" (Printexc.to_string exn))
      end

let feed_parser parser chunk =
  Buffer.add_string parser.pending (strip_carriage_returns chunk);
  let rec loop () =
    let content = Buffer.contents parser.pending in
    let len = String.length content in
    let rec find_event_end index =
      if index + 1 >= len then None
      else if content.[index] = '\n' && content.[index + 1] = '\n' then Some (index, 2)
      else find_event_end (index + 1)
    in
    match find_event_end 0 with
    | Some (idx, sep_len) ->
        let raw_event = String.sub content 0 idx in
        let remaining =
          String.sub content (idx + sep_len) (String.length content - idx - sep_len)
        in
        Buffer.clear parser.pending;
        Buffer.add_string parser.pending remaining;
        begin
          match process_stream_event parser raw_event with
          | Ok () -> loop ()
          | Error _ as err -> err
        end
    | None -> Ok ()
  in
  loop ()

let finalize_parser parser =
  let content = Buffer.contents parser.pending in
  let result =
    if String.trim content <> "" then
      process_stream_event parser content
    else
      Ok ()
  in
  match result with
  | Error _ as err -> err
  | Ok () ->
      let response_content =
        let text = Buffer.contents parser.text_buffer in
        let text_blocks =
          if String.trim text = "" then [] else [ Llm.Response_text { text } ]
        in
        let tool_blocks =
          Hashtbl.to_seq_keys parser.tool_calls
          |> List.of_seq
          |> List.sort compare
          |> List.filter_map (fun index ->
                 match Hashtbl.find_opt parser.tool_calls index with
                 | None -> None
                 | Some tool_call ->
                     let id = Option.value ~default:(Printf.sprintf "call-%d" index) tool_call.id in
                     let name = Buffer.contents tool_call.name in
                     let arguments = Buffer.contents tool_call.arguments in
                     let input =
                       if String.trim arguments = "" then `Assoc []
                       else Yojson.Safe.from_string arguments
                     in
                     Some (Llm.Response_tool_use { id; name; input }))
        in
        text_blocks @ tool_blocks
      in
      Ok {
        Llm.content = response_content;
        stop_reason = !(parser.stop_reason);
        usage = !(parser.usage);
      }

let error_message_of_body body =
  let body = String.trim body in
  if body = "" then
    None
  else
    try
      let json = Yojson.Safe.from_string body in
      match json |> member "error" |> member "message" with
      | `String message when String.trim message <> "" -> Some message
      | _ -> Some body
    with _ -> Some body

let retry_error ?http_status message =
  Retry.{ message; http_status }

let http_error_of_response response body =
  let status = H1.Status.to_code response.H1.Response.status in
  let message =
    match error_message_of_body body with
    | Some error_body -> Printf.sprintf "HTTP %d: %s" status error_body
    | None -> Printf.sprintf "HTTP %d" status
  in
  retry_error ~http_status:status message

(** Execute the actual HTTP request to LLM API *)
let send_message_impl provider ?on_text_delta ~system_prompt messages ~tools =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    ("Content-Type", "application/json");
    ("Authorization", "Bearer " ^ provider.api_key);
  ] in
  let request = H1.Request.create ~headers:(H1.Headers.of_list headers) `POST url in
  let body =
    build_request_json provider ~system_prompt messages ~tools ~stream:true
    |> Yojson.Safe.to_string
  in
  let parser = create_stream_parser ?on_text_delta () in
  match Client.execute_request ~body ~timeout:provider.timeout ~on_write:(fun chunk ->
    ignore (feed_parser parser chunk)) request with
  | Error err -> Error (retry_error err)
  | Ok (response, body) ->
      let status = H1.Status.to_code response.H1.Response.status in
      if status < 200 || status >= 300 then
        Error (http_error_of_response response body)
      else
        finalize_parser parser
        |> Result.map_error (fun message -> retry_error ~http_status:status message)

(** Send message with automatic retry on transient failures *)
let send_message provider ?on_text_delta ~system_prompt messages ~tools =
  (* Wrap the actual API call with retry logic *)
  let make_request () =
    send_message_impl provider ?on_text_delta ~system_prompt messages ~tools
  in
  match Retry.with_retry ~config:Retry.default_config make_request with
  | Retry.Success response ->
      Log.info (fun m -> m "LLM API call succeeded");
      Ok response
  | Retry.Failed (error, attempts) ->
      Log.err (fun m -> m "LLM API call failed after %d attempts: %s" attempts error.message);
      Error (Printf.sprintf "LLM API error after %d attempts: %s" attempts error.message)

(* Expose Llm_types module for backward compatibility *)
module Llm_types = struct
  include Llm_types
end

(* Expose Retry module for testing and advanced usage *)
module Retry = Retry

(* Expose Token_estimator module for token counting *)
module Token_estimator = Token_estimator
