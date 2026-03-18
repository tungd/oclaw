(** LLM provider for structured messages via OpenAI-compatible chat completions. *)

open Yojson.Safe
open Yojson.Safe.Util

module Http = Http_client
module Llm = Llm_types
module Log = (val Logs.src_log (Logs.Src.create "llm_provider") : Logs.LOG)

type llm_model = {
  id : string;
  name : string;
  reasoning : bool;
  input_types : string list;
  cost : float * float * float * float;
  context_window : int;
  max_tokens : int;
}

type provider_config = {
  api_base : string;
  api_key : string;
  model : llm_model;
  temperature : float;
  max_tokens : int;
  timeout : int;
}

let qwen35_plus_model = {
  id = "qwen3.5-plus";
  name = "qwen3.5-plus";
  reasoning = false;
  input_types = [ "text"; "image" ];
  cost = (0.0, 0.0, 0.0, 0.0);
  context_window = 1_000_000;
  max_tokens = 65_536;
}

let create_dashscope_provider ?(temperature=0.7) ?(max_tokens=4096) () = {
  api_base = "https://coding-intl.dashscope.aliyuncs.com/v1";
  api_key = "";
  model = qwen35_plus_model;
  temperature;
  max_tokens;
  timeout = 60;
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
    ("model", `String provider.model.name);
    ("messages", `List request_messages);
    ("temperature", `Float provider.temperature);
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
    let choice = json |> member "choices" |> to_list |> List.hd in
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
      try
        message_json
        |> member "tool_calls"
        |> to_list
        |> List.map (fun item ->
               let id = item |> member "id" |> to_string in
               let name = item |> member "function" |> member "name" |> to_string in
               let args =
                 item |> member "function" |> member "arguments"
                 |> to_string
                 |> Yojson.Safe.from_string
               in
               Llm.Response_tool_use { id; name; input = args })
      with _ ->
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
              try json |> member "choices" |> to_list
              with _ -> []
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
                  try delta |> member "tool_calls" |> to_list
                  with _ -> []
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

let send_message provider ?on_text_delta ~system_prompt messages ~tools =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    ("Content-Type", "application/json");
    ("Authorization", "Bearer " ^ provider.api_key);
  ] in
  let body =
    build_request_json provider ~system_prompt messages ~tools ~stream:true
    |> Yojson.Safe.to_string
  in
  let parser = create_stream_parser ?on_text_delta () in
  match Http.post_streaming url headers body provider.timeout (fun chunk ->
    ignore (feed_parser parser chunk)) with
  | Error err -> Error err
  | Ok () -> finalize_parser parser
