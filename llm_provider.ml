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

let send_message provider ~system_prompt messages ~tools =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    ("Content-Type", "application/json");
    ("Authorization", "Bearer " ^ provider.api_key);
  ] in
  let body =
    build_request_json provider ~system_prompt messages ~tools ~stream:false
    |> Yojson.Safe.to_string
  in
  
  (* Use curl.multi for better concurrency *)
  let request = Http.HttpRequest.create 
      ~method_:Http.HttpMethod.POST 
      ~url 
      ~headers 
      ~body 
      ~timeout:provider.timeout 
      () 
  in
  
  (* For single request, use the multi interface anyway for consistency *)
  let responses = Http.perform_multi_requests [request] in
  
  match List.hd responses with
  | response when response.Http.HttpResponse.error <> None ->
      Error (Option.get response.Http.HttpResponse.error)
  | response when response.Http.HttpResponse.status < 200 || response.Http.HttpResponse.status >= 300 ->
      Error
        (Printf.sprintf
           "LLM request failed: status %d, body: %s"
           response.Http.HttpResponse.status
           response.Http.HttpResponse.body)
  | response ->
      try
        response.Http.HttpResponse.body
        |> Yojson.Safe.from_string
        |> parse_response
      with exn ->
        let err = Printexc.to_string exn in
        Log.err (fun m -> m "Failed to parse provider response: %s" err);
        Error err
