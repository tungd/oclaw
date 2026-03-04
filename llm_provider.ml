(** LLM Provider module for OpenAI-compatible APIs *)

open Http_client
open Yojson.Safe
open Yojson.Safe.Util

module Http = Http_client
module Log = (val Logs.src_log (Logs.Src.create "llm_provider") : Logs.LOG)

(* Helper function to safely extract optional string from JSON *)
let to_string_option json =
  try Some (to_string json)
  with _ -> None

(* LLM Model configuration *)
type llm_model = {
  id : string;
  name : string;
  reasoning : bool;
  input_types : string list;
  cost : float * float * float * float; (* input, output, cacheRead, cacheWrite *)
  context_window : int;
  max_tokens : int;
}

(* Tool call structure *)
type llm_tool_call = {
  id : string;
  type_ : string;
  function_name : string;
  function_args : string;
}

(* Message types for LLM conversation *)
type message_role = System | User | Assistant | Tool
type message = {
  role : message_role;
  content : string;
  tool_call_id : string option; (* Only used for tool messages *)
  tool_calls : llm_tool_call list; (* Only used for assistant messages *)
}

(* LLM Provider configuration *)
type provider_config = {
  api_base : string;
  api_key : string;
  model : llm_model;
  temperature : float;
  max_tokens : int;
  timeout : int;
}


type llm_choice = {
  index : int;
  message : message;
  finish_reason : string;
  tool_calls : llm_tool_call list;
}

type llm_response = {
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

type llm_result = 
  | Success of llm_response
  | Error of string

(* Convert message role to string *)
let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"

(* Convert string to message role *)
let string_to_role = function
  | "system" -> System
  | "user" -> User
  | "assistant" -> Assistant
  | "tool" -> Tool
  | _ -> User (* default to user *)

(* Default Qwen3.5+ model configuration *)
let qwen35_plus_model = {
  id = "qwen3.5-plus";
  name = "qwen3.5-plus";
  reasoning = false;
  input_types = ["text"; "image"];
  cost = (0.0, 0.0, 0.0, 0.0);
  context_window = 1000000;
  max_tokens = 65536;
}

(* Create DashScope provider configuration *)
let create_dashscope_provider ?(temperature=0.7) ?(max_tokens=4096) () = {
  api_base = "https://coding-intl.dashscope.aliyuncs.com/v1";
  api_key = "sk-sp-4326acec735b4e29b33b31e97d1d66fa";
  model = qwen35_plus_model;
  temperature = temperature;
  max_tokens = max_tokens;
  timeout = 60;
}

let call_llm provider messages ?(tools=None) () =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    "Content-Type", "application/json";
    "Authorization", "Bearer " ^ provider.api_key;
  ] in

  (* Build tools JSON if provided *)
  let tools_json = match tools with
    | Some tools -> Yojson.Safe.to_string tools
    | None -> "null"
  in

  (* Build messages JSON *)
  let message_json_list = List.map (fun msg ->
    (* Handle tool messages with tool_call_id *)
    if msg.role = Tool && msg.tool_call_id <> None then
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\", \"tool_call_id\": \"%s\"}"
        (role_to_string msg.role)
        (String.escaped msg.content)
        (match msg.tool_call_id with Some id -> id | None -> "")
    else if msg.role = Assistant && msg.tool_calls <> [] then
      let tool_calls_json = List.map (fun {id; type_; function_name; function_args} ->
        Printf.sprintf "{\"id\": \"%s\", \"type\": \"%s\", \"function\": {\"name\": \"%s\", \"arguments\": \"%s\"}}"
          (String.escaped id)
          (String.escaped type_)
          (String.escaped function_name)
          (String.escaped function_args)
      ) msg.tool_calls in
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\", \"tool_calls\": [%s]}"
        (role_to_string msg.role)
        (String.escaped msg.content)
        (String.concat ", " tool_calls_json)
    else
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\"}"
        (role_to_string msg.role)
        (String.escaped msg.content)
  ) messages in
  let messages_json = "[" ^ (String.concat ", " message_json_list) ^ "]" in

  (* Build request body *)
  let body = Printf.sprintf
    "{\"model\": \"%s\", \"messages\": %s, \"temperature\": %.2f, \"max_tokens\": %d, \"tools\": %s, \"tool_choice\": \"auto\"}"
    provider.model.name messages_json provider.temperature provider.max_tokens tools_json in

  (* Make HTTP request *)
  let response = Http.post url headers body provider.timeout in

  match response.Http.HttpResponse.error with
  | Some error -> Error error
  | None ->
      try
        (* Parse JSON response *)
        if String.length response.Http.HttpResponse.body > 0 && response.Http.HttpResponse.status = 200 then
          let json = Yojson.Safe.from_string response.Http.HttpResponse.body in

          (* Extract fields from JSON *)
          let id = member "id" json |> to_string in
          let object_ = member "object" json |> to_string in
          let created = member "created" json |> to_int in
          let model = member "model" json |> to_string in

          (* Parse choices *)
          let choices_json = member "choices" json |> to_list in
          let choices = List.mapi (fun index choice_json ->
            let finish_reason = member "finish_reason" choice_json |> to_string in
            let message_json = member "message" choice_json in
            let role_str = member "role" message_json |> to_string in
            let content = member "content" message_json |> to_string in
            let tool_call_id =
              try Some (member "tool_call_id" message_json |> to_string)
              with _ -> None
            in

            (* Parse tool calls if present *)
            let tool_calls =
              try
                let tool_calls_json = member "tool_calls" message_json |> to_list in
                let parsed_tool_calls = List.map (fun tool_call_json ->
                  {
                    id = member "id" tool_call_json |> to_string;
                    type_ = member "type" tool_call_json |> to_string;
                    function_name = (member "function" tool_call_json) |> member "name" |> to_string;
                    function_args = (member "function" tool_call_json) |> member "arguments" |> to_string
                  }
                ) tool_calls_json in
                Log.debug (fun m -> m "Parsed %d tool calls" (List.length parsed_tool_calls));
                parsed_tool_calls
              with _ -> []
            in

            (* Log tool calls when debug is enabled *)
            if tool_calls <> [] then (
              List.iter (fun tc ->
                Log.debug (fun m -> m "Tool call: %s(%s)" tc.function_name tc.function_args)
              ) tool_calls
            );

            {
              index = index;
              message = { role = string_to_role role_str; content = content; tool_call_id = tool_call_id; tool_calls = tool_calls };
              finish_reason = finish_reason;
              tool_calls = tool_calls
            }
          ) choices_json in

          (* Parse usage *)
          let usage_json = member "usage" json in
          let usage_prompt_tokens = member "prompt_tokens" usage_json |> to_int in
          let usage_completion_tokens = member "completion_tokens" usage_json |> to_int in
          let usage_total_tokens = member "total_tokens" usage_json |> to_int in

          (* Parse system_fingerprint if present *)
          let system_fingerprint =
            try
              Some (member "system_fingerprint" json |> to_string)
            with _ -> None
          in

          Success {
            id = id;
            object_ = object_;
            created = created;
            model = model;
            choices = choices;
            usage_prompt_tokens = usage_prompt_tokens;
            usage_completion_tokens = usage_completion_tokens;
            usage_total_tokens = usage_total_tokens;
            system_fingerprint = system_fingerprint;
          }
        else
          Error (Printf.sprintf "Invalid response: status %d, body: %s"
            response.Http.HttpResponse.status response.Http.HttpResponse.body)
      with exn ->
        Error (Printf.sprintf "JSON parse error: %s" (Printexc.to_string exn))

(* Make streaming LLM API call *)
let call_llm_streaming provider messages ?(tools=None) chunk_callback =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    "Content-Type", "application/json";
    "Authorization", "Bearer " ^ provider.api_key;
  ] in

  (* Build tools JSON if provided *)
  let tools_json = match tools with
    | Some tools -> Yojson.Safe.to_string tools
    | None -> "null"
  in

  (* Build messages JSON *)
  let message_json_list = List.map (fun msg ->
    (* Handle tool messages with tool_call_id *)
    if msg.role = Tool && msg.tool_call_id <> None then
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\", \"tool_call_id\": \"%s\"}"
        (role_to_string msg.role)
        (String.escaped msg.content)
        (match msg.tool_call_id with Some id -> id | None -> "")
    else if msg.role = Assistant && msg.tool_calls <> [] then
      let tool_calls_json = List.map (fun {id; type_; function_name; function_args} ->
        Printf.sprintf "{\"id\": \"%s\", \"type\": \"%s\", \"function\": {\"name\": \"%s\", \"arguments\": \"%s\"}}"
          (String.escaped id)
          (String.escaped type_)
          (String.escaped function_name)
          (String.escaped function_args)
      ) msg.tool_calls in
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\", \"tool_calls\": [%s]}"
        (role_to_string msg.role)
        (String.escaped msg.content)
        (String.concat ", " tool_calls_json)
    else
      Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\"}"
        (role_to_string msg.role)
        (String.escaped msg.content)
  ) messages in
  let messages_json = "[" ^ (String.concat ", " message_json_list) ^ "]" in

  (* Build request body *)
  let body = Printf.sprintf
    "{\"model\": \"%s\", \"messages\": %s, \"temperature\": %.2f, \"max_tokens\": %d, \"tools\": %s, \"stream\": true, \"tool_choice\": \"auto\"}"
    provider.model.name messages_json provider.temperature provider.max_tokens tools_json in

  (* Make streaming HTTP request *)
  let result = Http.post_streaming url headers body provider.timeout (fun chunk ->
    (* Parse streaming chunks - each chunk is in format: data: {"choices":[{"delta":{"content":"..."}}]}\n\n *)
    try
      if String.length chunk > 0 then (
        (* Remove any trailing data prefix and parse JSON lines *)
        let lines = String.split_on_char '\n' chunk in
        List.iter (fun line ->
          if String.length line > 6 && String.sub line 0 6 = "data: " then (
            let json_data = String.sub line 6 (String.length line - 6) in
            if json_data <> "[DONE]" then (
              try
                let json = Yojson.Safe.from_string json_data in
                let choices = member "choices" json |> to_list in
                List.iter (fun choice_json ->
                  let delta = member "delta" choice_json in
                  let content = member "content" delta |> to_string_option in
                  match content with
                  | Some text -> chunk_callback text
                  | None -> ()
                ) choices
              with _ -> () (* Ignore parse errors for individual chunks *)
            )
          )
        ) lines
      )
    with _ -> () (* Ignore any parsing errors *)
  ) in

  match result with
  | Ok () -> Ok ()
  | Error msg -> Error msg

(* Simple tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters_json : string;
}

(* Extract assistant message from response *)
let get_assistant_message = function
  | Success response ->
      if List.length response.choices > 0 then
        Some (List.hd response.choices).message
      else None
  | Error _ -> None

(* Check if response contains tool calls *)
let has_tool_calls response = 
  match response with
  | Success resp ->
      List.exists (fun choice ->
        (* Check if message content contains tool calls - simplified *)
        String.length choice.message.content > 0 &&
        choice.message.content.[0] = '{' (* JSON-like content *)
      ) resp.choices
  | Error _ -> false