(** LLM Provider module for OpenAI-compatible APIs *)

open Http_client
open Yojson.Basic.Util

module Http = Http_client

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

(* Message types for LLM conversation *)
type message_role = System | User | Assistant | Tool
type message = {
  role : message_role;
  content : string;
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

(* Tool call definition *)
type tool_call = {
  id : string;
  type_ : string;
  function_name : string;
  function_args : string;
}

(* LLM Response types *)
type llm_choice = {
  index : int;
  message : message;
  finish_reason : string;
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

(* Make LLM API call *)
let call_llm provider messages ?(tools=None) () =
  let url = provider.api_base ^ "/chat/completions" in
  let headers = [
    "Content-Type", "application/json";
    "Authorization", "Bearer " ^ provider.api_key;
  ] in
  
  (* Build tools JSON if provided - simplified for now *)
  let tools_json = match tools with
    | Some _ -> "null" (* TODO: implement proper tool serialization *)
    | None -> "null"
  in
  
  (* Build messages JSON *)
  let message_json_list = List.map (fun msg ->
    Printf.sprintf "{\"role\": \"%s\", \"content\": \"%s\"}"
      (role_to_string msg.role)
      (String.escaped msg.content)
  ) messages in
  let messages_json = "[" ^ (String.concat ", " message_json_list) ^ "]" in
  
  (* Build request body *)
  let body = Printf.sprintf 
    "{\"model\": \"%s\", \"messages\": %s, \"temperature\": %.2f, \"max_tokens\": %d, \"tools\": %s}"
    provider.model.name messages_json provider.temperature provider.max_tokens tools_json in
  
  (* Make HTTP request *)
  let response = Http.post url headers body provider.timeout in
  
  match response.Http.HttpResponse.error with
  | Some error -> Error error
  | None ->
      try
        (* Parse JSON response using Yojson *)
        if String.length response.Http.HttpResponse.body > 0 && response.Http.HttpResponse.status = 200 then
          let json = Yojson.Basic.from_string response.Http.HttpResponse.body in
          
          (* Extract fields from JSON *)
          let id = json |> member "id" |> to_string in
          let object_ = json |> member "object" |> to_string in
          let created = json |> member "created" |> to_int in
          let model = json |> member "model" |> to_string in
          
          (* Parse choices *)
          let choices_json = json |> member "choices" |> to_list in
          let choices = List.mapi (fun index choice_json ->
            let finish_reason = choice_json |> member "finish_reason" |> to_string in
            let message_json = choice_json |> member "message" in
            let role_str = message_json |> member "role" |> to_string in
            let content = message_json |> member "content" |> to_string in
            {
              index = index;
              message = { role = string_to_role role_str; content = content };
              finish_reason = finish_reason
            }
          ) choices_json in
          
          (* Parse usage *)
          let usage_json = json |> member "usage" in
          let usage_prompt_tokens = usage_json |> member "prompt_tokens" |> to_int in
          let usage_completion_tokens = usage_json |> member "completion_tokens" |> to_int in
          let usage_total_tokens = usage_json |> member "total_tokens" |> to_int in
          
          (* Parse system_fingerprint if present *)
          let system_fingerprint =
            try
              Some (json |> member "system_fingerprint" |> to_string)
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