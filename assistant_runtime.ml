open Yojson.Safe

module Log = (val Logs.src_log (Logs.Src.create "assistant_runtime") : Logs.LOG)

type llm_call =
  Llm_provider.provider_config ->
  Llm_provider.message list ->
  tools:Yojson.Safe.t ->
  Llm_provider.llm_result

type t = {
  provider_config : Llm_provider.provider_config;
  llm_call : llm_call;
  system_prompt : string;
  history_limit : int;
  mutable history : Llm_provider.message list;
}

let default_system_prompt =
  "You are OClaw, a concise terminal assistant. Use the available tools when they help answer the user accurately."

let load_workspace_file filename =
  let path = Filename.concat "workspace" filename in
  if Sys.file_exists path then
    try
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          Some (really_input_string channel (in_channel_length channel)))
    with _ -> None
  else
    None

let build_system_prompt () =
  [ "IDENTITY.md"; "AGENTS.md"; "SOUL.md"; "USER.md" ]
  |> List.filter_map load_workspace_file
  |> function
  | [] -> default_system_prompt
  | parts -> String.concat "\n\n---\n\n" parts

let default_llm_call provider_config messages ~tools =
  Llm_provider.call_llm provider_config messages ~tools:(Some tools) ()

let trim_history history history_limit =
  let rec drop count items =
    if count <= history_limit then items
    else
      match items with
      | [] -> []
      | _ :: rest -> drop (count - 1) rest
  in
  drop (List.length history) history

let create ?(llm_call=default_llm_call) ?system_prompt ?(history_limit=12) ~provider_config () =
  {
    provider_config;
    llm_call;
    system_prompt = Option.value ~default:(build_system_prompt ()) system_prompt;
    history_limit;
    history = [];
  }

let system_message content =
  {
    Llm_provider.role = Llm_provider.System;
    content;
    tool_call_id = None;
    tool_calls = [];
  }

let user_message content =
  {
    Llm_provider.role = Llm_provider.User;
    content;
    tool_call_id = None;
    tool_calls = [];
  }

let assistant_message content =
  {
    Llm_provider.role = Llm_provider.Assistant;
    content;
    tool_call_id = None;
    tool_calls = [];
  }

let tool_message call_id content =
  {
    Llm_provider.role = Llm_provider.Tool;
    content;
    tool_call_id = Some call_id;
    tool_calls = [];
  }

let history runtime = runtime.history

let query runtime prompt =
  let prompt = String.trim prompt in
  if prompt = "" then Error "Prompt is empty"
  else
    let tools_json = Tools.tools_to_json () in
    let user_msg = user_message prompt in
    let base_messages = system_message runtime.system_prompt :: runtime.history @ [user_msg] in
    let rec resolve messages rounds_remaining =
      match runtime.llm_call runtime.provider_config messages ~tools:tools_json with
      | Llm_provider.Error err -> Error err
      | Llm_provider.Success response ->
          begin
            match response.Llm_provider.choices with
            | [] -> Error "No choices in LLM response"
            | choice :: _ ->
                let assistant_msg = choice.Llm_provider.message in
                if assistant_msg.Llm_provider.tool_calls <> [] then
                  if rounds_remaining = 0 then
                    Error "Tool-call recursion limit exceeded"
                  else
                    let tool_messages =
                      assistant_msg.Llm_provider.tool_calls
                      |> List.map (fun call ->
                           let args =
                             try Yojson.Safe.from_string call.Llm_provider.function_args
                             with _ -> `Assoc []
                           in
                           let output = Tools.execute_tool call.Llm_provider.function_name args in
                           tool_message call.Llm_provider.id output)
                    in
                    resolve (messages @ [assistant_msg] @ tool_messages) (rounds_remaining - 1)
                else
                  let response_text = String.trim assistant_msg.Llm_provider.content in
                  runtime.history <-
                    trim_history
                      (runtime.history @ [user_msg; assistant_message response_text])
                      runtime.history_limit;
                  Ok response_text
          end
    in
    resolve base_messages 16
