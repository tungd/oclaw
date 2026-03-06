(** Effect handlers for async operations *)

open Effect
open Effect.Deep
open Llm_types
open Session_types

module Log = (val Logs.src_log (Logs.Src.create "effects") : Logs.LOG)

(* {1 Effect Definitions - no module aliases to avoid resolution issues } *)

type llm_effect_req = { req : Llm_types.llm_request }
type tool_effect_req = { call : Llm_types.tool_call }
type wait_effect_req = { seconds : float }
type skill_effect_req = { name : string }
type task_effect_req = { operation : string; payload : Yojson.Safe.t }

type _ Effect.t += Llm_request_effect : llm_effect_req -> Llm_types.llm_response Effect.t
type _ Effect.t += Tool_call_effect : tool_effect_req -> Session_types.tool_response Effect.t
type _ Effect.t += Agent_wait_effect : wait_effect_req -> unit Effect.t
type _ Effect.t += Load_skill_effect : skill_effect_req -> Skills.Skill.t option Effect.t
type _ Effect.t += Task_operation_effect : task_effect_req -> Yojson.Safe.t Effect.t

(* {1 Effect performance functions } *)

let llm_request (req : Llm_types.llm_request) : Llm_types.llm_response =
  perform (Llm_request_effect { req })

let tool_call (call : Llm_types.tool_call) : Session_types.tool_response =
  perform (Tool_call_effect { call })

let agent_wait (seconds : float) : unit =
  perform (Agent_wait_effect { seconds })

let load_skill (name : string) : Skills.Skill.t option =
  perform (Load_skill_effect { name })

let task_operation ~(operation : string) ~(payload : Yojson.Safe.t) : Yojson.Safe.t =
  perform (Task_operation_effect { operation; payload })

(* {1 Effect handlers } *)

let llm_to_provider_msg (llm_msg : Llm_types.llm_message) =
  let role =
    match llm_msg.role with
    | System -> Llm_provider.System
    | User -> Llm_provider.User
    | Assistant -> Llm_provider.Assistant
    | Tool -> Llm_provider.Tool
  in
  {
    Llm_provider.role = role;
    content = llm_msg.content;
    Llm_provider.tool_call_id = llm_msg.tool_call_id;
    Llm_provider.tool_calls =
      (match llm_msg.tool_calls with
       | None -> []
       | Some calls ->
           List.map (fun (tc : Llm_types.tool_call) ->
             {
               Llm_provider.id = tc.id;
               Llm_provider.type_ = tc.type_;
               Llm_provider.function_name = tc.function_name;
               Llm_provider.function_args = tc.function_args;
             }
           ) calls);
  }

let provider_to_llm_response (provider_response : Llm_provider.llm_response) =
  let choices = List.map (fun (pc : Llm_provider.llm_choice) ->
    {
      index = pc.index;
      message = {
        role = (match pc.message.role with
          | Llm_provider.System -> Llm_types.System
          | Llm_provider.User -> Llm_types.User
          | Llm_provider.Assistant -> Llm_types.Assistant
          | Llm_provider.Tool -> Llm_types.Tool);
        content = pc.message.content;
        tool_calls = Some (List.map (fun (tc : Llm_provider.llm_tool_call) ->
          { id = tc.Llm_provider.id; type_ = "function"; function_name = tc.Llm_provider.function_name; function_args = tc.Llm_provider.function_args }
        ) pc.message.tool_calls);
        tool_call_id = pc.message.tool_call_id;
      };
      finish_reason = Some pc.finish_reason;
    }
  ) provider_response.choices in
  {
    id = "response";
    object_ = "chat.completion";
    created = (int_of_float (Unix.gettimeofday ()));
    model = provider_response.model;
    choices;
    usage = None;
  }

let handle_llm_request ~llm_config (req : Llm_types.llm_request) =
  Log.debug (fun m -> m "Handling LLM request with %d messages" (List.length req.messages));
  let provider_msgs = List.map llm_to_provider_msg req.messages in
  let tools_json = Option.map (fun (tools : Llm_types.tool_definition list) ->
    `List (List.map (fun (tool : Llm_types.tool_definition) ->
      `Assoc [
        ("type", `String tool.type_);
        ("function", `Assoc [
          ("name", `String tool.function_.name);
          ("description", `String tool.function_.description);
          ("parameters", tool.function_.parameters);
        ])
      ]
    ) tools)
  ) req.tools in

  let provider_result = match tools_json with
  | Some tools -> Llm_provider.call_llm llm_config provider_msgs ~tools:(Some tools) ()
  | None -> Llm_provider.call_llm llm_config provider_msgs ()
  in

  match provider_result with
  | Llm_provider.Error err ->
      Log.err (fun m -> m "LLM provider error: %s" err);
      raise (Failure err)
  | Llm_provider.Success resp -> provider_to_llm_response resp

let handle_tool_call (tool_call : Llm_types.tool_call) =
  Log.debug (fun m -> m "Handling tool call: %s" tool_call.function_name);
  let classify_tool_result (name : string) (result_str : string) : Session_types.tool_response =
    let trimmed = String.trim result_str in
    let lower = String.lowercase_ascii trimmed in
    let is_tool_not_found =
      String.starts_with ~prefix:"Tool " result_str && String.ends_with ~suffix:" not found" result_str
    in
    let is_error_like =
      String.starts_with ~prefix:"error" lower
      || String.starts_with ~prefix:"invalid " lower
      || String.ends_with ~suffix:" is required" lower
    in
    if is_tool_not_found then
      { Session_types.name = name; result = ""; error = Some "Tool not found" }
    else if is_error_like then
      { Session_types.name = name; result = result_str; error = Some trimmed }
    else
      { Session_types.name = name; result = result_str; error = None }
  in
  try
    let args_json =
      try Yojson.Safe.from_string tool_call.function_args
      with _ -> `Assoc []
    in
    let result_str = Tools.execute_tool tool_call.function_name args_json in
    let resp = classify_tool_result tool_call.function_name result_str in
    (match resp.error with
     | Some err ->
         Log.warn (fun m -> m "Tool %s returned error: %s" tool_call.function_name err)
     | None -> ());
    resp
  with exn ->
    Log.err (fun m -> m "Tool execution error: %s" (Printexc.to_string exn));
    { Session_types.name = tool_call.function_name; result = ""; error = Some (Printexc.to_string exn) }

(* {1 Combined effect handler with configuration } *)

let run_with_handlers ~llm_config f =
  try_with f () {
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Llm_request_effect eff_req ->
          Some (fun (k : (a, _) continuation) ->
            let resp = handle_llm_request ~llm_config eff_req.req in
            continue k resp)
      | Tool_call_effect eff_call ->
          Some (fun (k : (a, _) continuation) ->
            let resp = handle_tool_call eff_call.call in
            continue k resp)
      | Agent_wait_effect eff_wait ->
          Some (fun (k : (a, _) continuation) ->
            Log.debug (fun m -> m "Agent waiting %.2f seconds" eff_wait.seconds);
            Unix.sleepf eff_wait.seconds;
            continue k ())
      | Load_skill_effect eff_skill ->
          Some (fun (k : (a, _) continuation) ->
            let skills = Skills.load_skills () in
            let skill_opt =
              List.find_opt
                (fun (s : Skills.Skill.t) -> String.equal s.name eff_skill.name)
                skills
            in
            continue k skill_opt)
      | Task_operation_effect task_req ->
          Some (fun (k : (a, _) continuation) ->
            match Task_service.dispatch_with_default ~operation:task_req.operation ~payload:task_req.payload with
            | Ok json -> continue k json
            | Error err -> raise (Failure err))
      | _ -> None
  }
