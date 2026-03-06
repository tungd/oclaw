(** Agent module using effect handlers *)

open Effect.Deep
open Effects
open Llm_types
open Yojson.Safe.Util

module Log = (val Logs.src_log (Logs.Src.create "agent") : Logs.LOG)

(* {1 Session Management } *)

module Session = struct
  type t = {
    id : string;
    directory : string;
    mutable messages : Memory.conversation_message list;
    created_at : float;
    mutable last_active : float;
    mutex : Mutex.t;
  }

  let create id dir =
    {
      id;
      directory = dir;
      messages = [];
      created_at = Unix.gettimeofday ();
      last_active = Unix.gettimeofday ();
      mutex = Mutex.create ();
    }

  let add_message session msg =
    Mutex.lock session.mutex;
    session.messages <- msg :: session.messages;
    session.last_active <- Unix.gettimeofday ();
    Mutex.unlock session.mutex

  let get_history session ~limit =
    Mutex.lock session.mutex;
    let result = List.rev session.messages |> List.take limit in
    Mutex.unlock session.mutex;
    result

  let touch session =
    Mutex.lock session.mutex;
    session.last_active <- Unix.gettimeofday ();
    Mutex.unlock session.mutex
end

module Session_manager = struct
  type t = {
    sessions : (string, Session.t) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create () =
    { sessions = Hashtbl.create 16; mutex = Mutex.create () }

  let create_or_get mgr id dir =
    Mutex.lock mgr.mutex;
    let session = match Hashtbl.find_opt mgr.sessions id with
      | Some s -> s
      | None ->
          let s = Session.create id dir in
          Hashtbl.add mgr.sessions id s;
          s
    in
    Mutex.unlock mgr.mutex;
    session

  let get mgr id =
    Mutex.lock mgr.mutex;
    let result = Hashtbl.find_opt mgr.sessions id in
    Mutex.unlock mgr.mutex;
    result

  let list_all mgr =
    Mutex.lock mgr.mutex;
    let result = Hashtbl.fold (fun _ s acc -> s :: acc) mgr.sessions [] in
    Mutex.unlock mgr.mutex;
    result
end

(* {1 Knowledge Store } *)

module Knowledge = struct
  type t = {
    mutable identity : string;
    mutable agents : string option;
    mutable soul : string option;
    mutable user : string option;
    mutable skills : Skills.Skill.t list;
    mutable last_updated : float;
    mutex : Mutex.t;
  }

  let empty =
    {
      identity = "";
      agents = None;
      soul = None;
      user = None;
      skills = [];
      last_updated = 0.0;
      mutex = Mutex.create ();
    }

  let load_workspace_file workspace_dir filename =
    let path = Filename.concat workspace_dir filename in
    if Sys.file_exists path then
      try
        let ch = open_in path in
        let content = really_input_string ch (in_channel_length ch) in
        close_in ch;
        Some content
      with _ -> None
    else None

  let load () =
    let workspace_dir = "workspace" in
    let identity = Option.value ~default:"You are OClaw, an AI assistant." (load_workspace_file workspace_dir "IDENTITY.md") in
    let agents = load_workspace_file workspace_dir "AGENTS.md" in
    let soul = load_workspace_file workspace_dir "SOUL.md" in
    let user = load_workspace_file workspace_dir "USER.md" in
    let skills = Skills.load_skills () in
    {
      identity;
      agents;
      soul;
      user;
      skills;
      last_updated = Unix.gettimeofday ();
      mutex = Mutex.create ();
    }

  let refresh knowledge =
    Mutex.lock knowledge.mutex;
    let new_knowledge = load () in
    knowledge.identity <- new_knowledge.identity;
    knowledge.agents <- new_knowledge.agents;
    knowledge.soul <- new_knowledge.soul;
    knowledge.user <- new_knowledge.user;
    knowledge.skills <- new_knowledge.skills;
    knowledge.last_updated <- Unix.gettimeofday ();
    Mutex.unlock knowledge.mutex

  let get_system_prompt knowledge =
    Mutex.lock knowledge.mutex;
    let parts = ref [knowledge.identity] in
    Option.iter (fun a -> parts := !parts @ [a]) knowledge.agents;
    Option.iter (fun s -> parts := !parts @ [s]) knowledge.soul;
    Option.iter (fun u -> parts := !parts @ [u]) knowledge.user;
    if knowledge.skills <> [] then
      parts := !parts @ [Skills.skills_to_prompt knowledge.skills];
    let result = String.concat "\n\n---\n\n" !parts in
    Mutex.unlock knowledge.mutex;
    result
end

module Subagent_runtime = struct
  type running_task = {
    id : string;
    mutable cancel_requested : bool;
  }

  type outcome =
    | Completed of string
    | Failed of string
    | Canceled of string

  type t = {
    running : (string, running_task) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create () =
    { running = Hashtbl.create 32; mutex = Mutex.create () }

  let with_lock t f =
    Mutex.lock t.mutex;
    try
      let v = f () in
      Mutex.unlock t.mutex;
      v
    with exn ->
      Mutex.unlock t.mutex;
      raise exn

  let request_cancel t id =
    with_lock t (fun () ->
      match Hashtbl.find_opt t.running id with
      | None -> false
      | Some task ->
          task.cancel_requested <- true;
          true)

  let spawn t ~task_id ~run ~on_finish =
    with_lock t (fun () ->
      Hashtbl.replace t.running task_id { id = task_id; cancel_requested = false });
    let _ =
      Domain.spawn (fun () ->
        let cancelled () =
          with_lock t (fun () ->
            match Hashtbl.find_opt t.running task_id with
            | Some task -> task.cancel_requested
            | None -> true)
        in
        let outcome =
          try
            if cancelled () then Canceled "Subagent canceled before execution"
            else
              match run ~cancel_requested:cancelled with
              | Ok output ->
                  if cancelled () then Canceled "Subagent canceled"
                  else Completed output
              | Error err ->
                  if cancelled () then Canceled "Subagent canceled"
                  else Failed err
          with exn ->
            Failed (Printexc.to_string exn)
        in
        with_lock t (fun () ->
          Hashtbl.remove t.running task_id);
        try
          on_finish outcome
        with exn ->
          Log.err (fun m ->
            m "Subagent on_finish callback failed for %s: %s"
              task_id
              (Printexc.to_string exn)))
    in
    ()
end

(* {1 Agent } *)

module Agent = struct
  let max_tool_rounds_default = 64

  type t = {
    llm_config : Llm_provider.provider_config;
    session_manager : Session_manager.t;
    knowledge : Knowledge.t;
    model : string;
    default_temperature : float;
    max_tokens : int;
    subagents : Subagent_runtime.t;
  }

  let create ~llm_config ~model ?(temperature=0.7) ?(max_tokens=4096) () =
    let session_manager = Session_manager.create () in
    let knowledge = Knowledge.load () in
    let subagents = Subagent_runtime.create () in
    { llm_config; session_manager; knowledge; model; default_temperature = temperature; max_tokens; subagents }

  let refresh_knowledge agent =
    Knowledge.refresh agent.knowledge

  let build_messages agent session content =
    let system_prompt = Knowledge.get_system_prompt agent.knowledge in
    let history = Session.get_history session ~limit:10 in
    let history_msgs = List.map (fun item ->
      let role = match item.Memory.role with
        | "user" -> User
        | "assistant" -> Assistant
        | "system" -> System
        | "tool" -> Tool
        | _ -> User
      in
      { role; content = item.Memory.content; tool_calls = None; tool_call_id = None }
    ) history in
    let system_msg = { role = System; content = system_prompt; tool_calls = None; tool_call_id = None } in
    let user_msg = { role = User; content; tool_calls = None; tool_call_id = None } in
    system_msg :: (history_msgs @ [user_msg])

  let make_memory_message role content : Memory.conversation_message =
    {
      role;
      content;
      timestamp = Unix.gettimeofday ();
      importance = if role = "assistant" || role = "system" then 1.0 else 0.5;
      estimated_tokens = max 1 (String.length content / 4);
      metadata = [];
    }

  let get_tools_json ?(exclude=[]) () =
    let all_tools =
      Tools.get_all_tools ()
      |> List.filter (fun (name, _) -> not (List.exists (String.equal name) exclude))
    in
    List.map (fun (name, desc) ->
      let tool = match Tools.get_tool name with
        | Some t -> t
        | None -> raise Not_found
      in
      let parameters = match tool.Tools.parameters with
        | [] -> `Assoc [("type", `String "object"); ("properties", `Assoc []); ("required", `List [])]
        | params ->
            let props = `Assoc (List.map (fun (pname, pschema) ->
              match pschema with
              | `String s ->
                  let parts = String.split_on_char ':' s in
                  if List.length parts >= 2 then
                    let ptype = String.trim (List.hd parts) in
                    let pdesc = String.trim (String.concat ":" (List.tl parts)) in
                    (pname, `Assoc [("type", `String ptype); ("description", `String pdesc)])
                  else
                    (pname, `Assoc [("type", `String "string"); ("description", `String s)])
              | _ -> (pname, `Assoc [("type", `String "string"); ("description", `String "")])
            ) params)
            in
            `Assoc [
              ("type", `String "object");
              ("properties", props);
              ("required", `List (List.map (fun (n, _) -> `String n) params))
            ]
      in
      { type_ = "function"; function_ = { name = tool.Tools.name; description = tool.Tools.description; parameters } }
    ) all_tools

  let llm_tools_to_json (tools : Llm_types.tool_definition list) : Yojson.Safe.t =
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

  let parse_tool_args tool_call =
    try Yojson.Safe.from_string tool_call.Llm_types.function_args
    with _ -> `Assoc []

  let normalize_json_object_string raw =
    let raw = String.trim raw in
    if raw = "" then "{}"
    else
      try
        match Yojson.Safe.from_string raw with
        | `Assoc _ as json -> Yojson.Safe.to_string json
        | _ -> "{}"
      with _ -> "{}"

  let sanitize_llm_tool_call_args (call : Llm_types.tool_call) : Llm_types.tool_call =
    { call with function_args = normalize_json_object_string call.function_args }

  let sanitize_provider_tool_call_args (call : Llm_provider.llm_tool_call) : Llm_provider.llm_tool_call =
    { call with function_args = normalize_json_object_string call.function_args }

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

  let preview s =
    if String.length s > 220 then String.sub s 0 220 ^ "..."
    else s

  let execute_tool_call_direct (call : Llm_types.tool_call) : Session_types.tool_response =
    try
      let args_json = parse_tool_args call in
      let result_str = Tools.execute_tool call.function_name args_json in
      let response = classify_tool_result call.function_name result_str in
      (match response.error with
       | Some err ->
           Log.warn (fun m ->
             m "Tool %s returned error: %s | result=%s"
               call.function_name
               (preview err)
               (preview result_str))
       | None ->
           Log.debug (fun m ->
             m "Tool %s success result=%s"
               call.function_name
               (preview result_str)));
      response
    with exn ->
      { Session_types.name = call.function_name; result = ""; error = Some (Printexc.to_string exn) }

  let json_string_opt json field =
    match member field json with
    | `String s ->
        let s = String.trim s in
        if s = "" then None else Some s
    | _ -> None

  let with_task_service f =
    match Task_service.get_default () with
    | Some service -> f service
    | None -> Error "Task service is not initialized"

  let subagent_status_of_task_status status =
    match Task_types.normalize_status status with
    | "closed" -> "completed"
    | "failed" -> "failed"
    | "canceled" -> "killed"
    | "in_progress" | "open" | "blocked" | "deferred" -> "running"
    | other -> other

  let subagent_json_of_task (task : Task_types.task) =
    let status = subagent_status_of_task_status task.status in
    `Assoc [
      ("id", `String task.id);
      ("label", `String task.title);
      ("task", `String task.description);
      ("status", `String status);
      ("result", match task.result with Some s -> `String s | None -> `Null);
      ("error", match task.error with Some s -> `String s | None -> `Null);
      ("created_at", `Float task.created_at);
      ("started_at", match task.started_at with Some s -> `Float s | None -> `Null);
      ("finished_at", match task.closed_at with Some s -> `Float s | None -> `Null);
    ]

  let run_subagent_task agent ~subagent_tools_json ~task ~cancel_requested =
    let llm_config = {
      agent.llm_config with
      temperature = agent.default_temperature;
      max_tokens = agent.max_tokens;
    } in
    let max_tool_rounds = max_tool_rounds_default in
    let rec resolve_with_tools current_messages rounds_remaining =
      if cancel_requested () then
        Error "Subagent canceled"
      else
        match Llm_provider.call_llm llm_config current_messages ~tools:(Some subagent_tools_json) () with
        | Llm_provider.Error err -> Error err
        | Llm_provider.Success resp ->
            if resp.Llm_provider.choices = [] then
              Error "No choices in subagent LLM response"
            else
              let choice = List.hd resp.Llm_provider.choices in
              let tool_calls =
                List.map sanitize_provider_tool_call_args choice.Llm_provider.message.Llm_provider.tool_calls
              in
              if tool_calls = [] then
                Ok choice.Llm_provider.message.Llm_provider.content
              else if rounds_remaining <= 0 then
                Error "Subagent tool-call recursion limit exceeded"
              else
                let tool_messages = List.map (fun tool_call ->
                  let args_json =
                    try Yojson.Safe.from_string tool_call.Llm_provider.function_args
                    with _ -> `Assoc []
                  in
                  let result = Tools.execute_tool tool_call.Llm_provider.function_name args_json in
                  {
                    Llm_provider.role = Llm_provider.Tool;
                    content = result;
                    Llm_provider.tool_call_id = Some tool_call.Llm_provider.id;
                    Llm_provider.tool_calls = [];
                  }
                ) tool_calls in
                let assistant_message =
                  { choice.Llm_provider.message with tool_calls = tool_calls }
                in
                let next_messages = current_messages @ [assistant_message] @ tool_messages in
                resolve_with_tools next_messages (rounds_remaining - 1)
    in
    let subagent_messages = [
      {
        Llm_provider.role = Llm_provider.System;
        content = "You are a background subagent. Complete the delegated task independently. Use tools when useful, then return a concise final result.";
        Llm_provider.tool_call_id = None;
        Llm_provider.tool_calls = [];
      };
      {
        Llm_provider.role = Llm_provider.User;
        content = task;
        Llm_provider.tool_call_id = None;
        Llm_provider.tool_calls = [];
      };
    ] in
    resolve_with_tools subagent_messages max_tool_rounds

  let handle_subagent_tool_call agent ~subagent_tools_json (call : Llm_types.tool_call)
      : Session_types.tool_response option =
    let args = parse_tool_args call in
    match call.function_name with
    | "spawn" ->
        (match json_string_opt args "task" with
         | None ->
             Some { Session_types.name = call.function_name; result = ""; error = Some "task is required" }
         | Some task ->
             let label = json_string_opt args "label" in
             let title =
               match label with
               | Some s -> s
               | None ->
                   if String.length task <= 80 then task
                   else String.sub task 0 80 ^ "..."
             in
             let payload = `Assoc [
               ("kind", `String "subagent");
               ("title", `String title);
               ("description", `String task);
               ("status", `String "in_progress");
               ("priority", `Int 2);
               ("created_by", `String "agent");
               ("actor", `String "agent");
             ] in
             (match
                try Ok (Effects.task_operation ~operation:"task_create" ~payload)
                with Failure msg -> Error msg
              with
              | Error err ->
                  Some { Session_types.name = call.function_name; result = ""; error = Some err }
              | Ok created ->
                  let task_id =
                    try created |> member "task" |> member "id" |> to_string
                    with _ -> ""
                  in
                  let created_at =
                    try created |> member "task" |> member "created_at" |> to_float
                    with _ -> Unix.gettimeofday ()
                  in
                  if task_id = "" then
                    Some {
                      Session_types.name = call.function_name;
                      result = "";
                      error = Some "Failed to create subagent task";
                    }
                  else (
                    Subagent_runtime.spawn
                      agent.subagents
                      ~task_id
                      ~run:(fun ~cancel_requested ->
                        run_subagent_task agent ~subagent_tools_json ~task ~cancel_requested)
                      ~on_finish:(function
                        | Subagent_runtime.Completed output ->
                            (match Task_service.get_default () with
                             | None -> ()
                             | Some service ->
                                 ignore (Task_service.complete_task service ~id:task_id ~result:(Some output) ~actor:"subagent"))
                        | Subagent_runtime.Failed err ->
                            (match Task_service.get_default () with
                             | None -> ()
                             | Some service ->
                                 ignore (Task_service.fail_task service ~id:task_id ~error:err ~actor:"subagent"))
                        | Subagent_runtime.Canceled reason ->
                            (match Task_service.get_default () with
                             | None -> ()
                             | Some service ->
                                 let req : Task_types.task_update_request = {
                                   title = None;
                                   description = None;
                                   status = Some "canceled";
                                   priority = None;
                                   assignee = None;
                                   close_reason = Some reason;
                                   result = None;
                                   error = Some reason;
                                 } in
                                 ignore (Task_service.update_task service ~id:task_id ~req ~actor:"subagent")));
                    let payload =
                      `Assoc [
                        ("status", `String "ok");
                        ("id", `String task_id);
                        ("label", match label with Some s -> `String s | None -> `Null);
                        ("created_at", `Float created_at);
                        ("message", `String (Printf.sprintf "Spawned subagent %s" task_id));
                      ]
                      |> Yojson.Safe.pretty_to_string
                    in
                    Some { Session_types.name = call.function_name; result = payload; error = None })))
    | "subagent_list" ->
        let status = json_string_opt args "status" in
        let list_result =
          with_task_service (fun service ->
            let req = Task_types.parse_list_request ~kind:"subagent" () in
            Task_service.list_tasks service req)
        in
        (match list_result with
         | Error err ->
             Some { Session_types.name = call.function_name; result = ""; error = Some err }
         | Ok tasks ->
             let tasks =
               match status with
               | None -> tasks
               | Some desired ->
                   let desired = String.lowercase_ascii (String.trim desired) in
                   List.filter (fun (t : Task_types.task) ->
                     String.equal desired (subagent_status_of_task_status t.status)
                   ) tasks
             in
             let result =
               `Assoc [
                 ("count", `Int (List.length tasks));
                 ("tasks", `List (List.map subagent_json_of_task tasks))
               ]
               |> Yojson.Safe.pretty_to_string
             in
             Some { Session_types.name = call.function_name; result; error = None })
    | "subagent_manage" ->
        (match json_string_opt args "action", json_string_opt args "id" with
         | None, _ ->
             Some { Session_types.name = call.function_name; result = ""; error = Some "action is required" }
         | _, None ->
             Some { Session_types.name = call.function_name; result = ""; error = Some "id is required" }
         | Some action, Some id ->
             let action = String.lowercase_ascii action in
             let handled =
               with_task_service (fun service ->
                 match action with
                 | "status" ->
                     (match Task_service.get_task service id with
                      | Ok (Some task) -> Ok (subagent_json_of_task task)
                      | Ok None -> Error (Printf.sprintf "Subagent not found: %s" id)
                      | Error e -> Error e)
                 | "kill" ->
                     ignore (Subagent_runtime.request_cancel agent.subagents id);
                     (match Task_service.cancel_task service ~id ~actor:"agent" with
                      | Ok task -> Ok (subagent_json_of_task task)
                      | Error e -> Error e)
                 | _ -> Error "Invalid action. Expected: status | kill")
             in
             (match handled with
              | Ok json ->
                  Some {
                    Session_types.name = call.function_name;
                    result = (`Assoc [("status", `String "ok"); ("task", json)] |> Yojson.Safe.pretty_to_string);
                    error = None;
                  }
              | Error err ->
                  Some { Session_types.name = call.function_name; result = ""; error = Some err }))
    | _ -> None

  let rec process_query_internal agent session_id content =
    let session = Session_manager.create_or_get agent.session_manager session_id "." in
    Session.touch session;

    let messages = build_messages agent session content in
    let tools = get_tools_json () in
    let subagent_tools =
      get_tools_json ~exclude:["spawn"; "subagent_list"; "subagent_manage"] ()
    in
    let subagent_tools_json = llm_tools_to_json subagent_tools in

    let max_tool_rounds = max_tool_rounds_default in

    let rec resolve_with_tools current_messages rounds_remaining =
      let llm_req = {
        model = agent.model;
        messages = current_messages;
        tools = Some tools;
        temperature = Some agent.default_temperature;
        max_tokens = Some agent.max_tokens;
      } in

      let llm_resp = Effects.llm_request llm_req in
      if llm_resp.choices = [] then
        raise (Failure "No choices in LLM response");

      let choice = List.hd llm_resp.choices in
      let response_content = choice.message.content in

      match choice.message.tool_calls with
      | Some calls when calls <> [] ->
          let calls = List.map sanitize_llm_tool_call_args calls in
          if rounds_remaining <= 0 then
            raise (Failure "Tool-call recursion limit exceeded");
          Log.debug (fun m -> m "Processing %d tool calls" (List.length calls));
          let batch_started = Unix.gettimeofday () in

          (* Execute tools in parallel using domains *)
          let tool_results = List.map (fun (call : Llm_types.tool_call) ->
            Domain.spawn (fun () ->
              let started = Unix.gettimeofday () in
              Log.debug (fun m ->
                m "Tool worker start id=%s name=%s t=%.6f"
                  call.id call.function_name started);
              let resp =
                match handle_subagent_tool_call agent ~subagent_tools_json call with
                | Some r -> r
                | None -> execute_tool_call_direct call
              in
              let finished = Unix.gettimeofday () in
              let elapsed_ms = (finished -. started) *. 1000.0 in
              Log.debug (fun m ->
                m "Tool worker finish id=%s name=%s t=%.6f elapsed_ms=%.2f status=%s"
                  call.id
                  call.function_name
                  finished
                  elapsed_ms
                  (match resp.error with Some _ -> "error" | None -> "ok"));
              resp)
          ) calls in

          (* Wait for all tool results *)
          let results = List.map (fun d -> Domain.join d) tool_results in
          let batch_finished = Unix.gettimeofday () in
          Log.debug (fun m ->
            m "Tool batch complete count=%d elapsed_ms=%.2f"
              (List.length calls)
              ((batch_finished -. batch_started) *. 1000.0));

          (* Build tool response messages *)
          let tool_msgs =
            List.map2
              (fun (call : Llm_types.tool_call) (result : Session_types.tool_response) ->
                let content =
                  match result.error with
                  | Some e when result.result = "" -> "Error: " ^ e
                  | Some e when String.trim result.result = String.trim e -> "Error: " ^ e
                  | Some e -> result.result ^ "\nError: " ^ e
                  | None -> result.result
                in
                {
                  role = Llm_types.Tool;
                  content;
                  tool_calls = None;
                  tool_call_id = Some call.id;
                }
              )
              calls results
          in

          let assistant_message =
            { choice.message with tool_calls = Some calls }
          in
          let next_messages = current_messages @ [assistant_message] @ tool_msgs in
          resolve_with_tools next_messages (rounds_remaining - 1)

      | _ -> response_content
    in

    Session.add_message session (make_memory_message "user" content);
    let final_content = resolve_with_tools messages max_tool_rounds in
    Session.add_message session (make_memory_message "assistant" final_content);
    final_content

  let process_query agent ~session_id ~content =
    Effects.run_with_handlers ~llm_config:agent.llm_config (fun () ->
      try
        Ok (process_query_internal agent session_id content)
      with
      | Failure msg -> Error msg
      | exn -> Error (Printexc.to_string exn)
    )

  let list_sessions agent =
    Mutex.lock agent.session_manager.Session_manager.mutex;
    let sessions = Hashtbl.fold (fun id session acc ->
      let info : Session_types.session_info = {
        id = session.Session.id;
        directory = session.Session.directory;
        created_at = session.Session.created_at;
        last_active = session.Session.last_active;
        message_count = List.length session.Session.messages;
      } in
      (id, info) :: acc
    ) agent.session_manager.Session_manager.sessions [] in
    Mutex.unlock agent.session_manager.Session_manager.mutex;
    List.rev sessions

  let get_knowledge agent =
    agent.knowledge
end

type t = Agent.t

let create = Agent.create
let refresh_knowledge = Agent.refresh_knowledge
let process_query = Agent.process_query
let list_sessions = Agent.list_sessions
let get_knowledge = Agent.get_knowledge
