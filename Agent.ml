(** Agent module using effect handlers *)

open Effect.Deep
open Effects
open Llm_types

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

(* {1 Agent } *)

module Agent = struct
  type t = {
    llm_config : Llm_provider.provider_config;
    session_manager : Session_manager.t;
    knowledge : Knowledge.t;
    model : string;
    default_temperature : float;
    max_tokens : int;
  }

  let create ~llm_config ~model ?(temperature=0.7) ?(max_tokens=4096) () =
    let session_manager = Session_manager.create () in
    let knowledge = Knowledge.load () in
    { llm_config; session_manager; knowledge; model; default_temperature = temperature; max_tokens }

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

  let get_tools_json () =
    let all_tools = Tools.get_all_tools () in
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
                    let ptype = String.trim (List.hd (List.rev parts)) in
                    let pdesc = String.trim (String.concat ":" (List.rev (List.tl (List.rev parts)))) in
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

  let rec process_query_internal agent session_id content =
    let session = Session_manager.create_or_get agent.session_manager session_id "." in
    Session.touch session;

    let messages = build_messages agent session content in
    let tools = get_tools_json () in

    (* Call LLM using effect *)
    let llm_req = {
      model = agent.model;
      messages;
      tools = Some tools;
      temperature = Some agent.default_temperature;
      max_tokens = Some agent.max_tokens;
    } in

    let llm_resp = Effects.llm_request llm_req in

    (* Get the main response *)
    if llm_resp.choices = [] then
      raise (Failure "No choices in LLM response");

    let main_choice = List.hd llm_resp.choices in
    let response_content = main_choice.message.content in

    (* Handle tool calls *)
    (match main_choice.message.tool_calls with
     | Some calls when calls <> [] ->
         Log.debug (fun m -> m "Processing %d tool calls" (List.length calls));

         (* Save assistant message with tool calls *)
         Session.add_message session (make_memory_message "assistant" response_content);

         (* Execute tools in parallel using domains *)
         let tool_results = List.map (fun call ->
           Domain.spawn (fun () ->
             Effects.tool_call call
           )
         ) calls in

         (* Wait for all tool results *)
         let results = List.map (fun d -> Domain.join d) tool_results in

         (* Build tool response messages *)
         let tool_msgs =
           List.map2
             (fun (call : Llm_types.tool_call) (result : Session_types.tool_response) ->
               {
                 role = Llm_types.Tool;
                 content = result.result;
                 tool_calls = None;
                 tool_call_id = Some call.id;
               }
             )
             calls results
         in

         (* Add user message and tool results to history *)
         Session.add_message session (make_memory_message "user" content);

         (* Continue conversation with tool results *)
         let messages_with_tools = messages @ [main_choice.message] @ tool_msgs in
         let followup_req = {
           model = agent.model;
           messages = messages_with_tools;
           tools = Some tools;
           temperature = Some agent.default_temperature;
           max_tokens = Some agent.max_tokens;
         } in

         let followup_resp = Effects.llm_request followup_req in
         let followup_choice = List.hd followup_resp.choices in
         let final_content = followup_choice.message.content in

         (* Save final response *)
         Session.add_message session (make_memory_message "assistant" final_content);

         final_content

     | _ ->
         (* No tool calls, save conversation and return *)
         Session.add_message session (make_memory_message "user" content);
         Session.add_message session (make_memory_message "assistant" response_content);

         response_content)

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
