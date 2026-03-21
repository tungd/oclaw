module Llm = Llm_types
module Log = (val Logs.src_log (Logs.Src.create "agent_engine") : Logs.LOG)

let default_system_prompt =
  String.concat "\n"
    [
      "You are OClaw, a CLI-first assistant. Use tools when needed, verify results before claiming success, and report concrete outcomes.";
      "";
      "# Operational Guidance";
      "";
      "- Use tools when they reduce uncertainty or perform the requested action.";
      "- Do not claim success until the tool result confirms it.";
      "- For multi-step work, keep your actions ordered and summarize progress accurately.";
      "- Prefer editing targeted files over rewriting unrelated code.";
      "- Keep answers concise and directly tied to the request.";
      "";
      "# Tool Error Handling";
      "";
      "- When a tool fails, read the error message AND the recovery hint carefully.";
      "- Follow the recovery hint to fix the issue (e.g., verify file exists, check permissions, provide more context).";
      "- For file operations: use `bash` with `ls` to verify paths before reading/writing.";
      "- For edit operations: ensure `old_text` is unique by including more surrounding context.";
      "- For command failures: check the command output for specific errors before retrying.";
      "- Don't retry the same failing tool call with identical parameters - fix the root cause first.";
    ]

let build_system_prompt state ~chat_id:_ =
  match state.Runtime.system_prompt_override with
  | Some prompt -> prompt
  | None ->
      let skills_catalog = Tools.build_skills_catalog state.Runtime.tools in
      if String.trim skills_catalog = "" then
        default_system_prompt
      else
        default_system_prompt
        ^ "\n\n# Skills\n\nThe following skills are available. Use `activate_skill` before following a skill-specific workflow. Users can also explicitly activate a skill with `/skill <name>` or `$skill-name`.\n\n"
        ^ skills_catalog

let response_text response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } -> Some text
         | Llm.Response_tool_use _ -> None)
  |> String.concat ""

let truncate_for_llm ?(max_len=8192) output =
  if String.length output > max_len then
    String.sub output 0 max_len
    ^ Printf.sprintf "\n... (output truncated, %d bytes total - see transcript for full output)" (String.length output)
  else
    output

let truncate_message_content = function
  | Llm.Text_content s -> Llm.Text_content (truncate_for_llm s)
  | Llm.Blocks blocks ->
      Llm.Blocks
        (List.map
           (function
             | Llm.Text { text } -> Llm.Text { text = truncate_for_llm text }
             | Llm.Image _ as img -> img
             | Llm.Tool_use tu -> Llm.Tool_use tu
             | Llm.Tool_result tr -> Llm.Tool_result { tr with content = truncate_for_llm tr.content })
           blocks)

let assistant_blocks_of_response response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } when String.trim text <> "" -> Some (Llm.Text { text })
         | Llm.Response_text _ -> None
         | Llm.Response_tool_use { id; name; input } -> Some (Llm.Tool_use { id; name; input }))

let make_text_response state ~parent_id text =
  let final_text = if String.trim text = "" then "(empty_reply)" else text in
  let assistant_message_content = Llm.Text_content final_text in
  let model = state.Runtime.provider_config.Llm_provider.model_name in
  ignore
    (Transcript.add_llm_response state.Runtime.transcript ~parent_id ~model ~content:assistant_message_content ());
  Ok final_text

let activate_skill_response state ~chat_id ~parent_id name =
  let result = Tools.activate_skill state.Runtime.tools ~chat_id name in
  make_text_response state ~parent_id result.Tools.content

let with_pending_permissions state f =
  Mutex.lock state.Runtime.pending_permissions_mutex;
  try
    let result = f state.Runtime.pending_permissions in
    Mutex.unlock state.Runtime.pending_permissions_mutex;
    result
  with exn ->
    Mutex.unlock state.Runtime.pending_permissions_mutex;
    raise exn

let get_pending_permission state ~chat_id =
  with_pending_permissions state (fun pending -> Hashtbl.find_opt pending chat_id)

let set_pending_permission state ~chat_id pending_permission =
  with_pending_permissions state (fun pending ->
      Hashtbl.replace pending chat_id pending_permission)

let take_pending_permission state ~chat_id =
  with_pending_permissions state (fun pending ->
      let value = Hashtbl.find_opt pending chat_id in
      Hashtbl.remove pending chat_id;
      value)

let session_id_of_chat chat_id =
  string_of_int chat_id

let tool_kind_of_name = function
  | "read_file" -> Acp.Message.Read
  | "write_file" | "edit_file" -> Acp.Message.Edit
  | "bash" -> Acp.Message.Execute
  | "skill_search" -> Acp.Message.Search
  | "skill_install" -> Acp.Message.Other
  | _ -> Acp.Message.Other

let title_of_tool_call name =
  "Tool: " ^ name

let tool_call_payload ~tool_call_id ~name ~input ?status ?content ?raw_output () =
  {
    Acp.Message.tool_call_id;
    title = Some (title_of_tool_call name);
    kind = Some (tool_kind_of_name name);
    status;
    content;
    raw_input = Some input;
    raw_output;
  }

let tool_result_content result =
  if String.trim result.Tools.content = "" then
    None
  else
    Some [ Acp.Message.Content (Acp.Message.Text result.Tools.content) ]

let tool_result_raw_output result =
  Some
    (`Assoc
       [
         ("content", `String result.Tools.content);
         ("isError", `Bool result.Tools.is_error);
       ])

let emit_tool_call emit ~chat_id tool_call =
  emit
    (Acp.Message.Session_update
       { session_id = session_id_of_chat chat_id; update = Acp.Message.Tool_call tool_call })

let emit_tool_call_update emit ~chat_id tool_call =
  emit
    (Acp.Message.Session_update
       { session_id = session_id_of_chat chat_id; update = Acp.Message.Tool_call_update tool_call })

let emit_permission_request emit ~chat_id tool_call =
  emit
    (Acp.Message.Request_permission
       {
         session_id = session_id_of_chat chat_id;
         tool_call;
         options =
           [
             { Acp.Message.option_id = "allow-once"; name = "Allow once"; kind = Acp.Message.Allow_once };
             { Acp.Message.option_id = "reject-once"; name = "Reject"; kind = Acp.Message.Reject_once };
           ];
       })

let tool_result_block tool_use_id tool_result =
  let enhanced_content =
    if tool_result.Tools.is_error then
      let base = tool_result.Tools.content in
      match tool_result.Tools.recovery_hint with
      | Some hint -> Printf.sprintf "%s\n\nRecovery hint: %s" base hint
      | None -> base
    else
      tool_result.Tools.content
  in
  Llm.Tool_result
    {
      tool_use_id;
      content = enhanced_content;
      is_error = if tool_result.Tools.is_error then Some true else None;
    }

let emit_final_message emit ~chat_id content =
  emit (Acp.Message.Agent_message { content; chat_id = Some chat_id });
  emit Acp.Message.Done

let approve_request state request =
  match request.Tools.scope with
  | Tools.Execute -> Tools.approve_executable state.Runtime.tools request.target
  | Tools.Read -> Tools.approve_root state.Runtime.tools ~scope:Tools.Read request.target
  | Tools.Write -> Tools.approve_root state.Runtime.tools ~scope:Tools.Write request.target
  | Tools.Install -> Tools.approve_install state.Runtime.tools request.target

let rejection_result request =
  Tools.failure
    ~error_type:"permission_rejected"
    ~error_category:Tools.ApprovalRequired
    (Printf.sprintf "Permission rejected for %s" request.Tools.target)

let execute_tool state ~chat_id name input =
  try
    Tools.execute state.Runtime.tools ~chat_id name input
  with exn ->
    Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
    let msg = "Tool execution exception: " ^ Printexc.to_string exn in
    Tools.failure ~error_type:"exception" ~error_category:Tools.Other msg

let add_tool_result_node state ~parent_id ~tool_call_id result =
  let block = tool_result_block tool_call_id result in
  match block with
  | Llm.Tool_result { tool_use_id; content; is_error } ->
      let is_err = Option.value is_error ~default:false in
      Transcript.add_tool_result state.Runtime.transcript ~parent_id ~tool_use_id ~content ~is_error:is_err
  | _ -> parent_id

let rec continue_llm_loop ~emit state ~chat_id ~system_prompt current_node_id rounds_remaining =
  let messages = Transcript.get_branch state.Runtime.transcript current_node_id in
  let messages =
    List.map
      (fun (m : Llm.message) -> { m with content = truncate_message_content m.content })
      messages
  in
  match state.Runtime.llm_call state.provider_config ~emit ~system_prompt messages ~tools:(Tools.definitions state.Runtime.tools) with
  | Error err ->
      emit (Acp.Message.Error { message = err; code = 0 });
      Error err
  | Ok response ->
      let tool_calls =
        response.Llm.content
        |> List.filter_map (function
               | Llm.Response_tool_use { id; name; input } -> Some (id, name, input)
               | Llm.Response_text _ -> None)
      in
      if tool_calls <> [] then
        if rounds_remaining = 0 then
          let err = "Tool-call recursion limit exceeded" in
          emit (Acp.Message.Error { message = err; code = 0 });
          Error err
        else
          let assistant_message_content = Llm.Blocks (assistant_blocks_of_response response) in
          let model = state.provider_config.Llm_provider.model_name in
          let response_node_id =
            Transcript.add_llm_response
              state.Runtime.transcript
              ~parent_id:current_node_id
              ~model
              ~content:assistant_message_content
              ()
          in
          emit (Acp.Message.Status { status = "executing_tools"; message = None });
          execute_tool_calls
            ~emit
            state
            ~chat_id
            ~system_prompt
            ~rounds_remaining
            ~current_node_id:response_node_id
            tool_calls
      else
        let text = String.trim (response_text response) in
        match make_text_response state ~parent_id:current_node_id text with
        | Ok response_text ->
            emit_final_message emit ~chat_id response_text;
            Ok ()
        | Error err ->
            emit (Acp.Message.Error { message = err; code = 0 });
            Error err

and execute_tool_calls ~emit state ~chat_id ~system_prompt ~rounds_remaining ~current_node_id tool_calls =
  match tool_calls with
  | [] -> continue_llm_loop ~emit state ~chat_id ~system_prompt current_node_id (rounds_remaining - 1)
  | (tool_call_id, name, input) :: remaining ->
      let tool_call_id =
        if String.trim tool_call_id = "" then
          Printf.sprintf "tool-%f" (Unix.gettimeofday ())
        else
          tool_call_id
      in
      let created_tool_call =
        tool_call_payload ~tool_call_id ~name ~input ~status:Acp.Message.Pending ()
      in
      emit_tool_call emit ~chat_id created_tool_call;
      let tool_call_node_id =
        Transcript.add_tool_call state.Runtime.transcript ~parent_id:current_node_id ~tool_name:name ~input
      in
      let running_tool_call =
        { created_tool_call with Acp.Message.status = Some Acp.Message.In_progress }
      in
      let result = execute_tool state ~chat_id name input in
      match result.Tools.approval_request with
      | Some request ->
          let pending_tool_call =
            {
              created_tool_call with
              Acp.Message.content = tool_result_content result;
              status = Some Acp.Message.Pending;
            }
          in
          let resume outcome =
            match outcome with
            | Acp.Message.Cancelled ->
                emit (Acp.Message.Status { status = "cancelled"; message = Some "Permission request cancelled" });
                emit Acp.Message.Done;
                Ok ()
            | Acp.Message.Selected "allow-once" ->
                begin
                  match approve_request state request with
                  | Error err ->
                      emit (Acp.Message.Error { message = err; code = 0 });
                      Error err
                  | Ok _ ->
                      emit_tool_call_update emit ~chat_id running_tool_call;
                      let approved_result = execute_tool state ~chat_id name input in
                      let completed_tool_call =
                        {
                          running_tool_call with
                          Acp.Message.status =
                            Some (if approved_result.Tools.is_error then Acp.Message.Failed else Acp.Message.Completed);
                          content = tool_result_content approved_result;
                          raw_output = tool_result_raw_output approved_result;
                        }
                      in
                      emit_tool_call_update emit ~chat_id completed_tool_call;
                      let next_node_id =
                        add_tool_result_node state ~parent_id:tool_call_node_id ~tool_call_id approved_result
                      in
                      execute_tool_calls
                        ~emit
                        state
                        ~chat_id
                        ~system_prompt
                        ~rounds_remaining
                        ~current_node_id:next_node_id
                        remaining
                end
            | Acp.Message.Selected "reject-once" ->
                let rejected_result = rejection_result request in
                let failed_tool_call =
                  {
                    created_tool_call with
                    Acp.Message.status = Some Acp.Message.Failed;
                    content = tool_result_content rejected_result;
                    raw_output = tool_result_raw_output rejected_result;
                  }
                in
                emit_tool_call_update emit ~chat_id failed_tool_call;
                let next_node_id =
                  add_tool_result_node state ~parent_id:tool_call_node_id ~tool_call_id rejected_result
                in
                execute_tool_calls
                  ~emit
                  state
                  ~chat_id
                  ~system_prompt
                  ~rounds_remaining
                  ~current_node_id:next_node_id
                  remaining
            | Acp.Message.Selected option_id ->
                let err = "Unsupported permission option: " ^ option_id in
                emit (Acp.Message.Error { message = err; code = 0 });
                Error err
          in
          set_pending_permission state ~chat_id { Runtime.tool_call = pending_tool_call; request; resume };
          emit_permission_request emit ~chat_id pending_tool_call;
          Ok ()
      | None ->
          emit_tool_call_update emit ~chat_id running_tool_call;
          let completed_tool_call =
            {
              running_tool_call with
              Acp.Message.status =
                Some (if result.Tools.is_error then Acp.Message.Failed else Acp.Message.Completed);
              content = tool_result_content result;
              raw_output = tool_result_raw_output result;
            }
          in
          emit_tool_call_update emit ~chat_id completed_tool_call;
          let next_node_id =
            add_tool_result_node state ~parent_id:tool_call_node_id ~tool_call_id result
          in
          execute_tool_calls
            ~emit
            state
            ~chat_id
            ~system_prompt
            ~rounds_remaining
            ~current_node_id:next_node_id
            remaining

type approval_command_result =
  | Approval_text of (string, string) result
  | Approval_resume of (unit, string) result

let maybe_handle_approval_command state ~chat_id prompt =
  if not (String.starts_with ~prefix:"/approve " prompt) then
    None
  else
    let usage = "Usage: /approve <exec|read|write|install> <path-or-name>" in
    let rest = String.trim (String.sub prompt 9 (String.length prompt - 9)) in
    let scope, target =
      match String.index_opt rest ' ' with
      | None -> ("", "")
      | Some idx ->
          (String.sub rest 0 idx, String.trim (String.sub rest (idx + 1) (String.length rest - idx - 1)))
    in
    if scope = "" || target = "" then
      Some (Approval_text (Error usage))
    else
      match get_pending_permission state ~chat_id with
      | Some pending when pending.Runtime.request.target = target ->
          Some (Approval_resume (pending.resume (Acp.Message.Selected "allow-once")))
      | _ ->
          let response =
            match scope with
            | "exec" -> Tools.approve_executable state.Runtime.tools target
            | "read" -> Tools.approve_root state.Runtime.tools ~scope:Tools.Read target
            | "write" -> Tools.approve_root state.Runtime.tools ~scope:Tools.Write target
            | "install" -> Tools.approve_install state.Runtime.tools target
            | _ -> Error usage
          in
          Some (Approval_text response)

let maybe_handle_skill_command state ~chat_id ?parent_id prompt =
  let trimmed = String.trim prompt in
  let skill_name =
    if String.starts_with ~prefix:"/skill " trimmed then
      Some (String.trim (String.sub trimmed 7 (String.length trimmed - 7)))
    else if String.length trimmed > 1 && String.unsafe_get trimmed 0 = '$' then
      Some (String.trim (String.sub trimmed 1 (String.length trimmed - 1)))
    else
      None
  in
  match skill_name with
  | None -> None
  | Some "" -> Some (Error "Usage: /skill <name>")
  | Some name ->
      let prompt_node_id =
        match parent_id with
        | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
        | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
      in
      Some (activate_skill_response state ~chat_id ~parent_id:prompt_node_id name)

let process ~emit state ~chat_id ?(persistent=false) prompt =
  let prompt = String.trim prompt in
  if prompt = "" then begin
    emit (Acp.Message.Error { message = "Prompt is empty"; code = 0 });
    Error "Prompt is empty"
  end
  else
    let parent_id =
      if persistent then
        Transcript.get_latest_node state.Runtime.transcript ~chat_id
      else
        None
    in
    match maybe_handle_approval_command state ~chat_id prompt with
    | Some handled ->
        begin
          match handled with
          | Approval_text (Ok text) ->
              let prompt_node_id =
                match parent_id with
                | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
                | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
              in
              Result.map (fun response -> emit_final_message emit ~chat_id response) (make_text_response state ~parent_id:prompt_node_id text)
          | Approval_text (Error err) ->
              let prompt_node_id =
                match parent_id with
                | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
                | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
              in
              Result.map (fun response -> emit_final_message emit ~chat_id response) (make_text_response state ~parent_id:prompt_node_id err)
          | Approval_resume (Ok ()) -> Ok ()
          | Approval_resume (Error err) ->
              emit (Acp.Message.Error { message = err; code = 0 });
              Error err
        end
    | None ->
        begin
          match maybe_handle_skill_command state ~chat_id ?parent_id prompt with
          | Some handled ->
              Result.map (fun response -> emit_final_message emit ~chat_id response) handled
          | None ->
              let prompt_node_id =
                match parent_id with
                | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
                | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
              in
              let system_prompt = build_system_prompt state ~chat_id in
              emit (Acp.Message.Status { status = "thinking"; message = None });
              continue_llm_loop ~emit state ~chat_id ~system_prompt prompt_node_id state.config.max_tool_iterations
        end

let resolve_permission state ~chat_id outcome =
  match take_pending_permission state ~chat_id with
  | None -> Error "No pending permission request"
  | Some pending -> pending.Runtime.resume outcome
