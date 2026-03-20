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
      let skills_catalog = Agent_skills.Skills.build_skills_catalog state.Runtime.skills in
      if String.trim skills_catalog = "" then
        default_system_prompt
      else
        default_system_prompt ^ "\n\n# Skills\n\nThe following skills are available. Use `activate_skill` before following a skill-specific workflow. Users can also explicitly activate a skill with `/skill <name>` or `$skill-name`.\n\n" ^ skills_catalog

let response_text response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } -> Some text
         | Llm.Response_tool_use _ -> None)
  |> String.concat ""

(** Truncate large tool outputs for LLM context. Full content preserved in transcript. *)
let truncate_for_llm ?(max_len=8192) output =
  if String.length output > max_len then
    String.sub output 0 max_len ^ Printf.sprintf "\n... (output truncated, %d bytes total - see transcript for full output)" (String.length output)
  else output

(** Truncate large content blocks in messages for LLM context. *)
let truncate_message_content = function
  | Llm.Text_content s -> Llm.Text_content (truncate_for_llm s)
  | Llm.Blocks blocks ->
      Llm.Blocks (List.map (function
        | Llm.Text { text } -> Llm.Text { text = truncate_for_llm text }
        | Llm.Image _ as img -> img
        | Llm.Tool_use tu -> Llm.Tool_use tu  (* tool input isn't typically huge *)
        | Llm.Tool_result tr -> Llm.Tool_result { tr with content = truncate_for_llm tr.content }
      ) blocks)

let assistant_blocks_of_response response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } when String.trim text <> "" ->
             Some (Llm.Text { text })
         | Llm.Response_text _ ->
             None
         | Llm.Response_tool_use { id; name; input } ->
             Some (Llm.Tool_use { id; name; input }))

let make_text_response state ~parent_id text =
  let final_text = if String.trim text = "" then "(empty_reply)" else text in
  let assistant_message_content = Llm.Text_content final_text in
  let model = state.Runtime.provider_config.Llm_provider.model_name in
  ignore (Transcript.add_llm_response state.Runtime.transcript ~parent_id ~model ~content:assistant_message_content ());
  Ok final_text

let activate_skill_response state ~chat_id ~parent_id name =
  let result = Tools.activate_skill state.Runtime.tools ~chat_id name in
  make_text_response state ~parent_id result.Tools.content

let maybe_handle_approval_command state ~chat_id ?parent_id prompt =
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
    let response =
      if scope = "" || target = "" then
        Error usage
      else
        match scope with
        | "exec" -> Tools.approve_executable state.Runtime.tools target
        | "read" -> Tools.approve_root state.Runtime.tools ~scope:Tools.Read target
        | "write" -> Tools.approve_root state.Runtime.tools ~scope:Tools.Write target
        | "install" -> Tools.approve_install state.Runtime.tools target
        | _ -> Error usage
    in
    let prompt_node_id =
      match parent_id with
      | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
      | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
    in
    let text =
      match response with
      | Ok message -> message
      | Error message -> message
    in
    Some (make_text_response state ~parent_id:prompt_node_id text)

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

let execute_tool_parallel state ~chat_id (id, name, input) =
  let result : Tools.tool_result =
    try
      Tools.execute state.Runtime.tools ~chat_id name input
    with exn ->
      Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
      let msg = "Tool execution exception: " ^ Printexc.to_string exn in
      Tools.failure ~error_type:"exception" ~error_category:Tools.Other msg
  in
  (id, result)

let tool_results_of_response state ~chat_id response =
  let tool_calls = 
    response.Llm.content
    |> List.filter_map (function
           | Llm.Response_tool_use { id; name; input } -> Some (id, name, input)
           | Llm.Response_text _ -> None)
  in
  
  let tool_results : (string * Tools.tool_result) list =
    if List.length tool_calls <= 1 then
      List.map (fun (id, name, input) ->
        let result : Tools.tool_result =
          try
            Tools.execute state.Runtime.tools ~chat_id name input
          with exn ->
            Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
            let msg = "Tool execution exception: " ^ Printexc.to_string exn in
            Tools.failure ~error_type:"exception" ~error_category:Tools.Other msg
        in
        (id, result)
      ) tool_calls
    else
      (* Use the shared pool from the tools registry *)
      let pool = Tools.pool state.Runtime.tools in
      try
        let futures =
          List.map
            (fun tool_call ->
              Domainslib.Task.async pool (fun _ ->
                  execute_tool_parallel state ~chat_id tool_call))
            tool_calls
        in
        List.map (Domainslib.Task.await pool) futures
      with exn ->
        Log.err (fun m -> m "Parallel tool execution infrastructure failed: %s" (Printexc.to_string exn));
        List.map (fun (id, name, input) ->
          let result : Tools.tool_result =
            try
              Tools.execute state.Runtime.tools ~chat_id name input
            with exn ->
              Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
              let msg = "Tool execution exception: " ^ Printexc.to_string exn in
              Tools.failure ~error_type:"exception" ~error_category:Tools.Other msg
          in
          (id, result)
        ) tool_calls
  in
  tool_results

let tool_result_block tool_use_id tool_result =
  let enhanced_content =
    if tool_result.Tools.is_error then begin
      let base = tool_result.Tools.content in
      match tool_result.Tools.recovery_hint with
      | Some hint -> Printf.sprintf "%s\n\nRecovery hint: %s" base hint
      | None -> base
    end else
      tool_result.Tools.content
  in
  Llm.Tool_result {
    tool_use_id;
    content = enhanced_content;
    is_error = if tool_result.Tools.is_error then Some true else None;
  }

let process ?on_text_delta ?on_status state ~chat_id ?(persistent=false) prompt =
  let prompt = String.trim prompt in
  if prompt = "" then Error "Prompt is empty"
  else
    let parent_id =
      if persistent then
        match Transcript.get_latest_node state.Runtime.transcript ~chat_id with
        | Some node_id -> Some node_id
        | None -> None
      else
        None
    in
    match maybe_handle_approval_command state ~chat_id ?parent_id prompt with
    | Some handled -> handled
    | None ->
        begin
          match maybe_handle_skill_command state ~chat_id ?parent_id prompt with
          | Some handled -> handled
          | None ->
              let prompt_node_id =
                match parent_id with
                | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
                | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
              in
              let system_prompt = build_system_prompt state ~chat_id in
              let tool_defs = Tools.definitions state.Runtime.tools in
              let rec loop current_node_id rounds_remaining =
                let messages = Transcript.get_branch state.Runtime.transcript current_node_id in
                let messages = List.map (fun (m : Llm.message) -> { m with content = truncate_message_content m.content }) messages in
                match
                  state.llm_call
                    state.provider_config
                    ?on_text_delta
                    ~system_prompt
                    messages
                    ~tools:tool_defs
                with
                | Error err -> Error err
                | Ok response ->
                    let has_tool_use =
                      List.exists
                        (function
                          | Llm.Response_tool_use _ -> true
                          | Llm.Response_text _ -> false)
                        response.Llm.content
                    in
                    if has_tool_use then
                      if rounds_remaining = 0 then Error "Tool-call recursion limit exceeded"
                      else
                        let assistant_message_content = Llm.Blocks (assistant_blocks_of_response response) in
                        let model = state.provider_config.Llm_provider.model_name in
                        let response_node_id = Transcript.add_llm_response state.Runtime.transcript ~parent_id:current_node_id ~model ~content:assistant_message_content () in
                        Option.iter (fun f -> f "Executing tools...") on_status;
                        let tool_results = tool_results_of_response state ~chat_id response in
                        let last_node_id = ref response_node_id in
                        List.iter (fun (tool_use_id, tool_result) ->
                          let block = tool_result_block tool_use_id tool_result in
                          match block with
                          | Llm.Tool_result { tool_use_id; content; is_error } ->
                              let is_err = Option.value is_error ~default:false in
                              let result_id = Transcript.add_tool_result state.Runtime.transcript ~parent_id:!last_node_id ~tool_use_id ~content ~is_error:is_err in
                              last_node_id := result_id
                          | _ -> ()
                        ) tool_results;
                        begin
                          match List.find_map (fun (_id, result) ->
                            match result.Tools.approval_request with
                            | Some request -> Some (request, result.Tools.content)
                            | None -> None
                          ) tool_results with
                          | Some (_request, message) ->
                              let response_text =
                                Printf.sprintf "%s\n\nProject root: %s" message state.Runtime.project_root
                              in
                              make_text_response state ~parent_id:!last_node_id response_text
                          | None ->
                              loop !last_node_id (rounds_remaining - 1)
                        end
                    else
                      let text = String.trim (response_text response) in
                      make_text_response state ~parent_id:current_node_id text
              in
              loop prompt_node_id state.config.max_tool_iterations
        end
