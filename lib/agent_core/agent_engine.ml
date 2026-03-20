module Llm = Llm_types
module Log = (val Logs.src_log (Logs.Src.create "agent_engine") : Logs.LOG)

let default_system_prompt =
  String.concat "\n"
    [
      "You are OClaw, a CLI-first agentic assistant. You solve tasks by planning briefly, using tools when needed, verifying results, and reporting concrete outcomes.";
      "";
      "# Operational Guidance";
      "";
      "- Use tools when they reduce uncertainty or perform the requested action.";
      "- Do not claim success until the tool result confirms it.";
      "- For multi-step work, keep the todo list in sync with real progress.";
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
        default_system_prompt ^ "\n\n# Skills\n\nThe following skills are available. Use `activate_skill` before following a skill-specific workflow.\n\n" ^ skills_catalog

let response_text response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } -> Some text
         | Llm.Response_tool_use _ -> None)
  |> String.concat ""

let assistant_blocks_of_response response =
  response.Llm.content
  |> List.filter_map (function
         | Llm.Response_text { text } when String.trim text <> "" ->
             Some (Llm.Text { text })
         | Llm.Response_text _ ->
             None
         | Llm.Response_tool_use { id; name; input } ->
             Some (Llm.Tool_use { id; name; input }))

let execute_tool_parallel state ~chat_id (id, name, input) =
  let result : Agent_tools.Tools.tool_result =
    try
      Agent_tools.Tools.execute state.Runtime.tools ~chat_id name input
    with exn ->
      Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
      let msg = "Tool execution exception: " ^ Printexc.to_string exn in
      Agent_tools.Tools.failure ~error_type:"exception" ~error_category:Agent_tools.Tools.Other msg
  in
  (id, result)

let tool_results_of_response state ~chat_id response =
  let tool_calls = 
    response.Llm.content
    |> List.filter_map (function
           | Llm.Response_tool_use { id; name; input } -> Some (id, name, input)
           | Llm.Response_text _ -> None)
  in
  
  let tool_results : (string * Agent_tools.Tools.tool_result) list =
    if List.length tool_calls <= 1 then
      List.map (fun (id, name, input) ->
        let result : Agent_tools.Tools.tool_result =
          try
            Agent_tools.Tools.execute state.Runtime.tools ~chat_id name input
          with exn ->
            Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
            let msg = "Tool execution exception: " ^ Printexc.to_string exn in
            Agent_tools.Tools.failure ~error_type:"exception" ~error_category:Agent_tools.Tools.Other msg
        in
        (id, result)
      ) tool_calls
    else
      (* Use the shared pool from the tools registry *)
      let pool = Agent_tools.Tools.pool state.Runtime.tools in
      try
        let results =
          Domainslib.Task.run pool (fun _ ->
              let futures =
                List.map
                  (fun tool_call ->
                    Domainslib.Task.async pool (fun _ ->
                        execute_tool_parallel state ~chat_id tool_call))
                  tool_calls
              in
              List.map (Domainslib.Task.await pool) futures)
        in
        results
      with exn ->
        Log.err (fun m -> m "Parallel tool execution infrastructure failed: %s" (Printexc.to_string exn));
        List.map (fun (id, name, input) ->
          let result : Agent_tools.Tools.tool_result =
            try
              Agent_tools.Tools.execute state.Runtime.tools ~chat_id name input
            with exn ->
              Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
              let msg = "Tool execution exception: " ^ Printexc.to_string exn in
              Agent_tools.Tools.failure ~error_type:"exception" ~error_category:Agent_tools.Tools.Other msg
          in
          (id, result)
        ) tool_calls
  in
  List.map (fun (id, tool_result) ->
    (* Enhance error messages with recovery hints for the LLM *)
    let enhanced_content =
      if tool_result.Agent_tools.Tools.is_error then begin
        let base = tool_result.Agent_tools.Tools.content in
        match tool_result.Agent_tools.Tools.recovery_hint with
        | Some hint -> Printf.sprintf "%s\n\n💡 Recovery hint: %s" base hint
        | None -> base
      end else
        tool_result.Agent_tools.Tools.content
    in
    Llm.Tool_result {
      tool_use_id = id;
      content = enhanced_content;
      is_error = if tool_result.Agent_tools.Tools.is_error then Some true else None;
    }
  ) tool_results

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
    (* Add user prompt to transcript *)
    let prompt_node_id =
      match parent_id with
      | Some pid -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~parent_id:pid ~content:prompt ()
      | None -> Transcript.add_user_prompt state.Runtime.transcript ~chat_id ~content:prompt ()
    in
    let system_prompt = build_system_prompt state ~chat_id in
    let tool_defs = Agent_tools.Tools.definitions state.Runtime.tools in
    let rec loop current_node_id rounds_remaining =
      let messages = Transcript.get_branch state.Runtime.transcript current_node_id in
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
              (* Get model from provider config *)
              let model = state.provider_config.Llm_provider.model.name in
              let response_node_id = Transcript.add_llm_response state.Runtime.transcript ~parent_id:current_node_id ~model ~content:assistant_message_content () in
              Option.iter (fun f -> f "Executing tools...") on_status;
              let tool_results = tool_results_of_response state ~chat_id response in
              (* Store tool results as children of the assistant message *)
              let last_node_id = ref response_node_id in
              List.iter (fun tool_result ->
                match tool_result with
                | Llm.Tool_result { tool_use_id; content; is_error } ->
                    let is_err = Option.value is_error ~default:false in
                    let result_id = Transcript.add_tool_result state.Runtime.transcript ~parent_id:!last_node_id ~tool_use_id ~content ~is_error:is_err in
                    last_node_id := result_id
                | _ -> () (* Other content blocks should not appear in tool results *)
              ) tool_results;
              loop !last_node_id (rounds_remaining - 1)
          else
            let text = String.trim (response_text response) in
            let final_text = if text = "" then "(empty_reply)" else text in
            let assistant_message_content = Llm.Text_content final_text in
            let model = state.provider_config.Llm_provider.model.name in
            ignore (Transcript.add_llm_response state.Runtime.transcript ~parent_id:current_node_id ~model ~content:assistant_message_content ());
            Ok final_text
    in
    loop prompt_node_id state.config.max_tool_iterations
