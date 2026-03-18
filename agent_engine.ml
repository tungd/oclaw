open Yojson.Safe

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
    ]

let build_system_prompt state ~chat_id:_ =
  match state.Runtime.system_prompt_override with
  | Some prompt -> prompt
  | None ->
      let skills_catalog = Skills.build_skills_catalog state.Runtime.skills in
      if String.trim skills_catalog = "" then
        default_system_prompt
      else
        default_system_prompt ^ "\n\n# Skills\n\nThe following skills are available. Use `activate_skill` before following a skill-specific workflow.\n\n" ^ skills_catalog

let load_messages state ~chat_id =
  match Db.get_all_messages state.Runtime.db ~chat_id with
  | Ok messages -> Ok messages
  | Error err -> Error err

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

let get_num_worker_domains () =
  try
    let n = Domain.recommended_domain_count () in
    if n > 1 then min (n - 1) 4 else 1
  with _ -> 2

let execute_tool_parallel state ~chat_id (id, name, input) =
  let result =
    try
      Tools.execute state.Runtime.tools ~chat_id name input
    with exn ->
      Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
      Tools.failure ~error_type:"exception" ("Tool execution exception: " ^ Printexc.to_string exn)
  in
  (id, result)

let tool_results_of_response state ~chat_id response =
  let tool_calls = 
    response.Llm.content
    |> List.filter_map (function
           | Llm.Response_tool_use { id; name; input } -> Some (id, name, input)
           | Llm.Response_text _ -> None)
  in
  
  let tool_results =
    if List.length tool_calls <= 1 then
      List.map (fun (id, name, input) ->
        let result =
          try
            Tools.execute state.Runtime.tools ~chat_id name input
          with exn ->
            Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
            Tools.failure ~error_type:"exception" ("Tool execution exception: " ^ Printexc.to_string exn)
        in
        (id, result)
      ) tool_calls
    else
      let num_workers = get_num_worker_domains () in
      let pool = Domainslib.Task.setup_pool ~num_domains:num_workers () in
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
        Domainslib.Task.teardown_pool pool;
        results
      with exn ->
        Log.err (fun m -> m "Parallel tool execution infrastructure failed: %s" (Printexc.to_string exn));
        Domainslib.Task.teardown_pool pool;
        List.map (fun (id, name, input) ->
          let result =
            try
              Tools.execute state.Runtime.tools ~chat_id name input
            with exn ->
              Log.err (fun m -> m "Tool %s execution failed: %s" name (Printexc.to_string exn));
              Tools.failure ~error_type:"exception" ("Tool execution exception: " ^ Printexc.to_string exn)
          in
          (id, result)
        ) tool_calls
  in
  List.map (fun (id, result) ->
    Llm.Tool_result {
      tool_use_id = id;
      content = result.Tools.content;
      is_error = if result.Tools.is_error then Some true else None;
    }
  ) tool_results

let process ?on_text_delta ?on_status state ~chat_id ?(persistent=false) prompt =
  let prompt = String.trim prompt in
  if prompt = "" then Error "Prompt is empty"
  else
    let base_messages =
      if persistent then
        match load_messages state ~chat_id with
        | Error err -> 
            Log.warn (fun m -> m "Failed to load chat history: %s" err);
            []
        | Ok messages -> messages
      else
        []
    in
    let user_message = { Llm.role = "user"; content = Llm.Text_content prompt } in
    match Db.store_message state.Runtime.db ~chat_id ~message:user_message with
    | Error err -> Error err
    | Ok () ->
        let system_prompt = build_system_prompt state ~chat_id in
        let tool_defs = Tools.definitions state.Runtime.tools in
        let rec loop messages rounds_remaining =
          match
            state.Runtime.llm_call
              state.Runtime.provider_config
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
                  let assistant_message =
                    {
                      Llm.role = "assistant";
                      content = Llm.Blocks (assistant_blocks_of_response response);
                    }
                  in
                  match Db.store_message state.Runtime.db ~chat_id ~message:assistant_message with
                  | Error err -> Error err
                  | Ok () ->
                      Option.iter (fun f -> f "Executing tools...") on_status;
                      let tool_results = tool_results_of_response state ~chat_id response in
                      let tool_results_message = { Llm.role = "user"; content = Llm.Blocks tool_results } in
                      match Db.store_message state.Runtime.db ~chat_id ~message:tool_results_message with
                      | Error err -> Error err
                      | Ok () ->
                          loop (messages @ [ assistant_message; tool_results_message ]) (rounds_remaining - 1)
              else
                let text = String.trim (response_text response) in
                let final_text = if text = "" then "(empty_reply)" else text in
                let assistant_message = { Llm.role = "assistant"; content = Llm.Text_content final_text } in
                match Db.store_message state.Runtime.db ~chat_id ~message:assistant_message with
                | Error err -> Error err
                | Ok () -> Ok final_text
        in
        loop (base_messages @ [ user_message ]) state.Runtime.config.max_tool_iterations
