open Yojson.Safe

module Llm = Llm_types

let read_optional_file path =
  try Some (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with _ -> None

let load_optional_fragment path heading =
  match read_optional_file path with
  | Some content when String.trim content <> "" ->
      Some (heading ^ "\n" ^ content)
  | _ ->
      None

let default_identity caller =
  Printf.sprintf
    "You are OClaw, a CLI-first agentic assistant. You solve tasks by planning briefly, using tools when needed, verifying results, and reporting concrete outcomes.\n\nConnected via: %s."
    caller

let builtin_operational_guidance =
  String.concat "\n"
    [
      "# Operational Guidance";
      "";
      "- Use tools when they reduce uncertainty or perform the requested action.";
      "- Do not claim success until the tool result confirms it.";
      "- For multi-step work, keep the todo list in sync with real progress.";
      "- Prefer editing targeted files over rewriting unrelated code.";
      "- Use memory for durable user or workspace facts, not transient scratch notes.";
      "- Keep answers concise and directly tied to the request.";
    ]

let build_system_prompt state ~chat_id =
  match state.Runtime.system_prompt_override with
  | Some prompt -> prompt
  | None ->
      let data_dir = state.Runtime.config.data_dir in
      let soul_path = Filename.concat data_dir "SOUL.md" in
      let identity_path = Filename.concat data_dir "IDENTITY.md" in
      let user_path = Filename.concat data_dir "USER.md" in
      let identity =
        match read_optional_file soul_path with
        | Some soul when String.trim soul <> "" ->
            "<soul>\n" ^ soul ^ "\n</soul>\n\n" ^ default_identity "cli"
        | _ -> default_identity "cli"
      in
      let sections =
        [
          Some identity;
          load_optional_fragment identity_path "# Identity Context";
          load_optional_fragment user_path "# User Context";
          Some builtin_operational_guidance;
          (let memory_context = Memory.build_memory_context state.Runtime.memory chat_id in
           if String.trim memory_context = "" then None
           else Some ("# Memories\n\n" ^ memory_context));
          (let skills_catalog = Skills.build_skills_catalog state.Runtime.skills in
           if String.trim skills_catalog = "" then None
           else
             Some
               ("# Agent Skills\n\nThe following skills are available. Use `activate_skill` before following a skill-specific workflow.\n\n"
                ^ skills_catalog));
        ]
      in
      sections |> List.filter_map Fun.id |> String.concat "\n\n"

let parse_session json =
  try
    let payload = Yojson.Safe.from_string json in
    Llm.messages_of_yojson payload
  with exn ->
    Error (Printexc.to_string exn)

let save_session state ~chat_id messages =
  let json = Llm.messages_to_yojson messages |> Yojson.Safe.to_string in
  Db.save_session state.Runtime.db ~chat_id ~messages_json:json

let load_history_from_db state ~chat_id =
  match Db.get_recent_messages state.Runtime.db ~chat_id ~limit:state.Runtime.config.max_history_messages with
  | Error err -> Error err
  | Ok history ->
      Ok
        (history
         |> List.map (fun message ->
                {
                  Llm.role = message.Db.role;
                  content = Llm.Text_content message.content;
                }))

let load_messages state ~chat_id =
  match Db.load_session state.Runtime.db ~chat_id with
  | Error err -> Error err
  | Ok (Some (json, _updated_at)) ->
      begin
        match parse_session json with
        | Ok messages -> Ok messages
        | Error _ -> load_history_from_db state ~chat_id
      end
  | Ok None ->
      load_history_from_db state ~chat_id

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
  let result = Tools.execute state.Runtime.tools ~chat_id name input in
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
      (* Sequential execution for single tool call *)
      List.map (fun (id, name, input) ->
        let result = Tools.execute state.Runtime.tools ~chat_id name input in
        (id, result)
      ) tool_calls
    else
      (* Parallel execution using Domainslib for multiple tool calls *)
      let pool = Domainslib.Task.setup_pool ~num_domains:(min 4 (Sys.cpu_count ())) () in
      try
        let futures = List.map (fun tool_call ->
          Domainslib.Task.run pool (fun _ ->
            execute_tool_parallel state ~chat_id tool_call
          )
        ) tool_calls in
        
        let results = List.map (Domainslib.Task.await pool) futures in
        Domainslib.Task.teardown_pool pool;
        results
      with exn ->
        Domainslib.Task.teardown_pool pool;
        Log.err (fun m -> m "Parallel tool execution failed: %s" (Printexc.to_string exn));
        (* Fallback to sequential execution on error *)
        List.map (fun (id, name, input) ->
          let result = Tools.execute state.Runtime.tools ~chat_id name input in
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

let process state ~chat_id prompt =
  let prompt = String.trim prompt in
  if prompt = "" then Error "Prompt is empty"
  else
    match load_messages state ~chat_id with
    | Error err -> Error err
    | Ok base_messages ->
        let user_message = { Llm.role = "user"; content = Llm.Text_content prompt } in
        begin
          match Db.store_message state.Runtime.db ~chat_id ~role:"user" ~content:prompt with
          | Error err -> Error err
          | Ok () ->
              let system_prompt = build_system_prompt state ~chat_id in
              let tool_defs = Tools.definitions state.Runtime.tools in
              let rec loop messages rounds_remaining =
                match
                  state.Runtime.llm_call
                    state.Runtime.provider_config
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
                      if rounds_remaining = 0 then
                        Error "Tool-call recursion limit exceeded"
                      else
                        let assistant_message =
                          {
                            Llm.role = "assistant";
                            content = Llm.Blocks (assistant_blocks_of_response response);
                          }
                        in
                        let tool_results = tool_results_of_response state ~chat_id response in
                        let next_messages =
                          messages
                          @ [ assistant_message; { Llm.role = "user"; content = Llm.Blocks tool_results } ]
                        in
                        begin
                          match save_session state ~chat_id next_messages with
                          | Error err -> Error err
                          | Ok () -> loop next_messages (rounds_remaining - 1)
                        end
                    else
                      let text = String.trim (response_text response) in
                      let final_text =
                        if text = "" then "(empty_reply)" else text
                      in
                      let next_messages =
                        messages @ [ { Llm.role = "assistant"; content = Llm.Text_content final_text } ]
                      in
                      match save_session state ~chat_id next_messages with
                      | Error err -> Error err
                      | Ok () ->
                          begin
                            match Db.store_message state.Runtime.db ~chat_id ~role:"assistant" ~content:final_text with
                            | Error err -> Error err
                            | Ok () -> Ok final_text
                          end
              in
              loop (base_messages @ [ user_message ]) state.Runtime.config.max_tool_iterations
        end
