open Yojson.Safe

module Llm = Llm_types
module Log = (val Logs.src_log (Logs.Src.create "agent_engine") : Logs.LOG)

(* Token counting helper - rough estimate: 4 chars ≈ 1 token for English *)
let estimate_tokens text =
  let len = String.length text in
  (len / 4) + 1  (* +1 to be conservative *)

let count_message_tokens (msg : Llm.message) =
  let count_block (block : Llm.content_block) =
    match block with
    | Llm.Text { text } -> estimate_tokens text
    | Llm.Image _ -> 1000  (* Rough estimate for images *)
    | Llm.Tool_use { input; _ } -> 
        estimate_tokens (Yojson.Safe.to_string input)
    | Llm.Tool_result { content; _ } -> estimate_tokens content
  in
  match msg.Llm.content with
  | Llm.Text_content text -> estimate_tokens text
  | Llm.Blocks blocks ->
      List.fold_left (fun acc block -> acc + count_block block) 0 blocks

let count_messages_tokens messages =
  List.fold_left (fun acc msg -> acc + count_message_tokens msg) 0 messages

(* ANSI color codes *)
let ansi_reset = "\027[0m"
let ansi_bold = "\027[1m"
let ansi_orange = "\027[38;5;208m"  (* Bright orange *)
let ansi_dim = "\027[2m"
let ansi_yellow = "\027[38;5;220m"  (* Warning yellow *)
let ansi_red = "\027[38;5;196m"     (* Error red *)

(* Unicode Braille patterns for spinner - clockwise rotation with 3 dots *)
(* Braille dot positions: 1=TL, 2=BL, 3=TR, 4=BR, 5=TM, 6=BM, 7=TR2, 8=BR2 *)
let braille_spinner = [|
  "⠋";  (* 3 dots: left column + top-right *)
  "⠙";  (* 3 dots: left-top + right column *)
  "⠹";  (* 3 dots: top row + right-bottom *)
  "⠸";  (* 3 dots: right column + bottom-left *)
  "⠼";  (* 3 dots: right-bottom + left column *)
  "⠴";  (* 3 dots: bottom row + left-top *)
  "⠦";  (* 3 dots: left column + bottom-right *)
  "⠇";  (* 3 dots: left-bottom + right-top *)
|]

(* Status indicators for user feedback *)
let print_status msg =
  print_string "\r\027[K";  (* Clear line and return to beginning *)
  print_string msg;
  flush stdout

let clear_status () =
  print_string "\r\027[K";  (* Clear line *)
  flush stdout

let thinking_indicator = ref 0
let spinner_running = ref false
let spinner_thread = ref None

(* Start background spinner updates *)
let start_spinner ?(show_tokens=false) ?(token_count=0) ?(context_limit=0) () =
  if not !spinner_running then (
    spinner_running := true;
    thinking_indicator := 0;
    (* Use a simple domain-based background updater *)
    let domain_id = Domain.spawn (fun () ->
      while !spinner_running do
        Unix.sleepf 0.15;  (* ~7 FPS - smooth animation *)
        if !spinner_running then (
          thinking_indicator := (!thinking_indicator + 1) mod 8;
          let braille = braille_spinner.(!thinking_indicator) in
          let status_text = 
            if show_tokens && context_limit > 0 then (
              let pct = (token_count * 100) / context_limit in
              let color = 
                if pct < 50 then ansi_dim
                else if pct < 80 then ansi_yellow
                else ansi_red
              in
              Printf.sprintf "%s%s%s %sThinking...%s %s(%dK/%dK tokens, %d%%)%s"
                ansi_orange ansi_bold braille ansi_reset ansi_dim
                color (token_count / 1000) (context_limit / 1000) pct ansi_reset
            ) else
              Printf.sprintf "%s%s%s %sThinking...%s"
                ansi_orange ansi_bold braille ansi_reset ansi_dim
          in
          print_status status_text
        )
      done
    ) in
    spinner_thread := Some domain_id
  )

(* Stop background spinner *)
let stop_spinner () =
  spinner_running := false;
  match !spinner_thread with
  | Some domain_id -> 
      (try Domain.join domain_id with _ -> ());
      spinner_thread := None
  | None -> ()

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

let get_num_worker_domains () =
  (* Get number of worker domains for parallel execution.
     Domainslib needs worker domains in addition to the main domain.
     We use Domain.recommended_domain_count() if available, otherwise default to 2.
     Cap at 4 workers to avoid oversubscription. *)
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
  
  (* Show tool call summary *)
  if tool_calls <> [] then (
    let tool_names = List.map (fun (_, name, _) -> name) tool_calls in
    let count = List.length tool_names in
    if count = 1 then
      print_status (ansi_orange ^ "⚡ " ^ ansi_reset ^ "Calling: " ^ List.hd tool_names)
    else
      let shown = List.take (min 3 count) tool_names in
      print_status (ansi_orange ^ "⚡ " ^ ansi_reset ^ Printf.sprintf "Calling %d tools: %s" count 
        (String.concat ", " shown ^ 
         (if count > 3 then ansi_dim ^ "..." ^ ansi_reset else "")))
  );
  
  let tool_results =
    if List.length tool_calls <= 1 then
      (* Sequential execution for single tool call *)
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
      (* Parallel execution using Domainslib for multiple tool calls *)
      let num_workers = get_num_worker_domains () in
      Log.debug (fun m -> m "Executing %d tool calls in parallel with %d worker domains" (List.length tool_calls) num_workers);
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
        (* Fallback to sequential execution on infrastructure error *)
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
  (* Convert to Llm.Tool_result format *)
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
        let all_messages = base_messages @ [user_message] in
        
        (* Calculate current token usage *)
        let current_tokens = count_messages_tokens all_messages in
        let context_limit = state.Runtime.config.llm_max_tokens + 10000 in  (* Rough estimate *)
        let context_window = 1000000 in  (* Model's full context window *)
        
        (* Check if we're approaching limits *)
        if current_tokens > context_window * 90 / 100 then (
          Log.warn (fun m -> m "Context usage critical: %d/%d tokens (%d%%)" 
            current_tokens context_window ((current_tokens * 100) / context_window));
        );
        
        begin
          match Db.store_message state.Runtime.db ~chat_id ~role:"user" ~content:prompt with
          | Error err -> Error err
          | Ok () ->
              let system_prompt = build_system_prompt state ~chat_id in
              let tool_defs = Tools.definitions state.Runtime.tools in
              thinking_indicator := 0;
              start_spinner ~show_tokens:true ~token_count:current_tokens ~context_limit:context_window ();
              let rec loop messages rounds_remaining =
                (* Recalculate tokens for updated message list *)
                let msg_tokens = count_messages_tokens messages in
                let total_tokens = msg_tokens + estimate_tokens system_prompt in
                
                match
                  state.Runtime.llm_call
                    state.Runtime.provider_config
                    ~system_prompt
                    messages
                    ~tools:tool_defs
                with
                | Error err -> 
                    stop_spinner ();
                    clear_status ();
                    Error err
                | Ok response ->
                    (* Update token count with response usage if available *)
                    let final_tokens = 
                      match response.Llm.usage with
                      | Some usage -> usage.Llm.input_tokens + usage.Llm.output_tokens
                      | None -> total_tokens
                    in
                    
                    let has_tool_use =
                      List.exists
                        (function
                          | Llm.Response_tool_use _ -> true
                          | Llm.Response_text _ -> false)
                        response.Llm.content
                    in
                    if has_tool_use then
                      if rounds_remaining = 0 then (
                        stop_spinner ();
                        clear_status ();
                        Error "Tool-call recursion limit exceeded"
                      ) else
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
                          | Error err -> 
                              stop_spinner ();
                              clear_status ();
                              Error err
                          | Ok () -> 
                              (* Restart spinner with updated token count *)
                              start_spinner ~show_tokens:true ~token_count:final_tokens ~context_limit:context_window ();
                              loop next_messages (rounds_remaining - 1)
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
                        | Error err -> 
                            stop_spinner ();
                            clear_status ();
                            Error err
                        | Ok () -> 
                            stop_spinner ();
                            clear_status ();
                            (* Log final token usage *)
                            Log.info (fun m -> m "Completed: %d tokens used (%.1f%% of context)" 
                              final_tokens ((float final_tokens *. 100.0) /. float context_window));
                            Ok final_text
              in
              loop (base_messages @ [ user_message ]) state.Runtime.config.max_tool_iterations
        end
