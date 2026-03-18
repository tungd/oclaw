let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let make_provider () =
  {
    Llm_provider.api_base = "http://example.invalid";
    api_key = "test-key";
    model = {
      Llm_provider.id = "test-model";
      name = "test-model";
      reasoning = false;
      input_types = [ "text" ];
      cost = (0.0, 0.0, 0.0, 0.0);
      context_window = 16384;
      max_tokens = 256;
    };
    temperature = 0.0;
    max_tokens = 256;
    timeout = 1;
  }

let temp_dir () =
  let path = Filename.temp_file "oclaw-runtime-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let write_file path content =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun channel -> output_string channel content)

let contains_substring haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop index =
    if n_len = 0 then true
    else if index > h_len - n_len then false
    else if String.sub haystack index n_len = needle then true
    else loop (index + 1)
  in
  loop 0

let text_response content =
  Ok {
    Llm_types.content = [ Llm_types.Response_text { text = content } ];
    stop_reason = Some "end_turn";
    usage = None;
  }

let tool_response name args =
  Ok {
    Llm_types.content = [
      Llm_types.Response_tool_use {
        id = "call-1";
        name;
        input = `Assoc args;
      }
    ];
    stop_reason = Some "tool_use";
    usage = None;
  }

let make_config data_dir =
  {
    Oclaw_config.Config.default_config with
    llm_api_key = "test-key";
    llm_api_base = "http://example.invalid";
    llm_model = "test-model";
    data_dir;
    tools_workspace = data_dir;
    tools_restrict_to_workspace = true;
    max_history_messages = 24;
    max_tool_iterations = 8;
  }

let create_state ?llm_call data_dir =
  match Runtime.create_app_state ?llm_call (make_config data_dir) with
  | Ok state -> state
  | Error err -> fail err

let text_messages messages =
  messages
  |> List.filter_map (fun (msg : Llm_types.message) ->
         match msg.content with
         | Llm_types.Text_content text -> Some text
         | Llm_types.Blocks blocks ->
             blocks
             |> List.filter_map (function
                    | Llm_types.Text { text } -> Some text
                    | Llm_types.Tool_result { content; _ } -> Some content
                    | _ -> None)
             |> function
             | [] -> None
             | lines -> Some (String.concat "\n" lines))

let test_persistent_session () =
  let data_dir = temp_dir () in
  let first_llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "first prompt" contents) "current prompt missing from first turn";
    text_response "first answer"
  in
  let state1 = create_state ~llm_call:first_llm data_dir in
  begin
    match Agent_engine.process state1 ~chat_id:7 "first prompt" with
    | Ok "first answer" -> ()
    | Ok other -> fail ("unexpected first reply: " ^ other)
    | Error err -> fail err
  end;
  let second_llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "first prompt" contents) "prior user message missing after restart";
    expect (List.mem "first answer" contents) "prior assistant message missing after restart";
    expect (List.mem "second prompt" contents) "new prompt missing";
    text_response "second answer"
  in
  let state2 = create_state ~llm_call:second_llm data_dir in
  begin
    match Agent_engine.process state2 ~chat_id:7 "second prompt" with
    | Ok "second answer" -> ()
    | Ok other -> fail ("unexpected second reply: " ^ other)
    | Error err -> fail err
  end

let test_session_isolation () =
  let data_dir = temp_dir () in
  let first_llm _provider ?on_text_delta:_ ~system_prompt:_ _messages ~tools:_ =
    text_response "chat one answer"
  in
  let state1 = create_state ~llm_call:first_llm data_dir in
  ignore (Agent_engine.process state1 ~chat_id:1 "chat one prompt");
  let second_llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (not (List.mem "chat one prompt" contents)) "chat history leaked across chat ids";
    text_response "chat two answer"
  in
  let state2 = create_state ~llm_call:second_llm data_dir in
  ignore (Agent_engine.process state2 ~chat_id:2 "chat two prompt")

let test_memory_injection () =
  let data_dir = temp_dir () in
  write_file (Filename.concat data_dir "AGENTS.md") "global fact";
  write_file
    (Filename.concat (Filename.concat (Filename.concat data_dir "runtime") "groups/42") "AGENTS.md")
    "chat fact";
  let llm _provider ?on_text_delta:_ ~system_prompt _messages ~tools:_ =
    expect (contains_substring system_prompt "global fact") "global memory missing from system prompt";
    expect (contains_substring system_prompt "chat fact") "chat memory missing from system prompt";
    text_response "done"
  in
  let state = create_state ~llm_call:llm data_dir in
  ignore (Agent_engine.process state ~chat_id:42 "hello")

let test_tool_loop_and_resume () =
  let data_dir = temp_dir () in
  write_file (Filename.concat data_dir "note.txt") "tool output";
  let calls = ref 0 in
  let llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    if !calls = 1 then
      tool_response "read_file" [ ("path", `String "note.txt") ]
    else (
      let contents = text_messages messages in
      expect (List.mem "tool output" contents) "tool result missing from follow-up call";
      text_response "done"
    )
  in
  let state = create_state ~llm_call:llm data_dir in
  begin
    match Agent_engine.process state ~chat_id:3 "read the note" with
    | Ok "done" -> ()
    | Ok other -> fail ("unexpected tool loop reply: " ^ other)
    | Error err -> fail err
  end;
  let resumed _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "read the note" contents) "tool turn prompt missing after resume";
    expect (List.mem "done" contents) "final assistant reply missing after resume";
    text_response "second turn"
  in
  let state2 = create_state ~llm_call:resumed data_dir in
  begin
    match Agent_engine.process state2 ~chat_id:3 "follow up" with
    | Ok "second turn" -> ()
    | Ok other -> fail ("unexpected resumed reply: " ^ other)
    | Error err -> fail err
  end

let test_skill_activation () =
  let data_dir = temp_dir () in
  write_file
    (Filename.concat (Filename.concat data_dir "skills/pdf") "SKILL.md")
    "# PDF\n\nConvert things to PDF.";
  let state = create_state data_dir in
  let activate =
    Tools.execute state.tools ~chat_id:1 "activate_skill" (`Assoc [ ("name", `String "pdf") ])
  in
  expect (not activate.Tools.is_error) "activate_skill should succeed";
  expect (contains_substring activate.Tools.content "Convert things to PDF") "activate_skill returned wrong content"

let test_schedule_task_tool () =
  let data_dir = temp_dir () in
  let state = create_state data_dir in
  let result =
    Tools.execute state.tools ~chat_id:11 "schedule_task"
      (`Assoc
         [
           ("prompt", `String "Say hello");
           ("run_at", `String "2099-01-01T09:30:00");
         ])
  in
  expect (not result.Tools.is_error) "schedule_task should succeed for future one-shot tasks";
  let listed = Tools.execute state.tools ~chat_id:11 "list_scheduled_tasks" (`Assoc []) in
  expect (contains_substring listed.Tools.content "Say hello") "scheduled task should be listed"

let test_web_fetch_validation () =
  let data_dir = temp_dir () in
  let state = create_state data_dir in
  let result =
    Tools.execute state.tools ~chat_id:1 "web_fetch" (`Assoc [ ("url", `String "file:///etc/passwd") ])
  in
  expect result.Tools.is_error "web_fetch should reject unsupported schemes"

let () =
  test_persistent_session ();
  test_session_isolation ();
  test_memory_injection ();
  test_tool_loop_and_resume ();
  test_skill_activation ();
  test_schedule_task_tool ();
  test_web_fetch_validation ();
  Printf.printf "[PASS] assistant runtime tests\n"
