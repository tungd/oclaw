let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

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
    Agent_runtime.Config.default_config with
    llm_api_key = "test-key";
    llm_api_base = "http://example.invalid";
    llm_model = "test-model";
    data_dir;
    max_tool_iterations = 8;
  }

let create_state ?llm_call data_dir =
  match Agent_runtime.App.create ?llm_call (make_config data_dir) with
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
    match Agent_runtime.Session.process state1 ~chat_id:7 "first prompt" with
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
    match Agent_runtime.Session.process ~persistent:true state2 ~chat_id:7 "second prompt" with
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
  ignore (Agent_runtime.Session.process state1 ~chat_id:1 "chat one prompt");
  let second_llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (not (List.mem "chat one prompt" contents)) "chat history leaked across chat ids";
    text_response "chat two answer"
  in
  let state2 = create_state ~llm_call:second_llm data_dir in
  ignore (Agent_runtime.Session.process state2 ~chat_id:2 "chat two prompt")

let test_tool_loop_and_resume () =
  let data_dir = temp_dir () in
  write_file (Filename.concat data_dir "note.txt") "tool output";
  let calls = ref 0 in
  let llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    if !calls = 1 then
      tool_response "read_file" [ ("path", `String (Filename.concat data_dir "note.txt")) ]
    else (
      let contents = text_messages messages in
      expect (List.mem "tool output" contents) "tool result missing from follow-up call";
      text_response "done"
    )
  in
  let state = create_state ~llm_call:llm data_dir in
  begin
    match Agent_runtime.Session.approve_read state data_dir with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Agent_runtime.Session.process state ~chat_id:3 "read the note" with
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
    match Agent_runtime.Session.process ~persistent:true state2 ~chat_id:3 "follow up" with
    | Ok "second turn" -> ()
    | Ok other -> fail ("unexpected resumed reply: " ^ other)
    | Error err -> fail err
  end

let test_project_root_db_and_approval_command () =
  let root = temp_dir () in
  let normalized_root = Unix.realpath root in
  Unix.mkdir (Filename.concat root ".agents") 0o755;
  let nested = Filename.concat root "src/deep" in
  Unix.mkdir (Filename.concat root "src") 0o755;
  Unix.mkdir nested 0o755;
  let state = create_state nested in
  expect (Agent_runtime.App.project_root state = normalized_root) "runtime should reuse ancestor .agents directory";
  expect (Agent_runtime.App.db_path state = Filename.concat normalized_root ".agents/oclaw.db") "runtime should place db in .agents/oclaw.db";
  begin
    match Agent_runtime.Session.process ~persistent:true state ~chat_id:9 ("/approve read " ^ nested) with
    | Ok response ->
        expect (String.length response > 0) "approval command should return a response"
    | Error err -> fail err
  end;
  let target = Filename.concat nested "approved.txt" in
  write_file target "approved contents";
  let calls = ref 0 in
  let llm _provider ?on_text_delta:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    let contents = text_messages messages in
    if !calls = 1 then (
      expect (List.mem "read approved file" contents) "prompt should be present before tool call";
      tool_response "read_file" [ ("path", `String target) ]
    ) else (
      expect (List.mem "approved contents" contents) "approved tool result should be present";
      text_response "done"
    )
  in
  let state3 = create_state ~llm_call:llm nested in
  begin
    match Agent_runtime.Session.process state3 ~chat_id:9 "read approved file" with
    | Ok reply -> expect (reply = "done") "tool call should succeed after persisted approval"
    | Error err -> fail err
  end

let () =
  test_persistent_session ();
  test_session_isolation ();
  test_tool_loop_and_resume ();
  test_project_root_db_and_approval_command ();
  Printf.printf "[PASS] assistant runtime tests\n"
