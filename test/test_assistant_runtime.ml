let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let contains haystack needle =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if idx + needle_len > hay_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  needle_len = 0 || loop 0

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

let capture_process ?persistent state ~chat_id prompt =
  let events = ref [] in
  let emit event =
    events := !events @ [event]
  in
  let result = Agent_runtime.Session.process ?persistent ~emit state ~chat_id prompt in
  (result, !events)

let expect_final_message expected events =
  match
    List.find_map
      (function
        | Acp.Message.Agent_message { content; _ } -> Some content
        | _ -> None)
      events
  with
  | Some content -> expect (content = expected) ("unexpected final message: " ^ content)
  | None -> fail "expected final agent message"

let final_message events =
  match
    List.find_map
      (function
        | Acp.Message.Agent_message { content; _ } -> Some content
        | _ -> None)
      events
  with
  | Some content -> content
  | None -> fail "expected final agent message"

let tool_call_uses_path tool_call expected_path =
  match tool_call.Acp.Message.raw_input with
  | Some (`Assoc fields) ->
      begin
        match List.assoc_opt "path" fields, List.assoc_opt "command" fields with
        | Some (`String path), _ -> path = expected_path
        | _, Some (`String command) -> command = expected_path
        | _ -> false
      end
  | _ -> false

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

let count_assistant_tool_use_messages messages =
  messages
  |> List.fold_left
       (fun count (msg : Llm_types.message) ->
         match msg.role, msg.content with
         | "assistant", Llm_types.Blocks blocks
           when List.exists
                  (function
                    | Llm_types.Tool_use _ -> true
                    | _ -> false)
                  blocks ->
             count + 1
         | _ -> count)
       0

let test_persistent_session () =
  let data_dir = temp_dir () in
  let first_llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "first prompt" contents) "current prompt missing from first turn";
    text_response "first answer"
  in
  let state1 = create_state ~llm_call:first_llm data_dir in
  let result1, events1 = capture_process state1 ~chat_id:7 "first prompt" in
  begin
    match result1 with
    | Ok () -> expect_final_message "first answer" events1
    | Error err -> fail err
  end;
  let second_llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "first prompt" contents) "prior user message missing after restart";
    expect (List.mem "first answer" contents) "prior assistant message missing after restart";
    expect (List.mem "second prompt" contents) "new prompt missing";
    text_response "second answer"
  in
  let state2 = create_state ~llm_call:second_llm data_dir in
  let result2, events2 = capture_process ~persistent:true state2 ~chat_id:7 "second prompt" in
  begin
    match result2 with
    | Ok () -> expect_final_message "second answer" events2
    | Error err -> fail err
  end

let test_session_isolation () =
  let data_dir = temp_dir () in
  let first_llm _provider ?emit:_ ~system_prompt:_ _messages ~tools:_ =
    text_response "chat one answer"
  in
  let state1 = create_state ~llm_call:first_llm data_dir in
  begin
    match capture_process state1 ~chat_id:1 "chat one prompt" with
    | Ok (), _ -> ()
    | Error err, _ -> fail err
  end;
  let second_llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (not (List.mem "chat one prompt" contents)) "chat history leaked across chat ids";
    text_response "chat two answer"
  in
  let state2 = create_state ~llm_call:second_llm data_dir in
  begin
    match capture_process state2 ~chat_id:2 "chat two prompt" with
    | Ok (), _ -> ()
    | Error err, _ -> fail err
  end

let test_tool_loop_and_resume () =
  let data_dir = temp_dir () in
  write_file (Filename.concat data_dir "note.txt") "tool output";
  let calls = ref 0 in
  let llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    if !calls = 1 then
      tool_response "read_file" [ ("path", `String (Filename.concat data_dir "note.txt")) ]
    else (
      let contents = text_messages messages in
      expect
        (count_assistant_tool_use_messages messages = 1)
        "tool turn should replay exactly one assistant tool_use message";
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
  let result1, events1 = capture_process state ~chat_id:3 "read the note" in
  begin
    match result1 with
    | Ok () ->
        expect_final_message "done" events1;
        expect
          (List.exists
             (function
               | Acp.Message.Session_update { update = Acp.Message.Tool_call tool_call; _ } ->
                   tool_call_uses_path tool_call (Filename.concat data_dir "note.txt")
               | _ -> false)
             events1)
          "expected read_file tool call event";
        expect
          (List.exists
             (function
               | Acp.Message.Session_update { update = Acp.Message.Tool_call_update { status = Some Acp.Message.Completed; raw_input; _ }; _ } ->
                   raw_input = Some (`Assoc [ ("path", `String (Filename.concat data_dir "note.txt")) ])
               | _ -> false)
             events1)
          "expected successful read_file tool result event"
    | Error err -> fail err
  end;
  let resumed _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    let contents = text_messages messages in
    expect (List.mem "read the note" contents) "tool turn prompt missing after resume";
    expect (List.mem "done" contents) "final assistant reply missing after resume";
    text_response "second turn"
  in
  let state2 = create_state ~llm_call:resumed data_dir in
  let result2, events2 = capture_process ~persistent:true state2 ~chat_id:3 "follow up" in
  begin
    match result2 with
    | Ok () -> expect_final_message "second turn" events2
    | Error err -> fail err
  end

let test_permission_request_and_resume () =
  let data_dir = temp_dir () in
  let calls = ref 0 in
  let llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    if !calls = 1 then
      tool_response "bash" [ ("command", `String "echo approved") ]
    else (
      let contents = text_messages messages in
      expect (List.exists (fun text -> String.contains text 'a') contents) "approved tool result should be present after resume";
      text_response "done"
    )
  in
  let state = create_state ~llm_call:llm data_dir in
  let events = ref [] in
  let emit event = events := !events @ [ event ] in
  begin
    match Agent_runtime.Session.process ~emit state ~chat_id:15 "run echo" with
    | Error err -> fail err
    | Ok () ->
        expect
          (List.exists
             (function
               | Acp.Message.Request_permission { tool_call = { tool_call_id = "call-1"; _ }; options; _ } ->
                   List.length options = 2
               | _ -> false)
             !events)
          "expected request_permission event"
  end;
  begin
    match Agent_runtime.Session.resolve_permission state ~chat_id:15 (Acp.Message.Selected "allow-once") with
    | Error err -> fail err
    | Ok () ->
        expect_final_message "done" !events;
        expect
          (List.exists
             (function
               | Acp.Message.Session_update { update = Acp.Message.Tool_call_update { status = Some Acp.Message.Completed; _ }; _ } -> true
               | _ -> false)
             !events)
          "expected completed tool call after approval"
  end

let test_permission_request_and_reject () =
  let data_dir = temp_dir () in
  let calls = ref 0 in
  let llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
    incr calls;
    if !calls = 1 then
      tool_response "bash" [ ("command", `String "echo rejected") ]
    else (
      let contents = text_messages messages in
      expect
        (List.exists (fun text -> String.starts_with ~prefix:"Permission rejected" text) contents)
        "rejection should be returned as tool failure";
      text_response "rejected done"
    )
  in
  let state = create_state ~llm_call:llm data_dir in
  let events = ref [] in
  let emit event = events := !events @ [ event ] in
  begin
    match Agent_runtime.Session.process ~emit state ~chat_id:16 "run reject" with
    | Error err -> fail err
    | Ok () -> ()
  end;
  begin
    match Agent_runtime.Session.resolve_permission state ~chat_id:16 (Acp.Message.Selected "reject-once") with
    | Error err -> fail err
    | Ok () ->
        expect_final_message "rejected done" !events;
        expect
          (List.exists
             (function
               | Acp.Message.Session_update { update = Acp.Message.Tool_call_update { status = Some Acp.Message.Failed; _ }; _ } -> true
               | _ -> false)
             !events)
          "expected failed tool call after rejection"
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
  let approval_result, approval_events =
    capture_process ~persistent:true state ~chat_id:9 ("/approve read " ^ nested)
  in
  begin
    match approval_result with
    | Ok () ->
        begin
          match
            List.find_map
              (function
                | Acp.Message.Agent_message { content; _ } -> Some content
                | _ -> None)
              approval_events
          with
          | Some response -> expect (String.length response > 0) "approval command should return a response"
          | None -> fail "approval command should emit a response"
        end
    | Error err -> fail err
  end;
  let target = Filename.concat nested "approved.txt" in
  write_file target "approved contents";
  let calls = ref 0 in
  let llm _provider ?emit:_ ~system_prompt:_ messages ~tools:_ =
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
  let result3, events3 = capture_process state3 ~chat_id:9 "read approved file" in
  begin
    match result3 with
    | Ok () -> expect_final_message "done" events3
    | Error err -> fail err
  end

let test_permissions_command_lists_approvals () =
  let data_dir = temp_dir () in
  let llm_called = ref false in
  let llm _provider ?emit:_ ~system_prompt:_ _messages ~tools:_ =
    llm_called := true;
    text_response "unexpected"
  in
  let state = create_state ~llm_call:llm data_dir in
  let read_root = temp_dir () in
  let write_root = temp_dir () in
  begin
    match Agent_runtime.Session.approve_read state read_root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Agent_runtime.Session.approve_write state write_root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Agent_runtime.Session.approve_exec state "echo" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Agent_runtime.Session.approve_install state "demo-skill" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result, events = capture_process ~persistent:true state ~chat_id:17 "/permissions" in
  begin
    match result with
    | Error err -> fail err
    | Ok () ->
        expect (not !llm_called) "/permissions should not enter the LLM loop";
        let message = final_message events in
        let project_root = Agent_runtime.App.project_root state in
        expect (contains message "Approved tools (executables):") "permissions output should include executables";
        expect (contains message "Approved read roots:") "permissions output should include read roots";
        expect (contains message "Approved write roots:") "permissions output should include write roots";
        expect (contains message "Approved skill installs:") "permissions output should include installs";
        expect
          (contains message (project_root ^ " (project root, implicit)"))
          "permissions output should include implicit project root access";
        let history = Agent_runtime.Session.history state ~chat_id:17 in
        expect
          (List.exists
             (fun (msg : Llm_types.message) ->
               msg.role = "user" && msg.content = Llm_types.Text_content "/permissions")
             history)
          "permissions command should be stored in transcript";
        expect
          (List.exists
             (fun (msg : Llm_types.message) ->
               msg.role = "assistant" && msg.content = Llm_types.Text_content message)
             history)
          "permissions response should be stored in transcript"
  end

let test_permissions_command_read_filter () =
  let data_dir = temp_dir () in
  let llm_called = ref false in
  let llm _provider ?emit:_ ~system_prompt:_ _messages ~tools:_ =
    llm_called := true;
    text_response "unexpected"
  in
  let state = create_state ~llm_call:llm data_dir in
  let read_root = temp_dir () in
  begin
    match Agent_runtime.Session.approve_read state read_root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Agent_runtime.Session.approve_exec state "echo" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result, events = capture_process state ~chat_id:18 "/permissions read" in
  begin
    match result with
    | Error err -> fail err
    | Ok () ->
        expect (not !llm_called) "/permissions read should not enter the LLM loop";
        let message = final_message events in
        expect (contains message "Approved read roots:") "read filter should return the read section";
        expect (not (contains message "Approved tools (executables):")) "read filter should omit executables";
        expect (not (contains message "Approved write roots:")) "read filter should omit write roots";
        expect (not (contains message "Approved skill installs:")) "read filter should omit installs"
  end

let test_permissions_command_invalid_usage () =
  let data_dir = temp_dir () in
  let llm_called = ref false in
  let llm _provider ?emit:_ ~system_prompt:_ _messages ~tools:_ =
    llm_called := true;
    text_response "unexpected"
  in
  let state = create_state ~llm_call:llm data_dir in
  let result, events = capture_process state ~chat_id:19 "/permissions exec extra" in
  begin
    match result with
    | Error err -> fail err
    | Ok () ->
        expect (not !llm_called) "invalid /permissions usage should not enter the LLM loop";
        expect_final_message "Usage: /permissions [exec|read|write|install]" events
  end

let test_event_sequence_for_text_reply () =
  let data_dir = temp_dir () in
  let streamed = ref false in
  let llm _provider ?emit ~system_prompt:_ _messages ~tools:_ =
    begin
      match emit with
      | Some send ->
          send (Acp.Message.Agent_delta { content = "hello " });
          send (Acp.Message.Agent_delta { content = "world" });
          streamed := true
      | None -> ()
    end;
    text_response "hello world"
  in
  let state = create_state ~llm_call:llm data_dir in
  let result, events = capture_process state ~chat_id:11 "say hello" in
  begin
    match result with
    | Error err -> fail err
    | Ok () ->
        expect !streamed "llm emit hook should stream deltas";
        begin
          match events with
          | Acp.Message.Status { status; _ }
            :: Acp.Message.Agent_delta { content = "hello " }
            :: Acp.Message.Agent_delta { content = "world" }
            :: Acp.Message.Agent_message { content = "hello world"; _ }
            :: [Acp.Message.Done] ->
              expect (status = "thinking") "expected thinking status first"
          | _ -> fail "unexpected event sequence for text reply"
        end
  end

let () =
  test_persistent_session ();
  test_session_isolation ();
  test_tool_loop_and_resume ();
  test_permission_request_and_resume ();
  test_permission_request_and_reject ();
  test_project_root_db_and_approval_command ();
  test_permissions_command_lists_approvals ();
  test_permissions_command_read_filter ();
  test_permissions_command_invalid_usage ();
  test_event_sequence_for_text_reply ();
  Printf.printf "[PASS] assistant runtime tests\n"
