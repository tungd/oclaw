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

let assistant_reply content =
  Llm_provider.Success {
    id = "resp";
    object_ = "chat.completion";
    created = 0;
    model = "test-model";
    choices = [
      {
        Llm_provider.index = 0;
        message = {
          role = Llm_provider.Assistant;
          content;
          tool_call_id = None;
          tool_calls = [];
        };
        finish_reason = "stop";
        tool_calls = [];
      }
    ];
    usage_prompt_tokens = 0;
    usage_completion_tokens = 0;
    usage_total_tokens = 0;
    system_fingerprint = None;
  }

let assistant_tool_call ?(content="") name args =
  let call = {
    Llm_provider.id = "call-1";
    type_ = "function";
    function_name = name;
    function_args = Yojson.Safe.to_string (`Assoc args);
  } in
  Llm_provider.Success {
    id = "resp-tool";
    object_ = "chat.completion";
    created = 0;
    model = "test-model";
    choices = [
      {
        Llm_provider.index = 0;
        message = {
          role = Llm_provider.Assistant;
          content;
          tool_call_id = None;
          tool_calls = [ call ];
        };
        finish_reason = "tool_calls";
        tool_calls = [ call ];
      }
    ];
    usage_prompt_tokens = 0;
    usage_completion_tokens = 0;
    usage_total_tokens = 0;
    system_fingerprint = None;
  }

let test_session_memory () =
  let seen_messages = ref [] in
  let llm_call _provider messages ~tools:_ =
    seen_messages := !seen_messages @ [ messages ];
    if List.length !seen_messages = 1 then assistant_reply "first answer"
    else assistant_reply "second answer"
  in
  let runtime =
    Assistant_runtime.create
      ~provider_config:(make_provider ())
      ~llm_call
      ~system_prompt:"system"
      ()
  in
  ignore (Assistant_runtime.query runtime "first prompt");
  ignore (Assistant_runtime.query runtime "second prompt");
  match List.rev !seen_messages with
  | latest :: _ ->
      let contents = List.map (fun msg -> msg.Llm_provider.content) latest in
      expect (List.mem "first prompt" contents) "previous user message missing from follow-up context";
      expect (List.mem "first answer" contents) "previous assistant reply missing from follow-up context"
  | [] ->
      fail "expected LLM calls to be recorded"

let test_restart_loses_context () =
  let seen_first = ref [] in
  let llm1 _provider messages ~tools:_ =
    seen_first := messages;
    assistant_reply "run one"
  in
  let llm2 _provider messages ~tools:_ =
    let contents = List.map (fun msg -> msg.Llm_provider.content) messages in
    expect (not (List.mem "prompt one" contents)) "fresh runtime should not carry prior context";
    assistant_reply "run two"
  in
  let runtime1 =
    Assistant_runtime.create ~provider_config:(make_provider ()) ~llm_call:llm1 ~system_prompt:"system" ()
  in
  let runtime2 =
    Assistant_runtime.create ~provider_config:(make_provider ()) ~llm_call:llm2 ~system_prompt:"system" ()
  in
  ignore (Assistant_runtime.query runtime1 "prompt one");
  ignore (Assistant_runtime.query runtime2 "prompt two");
  expect (!seen_first <> []) "first runtime should have issued an LLM request"

let test_tool_execution () =
  let workspace = Filename.concat (Filename.get_temp_dir_name ()) "oclaw-test-tools" in
  (try Unix.mkdir workspace 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let file_path = Filename.concat workspace "note.txt" in
  let out = open_out file_path in
  output_string out "tool output";
  close_out out;
  Tools.init_default_tools ~sandbox_config:{
    Tools.workspace_root = workspace;
    restrict_to_workspace = true;
    allow_read_paths = [];
    allow_write_paths = [];
    exec_timeout_seconds = 1;
    exec_enable_deny_patterns = true;
    exec_custom_deny_patterns = [];
    exec_custom_allow_patterns = [];
  } ();
  let call_count = ref 0 in
  let llm_call _provider messages ~tools:_ =
    incr call_count;
    if !call_count = 1 then
      assistant_tool_call "read_file" [ ("path", `String "note.txt") ]
    else (
      let contents = List.map (fun msg -> msg.Llm_provider.content) messages in
      expect (List.mem "tool output" contents) "tool result should be fed back into the follow-up LLM call";
      assistant_reply "done"
    )
  in
  let runtime =
    Assistant_runtime.create ~provider_config:(make_provider ()) ~llm_call ~system_prompt:"system" ()
  in
  match Assistant_runtime.query runtime "read the note" with
  | Ok response -> expect (String.equal response "done") "unexpected final response after tool call"
  | Error err -> fail ("tool execution query failed: " ^ err)

let () =
  test_session_memory ();
  test_restart_loses_context ();
  test_tool_execution ();
  Printf.printf "[PASS] assistant runtime tests\n"
