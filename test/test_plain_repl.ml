let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let close_in_noerr ic =
  try close_in ic with _ -> ()

let close_out_noerr oc =
  try close_out oc with _ -> ()

let slurp path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let contains haystack needle =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if idx + needle_len > hay_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  needle_len = 0 || loop 0

let find_index haystack needle =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if idx + needle_len > hay_len then None
    else if String.sub haystack idx needle_len = needle then Some idx
    else loop (idx + 1)
  in
  if needle_len = 0 then Some 0 else loop 0

let count_occurrences haystack needle =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx count =
    if needle_len = 0 || idx + needle_len > hay_len then count
    else if String.sub haystack idx needle_len = needle then
      loop (idx + needle_len) (count + 1)
    else
      loop (idx + 1) count
  in
  loop 0 0

let run_loop_capture ~input_text ~persistent deps =
  let in_path = Filename.temp_file "plain_repl_in" ".txt" in
  let out_path = Filename.temp_file "plain_repl_out" ".txt" in
  let input_writer = open_out_bin in_path in
  output_string input_writer input_text;
  close_out input_writer;
  let ic = open_in_bin in_path in
  let oc = open_out_bin out_path in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr ic;
      close_out_noerr oc;
      Sys.remove in_path;
      Sys.remove out_path)
    (fun () ->
      Agent_tui.Plain_repl.run_loop ~input:ic ~output:oc ~persistent deps;
      close_out oc;
      slurp out_path)

let make_deps
    ?(history=(fun () -> []))
    ?(process=(fun ~emit:_ _ -> Ok ()))
    ?(create_conversation=(fun () -> Error "create_conversation not configured"))
    ?(fork_conversation=(fun () -> Error "fork_conversation not configured"))
    ?(resolve_permission=(fun _ -> Ok ()))
    ?(chat_label=(fun () -> "chat:1 ephemeral"))
    ()
  =
  {
    Agent_tui.Plain_repl.process;
    create_conversation;
    fork_conversation;
    resolve_permission;
    history;
    project_root = "/tmp/project";
    model_name = "test-model";
    chat_label;
  }

let allow_once =
  Acp.Message.{ option_id = "allow-once"; name = "Allow once"; kind = Allow_once }

let reject_once =
  Acp.Message.{ option_id = "reject-once"; name = "Reject"; kind = Reject_once }

let sample_tool_call =
  Acp.Message.{
    tool_call_id = "call-1";
    title = Some "List files";
    kind = Some Execute;
    status = Some Pending;
    content = None;
    raw_input = Some (`Assoc [ ("command", `String "ls") ]);
    raw_output = None;
  }

let sample_completed_tool_call =
  Acp.Message.{
    sample_tool_call with
    status = Some Completed;
    content = Some [ Content (Text "done") ];
    raw_output =
      Some
        (`Assoc
           [
             ("content", `String "done");
             ("isError", `Bool false);
             ("durationMs", `Int 42);
           ]);
  }

let test_render_history_message () =
  let user_lines =
    Agent_tui.Plain_repl.render_history_message
      Llm_types.{ role = "user"; content = Text_content "hello" }
  in
  expect (user_lines = [ "> hello" ]) "user history should render with a prompt prefix";

  let assistant_lines =
    Agent_tui.Plain_repl.render_history_message
      Llm_types.{
        role = "assistant";
        content =
          Blocks [
            Text { text = "visible" };
            Tool_use { id = "tool-1"; name = "bash"; input = `Assoc [ ("command", `String "pwd") ] };
          ];
      }
  in
  expect
    (assistant_lines = [ "visible"; "tool> Exec: pwd" ])
    "assistant history should render tool_use blocks inline";

  let tool_result_lines =
    Agent_tui.Plain_repl.render_history_message
      Llm_types.{
        role = "user";
        content =
          Blocks [
            Tool_result { tool_use_id = "tool-1"; content = "ok"; is_error = Some false };
          ];
      }
  in
  expect
    (tool_result_lines = [ "tool> Tool done: tool-1" ])
    "user tool_result history should render a concise status line"

let test_select_permission_option () =
  let selected =
    Agent_tui.Plain_repl.select_permission_option [ allow_once; reject_once ] "1"
  in
  expect
    (selected = Ok (Acp.Message.Selected "allow-once"))
    "choice 1 should map to the first option";
  let invalid =
    Agent_tui.Plain_repl.select_permission_option [ allow_once; reject_once ] "9"
  in
  expect
    (invalid = Error "Enter a number between 1 and 2.")
    "out-of-range approval choices should be rejected"

let test_classify_session_commands () =
  expect
    (Agent_tui.Plain_repl.classify_input "/new" = `New)
    "/new should classify as a local session command";
  expect
    (Agent_tui.Plain_repl.classify_input "/fork" = `Fork)
    "/fork should classify as a local session command"

let test_markdown_rendering () =
  let plain =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:false
      "# Heading\nUse `code` and **bold**"
  in
  expect
    (plain = [ "# Heading"; "Use code and bold" ])
    "plain markdown rendering should strip inline markdown markers";
  let colored =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:true
      "# Heading"
    |> String.concat "\n"
  in
  expect
    (contains colored "\027[")
    "ANSI markdown rendering should emit escape sequences when enabled"

let test_table_rendering () =
  let rendered =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:false
      "| Name | Value |\n| --- | ---: |\n| foo | 10 |\n| longer | 2 |"
  in
  expect
    (rendered =
       [
         "┌────────┬───────┐";
         "│ Name   │ Value │";
         "├────────┼───────┤";
         "│ foo    │    10 │";
         "│ longer │     2 │";
         "└────────┴───────┘";
       ])
    "pipe tables should render as aligned bordered tables"

let test_table_alignment_markers () =
  let rendered =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:false
      "| Left | Right | Center |\n| :--- | ---: | :---: |\n| a | 1 | x |\n| bb | 22 | yy |"
  in
  expect
    (List.mem "│ a    │     1 │   x    │" rendered)
    "table alignment markers should control cell padding"

let test_table_inline_markdown () =
  let rendered =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:false
      "| Name | Value |\n| --- | --- |\n| **foo** | `bar` |"
    |> String.concat "\n"
  in
  expect (contains rendered "foo") "table cells should keep visible bold text";
  expect (contains rendered "bar") "table cells should keep visible code text";
  expect (not (contains rendered "**foo**")) "table cells should not leak bold markers";
  expect (not (contains rendered "`bar`")) "table cells should not leak code markers"

let test_code_fence_ignores_table_detection () =
  let rendered =
    Agent_tui.Plain_repl.render_markdown_lines
      ~supports_ansi:false
      "```\n| Name | Value |\n| --- | --- |\n```"
  in
  expect
    (rendered = [ "```"; "| Name | Value |"; "| --- | --- |"; "```" ])
    "pipe rows inside code fences should stay literal"

let test_exit_commands_skip_process () =
  let process_calls = ref 0 in
  let deps =
    make_deps
      ~process:(fun ~emit:_ _prompt ->
        incr process_calls;
        Ok ())
      ()
  in
  ignore (run_loop_capture ~input_text:"/exit\n" ~persistent:false deps);
  expect (!process_calls = 0) "/exit should terminate without calling process";
  ignore (run_loop_capture ~input_text:"/quit\n" ~persistent:false deps);
  expect (!process_calls = 0) "/quit should terminate without calling process"

let test_session_commands_require_persistent_mode () =
  let create_calls = ref 0 in
  let fork_calls = ref 0 in
  let deps =
    make_deps
      ~create_conversation:(fun () ->
        incr create_calls;
        Ok 2)
      ~fork_conversation:(fun () ->
        incr fork_calls;
        Ok 3)
      ()
  in
  let output = run_loop_capture ~input_text:"/new\n/fork\n/exit\n" ~persistent:false deps in
  expect (!create_calls = 0) "/new should not run outside persistent mode";
  expect (!fork_calls = 0) "/fork should not run outside persistent mode";
  expect (contains output "/new requires --persistent.") "/new should explain the persistent-mode requirement";
  expect (contains output "/fork requires --persistent.") "/fork should explain the persistent-mode requirement"

let test_new_command_switches_active_session () =
  let active_chat = ref 1 in
  let prompts = ref [] in
  let deps =
    make_deps
      ~chat_label:(fun () -> Printf.sprintf "chat:%d persistent" !active_chat)
      ~create_conversation:(fun () ->
        active_chat := 2;
        Ok 2)
      ~process:(fun ~emit prompt ->
        prompts := !prompts @ [ (!active_chat, prompt) ];
        emit (Acp.Message.Agent_message { content = "ok"; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"/new\nhello\n/exit\n" ~persistent:true deps in
  expect (!prompts = [ (2, "hello") ]) "/new should switch the active chat before the next prompt";
  expect (contains output "Created new conversation and switched to chat:2.") "/new should print a local success message";
  expect (contains output "chat:2 persistent") "/new should re-render the banner with the new session label"

let test_fork_command_switches_active_session () =
  let active_chat = ref 4 in
  let prompts = ref [] in
  let deps =
    make_deps
      ~chat_label:(fun () -> Printf.sprintf "chat:%d persistent" !active_chat)
      ~fork_conversation:(fun () ->
        active_chat := 9;
        Ok 9)
      ~process:(fun ~emit prompt ->
        prompts := !prompts @ [ (!active_chat, prompt) ];
        emit (Acp.Message.Agent_message { content = "ok"; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"/fork\nafter fork\n/exit\n" ~persistent:true deps in
  expect (!prompts = [ (9, "after fork") ]) "/fork should switch the active chat before the next prompt";
  expect (contains output "Forked current conversation and switched to chat:9.") "/fork should print a local success message";
  expect (contains output "chat:9 persistent") "/fork should re-render the banner with the new session label"

let test_normal_prompt_calls_process () =
  let prompts = ref [] in
  let deps =
    make_deps
      ~process:(fun ~emit prompt ->
        prompts := prompt :: !prompts;
        emit (Acp.Message.Agent_message { content = "ok"; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"hello\n/exit\n" ~persistent:false deps in
  expect (!prompts = [ "hello" ]) "normal prompts should call process exactly once";
  expect (contains output "> hello") "non-tty loop should echo submitted prompts";
  expect (contains output "ok") "assistant responses should be printed"

let test_persistent_startup_replays_history () =
  let deps =
    make_deps
      ~history:(fun () ->
        [
          Llm_types.{ role = "user"; content = Text_content "old prompt" };
          Llm_types.{
            role = "assistant";
            content =
              Blocks [
                Text { text = "old reply" };
                Tool_use { id = "call-1"; name = "bash"; input = `Assoc [ ("command", `String "pwd") ] };
              ];
          };
          Llm_types.{
            role = "user";
            content = Blocks [ Tool_result { tool_use_id = "call-1"; content = "ignored"; is_error = Some false } ];
          };
        ])
      ()
  in
  let output = run_loop_capture ~input_text:"/exit\n" ~persistent:true deps in
  expect (contains output "> old prompt") "persistent startup should replay prior user messages";
  expect (contains output "old reply") "persistent startup should replay visible assistant text";
  expect (contains output "tool> Exec: pwd") "persistent replay should show tool_use lines";
  expect (contains output "tool> Exec done: pwd") "persistent replay should show tool_result lines";
  expect
    (not (contains output "tool> Exec: pwd\n\ntool> Exec done: pwd"))
    "persistent replay should keep a tool start and its done line grouped";
  expect (not (contains output "ignored")) "successful tool results should stay concise in replay";
  let old_idx = match find_index output "> old prompt" with Some idx -> idx | None -> fail "missing replayed prompt" in
  let exit_idx = match find_index output "> /exit" with Some idx -> idx | None -> fail "missing exit prompt echo" in
  expect (old_idx < exit_idx) "history replay should appear before the first interactive prompt"

let test_persistent_startup_hides_permissions_replay () =
  let deps =
    make_deps
      ~history:(fun () ->
        [
          Llm_types.{ role = "user"; content = Text_content "/permissions" };
          Llm_types.{
            role = "assistant";
            content = Text_content "Approved read roots:\n- /tmp/project (project root, implicit)";
          };
          Llm_types.{ role = "user"; content = Text_content "follow-up" };
          Llm_types.{ role = "assistant"; content = Text_content "kept" };
        ])
      ()
  in
  let output = run_loop_capture ~input_text:"/exit\n" ~persistent:true deps in
  expect (not (contains output "> /permissions")) "permissions slash command should be hidden on replay";
  expect (not (contains output "Approved read roots:")) "permissions slash command response should be hidden on replay";
  expect (contains output "> follow-up") "normal replayed user prompts should still be shown";
  expect (contains output "kept") "normal replayed assistant responses should still be shown"

let test_live_tool_call_logging () =
  let deps =
    make_deps
      ~process:(fun ~emit _prompt ->
        emit
          (Acp.Message.Session_update {
             session_id = "1";
             update = Acp.Message.Tool_call sample_tool_call;
           });
        emit
          (Acp.Message.Session_update {
             session_id = "1";
             update = Acp.Message.Tool_call_update sample_completed_tool_call;
           });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"run\n/exit\n" ~persistent:false deps in
  expect (contains output "tool> Exec: ls") "live tool_call events should be logged";
  expect
    (contains output "tool> Exec done: ls (42ms)")
    "completed tool updates should include timing";
  expect
    (not (contains output "tool> Exec: ls\n\ntool> Exec done: ls"))
    "sequential live tool updates should stay grouped"

let test_invalid_approval_reprompts () =
  let resolved = ref [] in
  let deps =
    make_deps
      ~process:(fun ~emit _prompt ->
        emit
          (Acp.Message.Request_permission {
             session_id = "1";
             tool_call = sample_tool_call;
             options = [ allow_once; reject_once ];
           });
        Ok ())
      ~resolve_permission:(fun outcome ->
        resolved := outcome :: !resolved;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"run\n9\n1\n/exit\n" ~persistent:false deps in
  expect (contains output "Approval required") "approval requests should be printed";
  expect
    (contains output "Invalid choice. Enter a number between 1 and 2.")
    "invalid approval input should reprompt";
  expect
    (!resolved = [ Acp.Message.Selected "allow-once" ])
    "approval loop should keep the pending request until a valid selection is made"

let test_streaming_does_not_duplicate_final_message () =
  let streamed = "streamed-response" in
  let deps =
    make_deps
      ~process:(fun ~emit _prompt ->
        emit (Acp.Message.Agent_delta { content = "streamed-" });
        emit (Acp.Message.Agent_delta { content = "response" });
        emit (Acp.Message.Agent_message { content = streamed; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"hello\n/exit\n" ~persistent:false deps in
  expect
    (count_occurrences output streamed = 1)
    "streamed assistant output should not be duplicated when the final Agent_message arrives"

let test_streaming_markdown_reassembles_split_inline_markup () =
  let deps =
    make_deps
      ~process:(fun ~emit _prompt ->
        emit (Acp.Message.Agent_delta { content = "**bo" });
        emit (Acp.Message.Agent_delta { content = "ld**" });
        emit (Acp.Message.Agent_message { content = "**bold**"; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"hello\n/exit\n" ~persistent:false deps in
  expect
    (contains output "bold")
    "split inline markdown should render its visible text once the turn completes";
  expect
    (not (contains output "**bold**"))
    "split inline markdown should not leak raw markdown markers into the transcript"

let test_streaming_table_renders_once_when_complete () =
  let table =
    "| Name | Value |\n| --- | ---: |\n| foo | 10 |\n| longer | 2 |"
  in
  let deps =
    make_deps
      ~process:(fun ~emit _prompt ->
        emit (Acp.Message.Agent_delta { content = "| Name | Value |\n" });
        emit (Acp.Message.Agent_delta { content = "| --- | ---: |\n| foo | 10 |\n" });
        emit (Acp.Message.Agent_delta { content = "| longer | 2 |" });
        emit (Acp.Message.Agent_message { content = table; chat_id = None });
        emit Acp.Message.Done;
        Ok ())
      ()
  in
  let output = run_loop_capture ~input_text:"hello\n/exit\n" ~persistent:false deps in
  expect (contains output "┌────────┬───────┐") "streamed tables should render as a bordered table";
  expect
    (not (contains output "| Name | Value |"))
    "streamed tables should not leak the raw header row";
  expect
    (not (contains output "| --- | ---: |"))
    "streamed tables should not leak the raw separator row"

let () =
  test_render_history_message ();
  test_select_permission_option ();
  test_classify_session_commands ();
  test_markdown_rendering ();
  test_table_rendering ();
  test_table_alignment_markers ();
  test_table_inline_markdown ();
  test_code_fence_ignores_table_detection ();
  test_exit_commands_skip_process ();
  test_session_commands_require_persistent_mode ();
  test_new_command_switches_active_session ();
  test_fork_command_switches_active_session ();
  test_normal_prompt_calls_process ();
  test_persistent_startup_replays_history ();
  test_persistent_startup_hides_permissions_replay ();
  test_live_tool_call_logging ();
  test_invalid_approval_reprompts ();
  test_streaming_does_not_duplicate_final_message ();
  test_streaming_markdown_reassembles_split_inline_markup ();
  test_streaming_table_renders_once_when_complete ();
  Printf.printf "[PASS] plain repl tests\n"
