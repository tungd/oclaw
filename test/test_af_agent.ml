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
  let path = Filename.temp_file "af-agent-test-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then ()
  else (
    mkdir_p (Filename.dirname dir);
    Unix.mkdir dir 0o755
  )

let write_file path content =
  mkdir_p (Filename.dirname path);
  Stdlib.Out_channel.with_open_bin path (fun channel -> output_string channel content)

let file_url path =
  "file://" ^ path

let digest content =
  Digest.string content |> Digest.to_hex

let make_config data_dir =
  {
    Agent_runtime.Config.default_config with
    llm_api_key = "test-key";
    llm_api_base = "http://example.invalid";
    llm_model = "test-model";
    data_dir;
    max_tool_iterations = 6;
  }

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

let tool_result_texts messages =
  messages
  |> List.filter_map (fun (message : Llm_types.message) ->
         match message.content with
         | Llm_types.Text_content _ -> None
         | Llm_types.Blocks blocks ->
             let results =
               blocks
               |> List.filter_map (function
                      | Llm_types.Tool_result { content; _ } -> Some content
                      | _ -> None)
             in
             begin
               match results with
               | [] -> None
               | values -> Some (String.concat "\n" values)
             end)

let final_message events =
  match
    List.find_map
      (function
        | Acp.Message.Agent_message { content; _ } -> Some content
        | _ -> None)
      events
  with
  | Some content -> content
  | None -> fail "expected final message"

let capture_process app prompt =
  let events = ref [] in
  let emit event = events := !events @ [ event ] in
  let result = Agent_runtime.Session.process ~emit app ~chat_id:1 ~persistent:false prompt in
  (result, !events)

let with_app ?llm_call path f =
  match Af_agent.load ~path with
  | Error err -> fail err
  | Ok agent ->
      begin
        match Af_agent.create_app ?llm_call (make_config (Filename.dirname path)) agent with
        | Error err -> fail err
        | Ok app ->
            Fun.protect
              ~finally:(fun () -> Agent_runtime.App.close app)
              (fun () -> f app)
      end

let test_parse_tools_and_prompt () =
  let content =
    String.trim {|
---
name: planner
description: Example planner
uses:
  worker: ./worker.md
---
# System

Plan carefully.

# Tools

## slugify
Create a slug from the input.

```python
import sys
print(sys.stdin.read().strip().replace(" ", "-"))
```

# Notes

Keep it compact.
|}
  in
  match Af_agent.parse_string ~locator:(Af_agent.Local "/tmp/planner.md") content with
  | Error err -> fail err
  | Ok document ->
      expect (document.name = "planner") "parser should load the agent name";
      expect (document.uses = [ ("worker", "./worker.md") ]) "parser should load uses";
      expect (List.length document.tools = 1) "parser should discover embedded tools";
      expect (contains document.prompt "Plan carefully.") "prompt should keep non-tool sections";
      expect (contains document.prompt "Keep it compact.") "prompt should keep later sections";
      expect (not (contains document.prompt "slugify")) "prompt should exclude the tools section";
      print_endline "  ✓ parse_tools_and_prompt"

let test_duplicate_tool_name_rejected () =
  let content =
    String.trim {|
---
name: planner
---
# Tools

## repeat
First

```bash
cat
```

## repeat
Second

```bash
cat
```
|}
  in
  match Af_agent.parse_string ~locator:(Af_agent.Local "/tmp/planner.md") content with
  | Ok _ -> fail "duplicate tool names should be rejected"
  | Error err ->
      expect (contains err "Duplicate tool name") "duplicate tool error should be reported";
      print_endline "  ✓ duplicate_tool_name_rejected"

let test_remote_use_requires_pin () =
  let root = temp_dir () in
  let child_path = Filename.concat root "remote-child.md" in
  let root_path = Filename.concat root "planner.md" in
  write_file child_path "---\nname: child\n---\n# System\nchild\n";
  write_file root_path
    (Printf.sprintf "---\nname: root\nuses:\n  child: %s\n---\n# System\nroot\n" (file_url child_path));
  match Af_agent.load ~path:root_path with
  | Ok _ -> fail "remote use without lockfile should be rejected"
  | Error err ->
      expect (contains err "not pinned") "missing pin should be reported";
      print_endline "  ✓ remote_use_requires_pin"

let test_remote_recursive_uses_and_digest_validation () =
  let root = temp_dir () in
  let remote_parent = Filename.concat root "remote-parent.md" in
  let remote_child = Filename.concat root "remote-child.md" in
  let local_root = Filename.concat root "root.md" in
  let remote_parent_body =
    Printf.sprintf "---\nname: remote-parent\nuses:\n  child: ./remote-child.md\n---\n# System\nparent\n"
  in
  let remote_child_body = "---\nname: remote-child\n---\n# System\nchild\n" in
  write_file remote_parent remote_parent_body;
  write_file remote_child remote_child_body;
  write_file (Af_agent.lock_path remote_parent)
    (Yojson.Safe.pretty_to_string
       (`Assoc
          [
            ("remote_uses",
             `List
               [
                 `Assoc
                   [
                     ("alias", `String "child");
                     ("source", `String "./remote-child.md");
                     ("resolved", `String (file_url remote_child));
                     ("digest", `String (digest remote_child_body));
                   ];
               ]);
          ]));
  let remote_parent_url = file_url remote_parent in
  let root_body =
    Printf.sprintf "---\nname: root\nuses:\n  parent: %s\n---\n# System\nroot\n" remote_parent_url
  in
  write_file local_root root_body;
  write_file (Af_agent.lock_path local_root)
    (Yojson.Safe.pretty_to_string
       (`Assoc
          [
            ("remote_uses",
             `List
               [
                 `Assoc
                   [
                     ("alias", `String "parent");
                     ("source", `String remote_parent_url);
                     ("resolved", `String remote_parent_url);
                     ("digest", `String (digest remote_parent_body));
                   ];
               ]);
          ]));
  match Af_agent.load ~path:local_root with
  | Error err -> fail err
  | Ok resolved ->
      expect (List.length resolved.children = 1) "root should load its remote child";
      let parent = snd (List.hd resolved.children) in
      expect (List.length parent.children = 1) "remote child should load its own pinned child";
      write_file (Af_agent.lock_path local_root)
        (Yojson.Safe.pretty_to_string
           (`Assoc
              [
                ("remote_uses",
                 `List
                   [
                     `Assoc
                       [
                         ("alias", `String "parent");
                         ("source", `String remote_parent_url);
                         ("resolved", `String remote_parent_url);
                         ("digest", `String "wrong");
                       ];
                   ]);
              ]));
      begin
        match Af_agent.load ~path:local_root with
        | Ok _ -> fail "digest mismatch should be rejected"
        | Error err ->
            expect (contains err "Digest mismatch") "digest mismatch should be reported"
      end;
      print_endline "  ✓ remote_recursive_uses_and_digest_validation"

let test_embedded_tool_execution () =
  let root = temp_dir () in
  let path = Filename.concat root "tool-agent.md" in
  write_file path
    (String.trim {|
---
name: tool-parent
---
# System

Use the embedded tool.

# Tools

## echo-input
Echo stdin back to the caller.

```bash
cat
```
|});
  let llm _provider ?emit:_ ~system_prompt messages ~tools:_ =
    expect (contains system_prompt "Name: tool-parent") "system prompt should include agent metadata";
    match tool_result_texts messages with
    | [] -> tool_response "echo-input" [ ("input", `String "hello from tool") ]
    | tool_results -> text_response (List.hd tool_results)
  in
  with_app ~llm_call:llm path (fun app ->
      begin
        match Agent_runtime.Session.approve_exec app "/bin/sh" with
        | Ok _ -> ()
        | Error err -> fail err
      end;
      let result, events = capture_process app "run the tool" in
      begin
        match result with
        | Ok () -> ()
        | Error err -> fail err
      end;
      expect (final_message events = "hello from tool") "embedded tool output should flow back into the agent";
      print_endline "  ✓ embedded_tool_execution")

let test_delegate_child_agent () =
  let root = temp_dir () in
  let parent_path = Filename.concat root "parent.md" in
  let child_path = Filename.concat root "child.md" in
  write_file child_path
    (String.trim {|
---
name: child-agent
---
# System

You are the child.
|});
  write_file parent_path
    (String.trim {|
---
name: parent-agent
uses:
  worker: ./child.md
---
# System

Delegate to the worker when appropriate.
|});
  let llm _provider ?emit:_ ~system_prompt messages ~tools =
    if contains system_prompt "Name: parent-agent" then
      match tool_result_texts messages with
      | [] ->
          expect (List.exists (fun (tool : Llm_types.tool_definition) -> tool.name = "delegate") tools)
            "parent should expose the delegate tool";
          tool_response "delegate" [ ("alias", `String "worker"); ("task", `String "research the issue") ]
      | results ->
          text_response ("parent saw: " ^ List.hd results)
    else if contains system_prompt "Name: child-agent" then
      text_response "child complete"
    else
      fail "unexpected system prompt"
  in
  with_app ~llm_call:llm parent_path (fun app ->
      let result, events = capture_process app "delegate this" in
      begin
        match result with
        | Ok () -> ()
        | Error err -> fail err
      end;
      expect (final_message events = "parent saw: child complete") "delegate tool should run the child agent";
      print_endline "  ✓ delegate_child_agent")

let () =
  test_parse_tools_and_prompt ();
  test_duplicate_tool_name_rejected ();
  test_remote_use_requires_pin ();
  test_remote_recursive_uses_and_digest_validation ();
  test_embedded_tool_execution ();
  test_delegate_child_agent ();
  print_endline "[PASS] af agent tests"
