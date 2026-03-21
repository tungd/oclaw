module Tools = Agent_runtime.Tools

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let temp_dir () =
  let path = Filename.temp_file "oclaw-tools-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let temp_registry () =
  let root = temp_dir () in
  let db_path = Filename.concat root "approvals.db" in
  let project_skills_dir = Filename.concat root "project-skills" in
  let user_skills_dir = Filename.concat root "user-skills" in
  Unix.mkdir project_skills_dir 0o755;
  Unix.mkdir user_skills_dir 0o755;
  let skills =
    Agent_skills.Skills.create
      ~project_skills_dir
      ~user_skills_dir
      ~catalog_cache_path:(Filename.concat root "skills-catalog.json")
      ()
  in
  let registry =
    Tools.create_default_registry
      ~db_path
      ~project_root:root
      ~skills
      ()
  in
  (root, skills, registry)

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

let test_read_file_requires_approval () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "note.txt" in
  write_file file_path "hello" ;
  let result = Tools.execute registry ~chat_id:1 "read_file" (`Assoc [("path", `String file_path)]) in
  expect (not result.Tools.is_error) "read_file should succeed inside project_root without extra approval";
  expect (result.Tools.content = "hello") "read_file should return file contents";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ read_file_allowed_in_project_root"

let test_read_file_succeeds_after_approval () =
  let _root, skills, registry = temp_registry () in
  let approved_root = temp_dir () in
  let nested = Filename.concat approved_root "docs/note.txt" in
  write_file nested "approved read" ;
  begin
    match Tools.approve_root registry ~scope:Tools.Read approved_root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result = Tools.execute registry ~chat_id:1 "read_file" (`Assoc [("path", `String nested)]) in
  expect (not result.Tools.is_error) "read_file should succeed after approval";
  expect (result.Tools.content = "approved read") "read_file should return file contents";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ read_file_succeeds_after_approval"

let test_write_allowed_in_project_root () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "write.txt" in
  let result =
    Tools.execute registry ~chat_id:1 "write_file"
      (`Assoc [("path", `String file_path); ("content", `String "text")])
  in
  expect (not result.Tools.is_error) "write_file should succeed inside project_root without extra approval";
  expect (Sys.file_exists file_path) "write_file should create the target file";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ write_allowed_in_project_root"

let test_write_requires_separate_write_approval_outside_project_root () =
  let root, skills, registry = temp_registry () in
  begin
    match Tools.approve_root registry ~scope:Tools.Read root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let outside_root = temp_dir () in
  let file_path = Filename.concat outside_root "write.txt" in
  let result =
    Tools.execute registry ~chat_id:1 "write_file"
      (`Assoc [("path", `String file_path); ("content", `String "text")])
  in
  expect result.Tools.is_error "write_file should fail outside project_root without write approval";
  expect (result.Tools.error_category = Some Tools.ApprovalRequired) "write_file should require write approval outside project_root";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ write_requires_separate_write_approval_outside_project_root"

let test_edit_pattern_errors_are_structured () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "edit.txt" in
  write_file file_path "repeat\nrepeat\n" ;
  begin
    match Tools.approve_root registry ~scope:Tools.Write root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result =
    Tools.execute registry ~chat_id:1 "edit_file"
      (`Assoc [
        ("path", `String file_path);
        ("old_text", `String "repeat");
        ("new_text", `String "done");
      ])
  in
  expect result.Tools.is_error "edit_file should fail on ambiguous replacement";
  expect (result.Tools.error_category = Some Tools.AmbiguousMatch) "edit_file should return AmbiguousMatch";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ edit_pattern_errors_are_structured"

let test_bash_timeout_validation () =
  let _root, skills, registry = temp_registry () in
  let result =
    Tools.execute registry ~chat_id:1 "bash"
      (`Assoc [("command", `String "echo ok"); ("timeout_seconds", `Int 0)])
  in
  expect result.Tools.is_error "bash should reject invalid timeout";
  expect (result.Tools.error_category = Some Tools.InvalidParameters) "bash should validate timeout bounds";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ bash_timeout_validation"

let test_bash_requires_executable_approval () =
  let _root, skills, registry = temp_registry () in
  let result = Tools.execute registry ~chat_id:1 "bash" (`Assoc [("command", `String "echo ok")]) in
  expect result.Tools.is_error "bash should fail before executable approval";
  expect (result.Tools.error_category = Some Tools.ApprovalRequired) "bash should request executable approval";
  begin
    match result.Tools.approval_request with
    | Some request -> expect (request.scope = Tools.Execute) "approval should target executable scope"
    | None -> fail "expected executable approval request"
  end;
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ bash_requires_executable_approval"

let test_bash_succeeds_after_executable_approval () =
  let _root, skills, registry = temp_registry () in
  begin
    match Tools.approve_executable registry "echo" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result =
    Tools.execute registry ~chat_id:1 "bash"
      (`Assoc [("command", `String "echo approved"); ("timeout_seconds", `Int 2)])
  in
  expect (not result.Tools.is_error) "bash should succeed after approval";
  expect (String.contains result.Tools.content 'a') "bash output should be present";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ bash_succeeds_after_executable_approval"

let () =
  print_endline "Running tool approval and error tests...";
  test_read_file_requires_approval ();
  test_read_file_succeeds_after_approval ();
  test_write_allowed_in_project_root ();
  test_write_requires_separate_write_approval_outside_project_root ();
  test_edit_pattern_errors_are_structured ();
  test_bash_timeout_validation ();
  test_bash_requires_executable_approval ();
  test_bash_succeeds_after_executable_approval ();
  print_endline "[PASS] tool approval and error tests"
