module Tools = Agent_runtime.Tools

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

let test_read_file_supports_line_ranges () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "range.txt" in
  write_file file_path "alpha\nbeta\ngamma\ndelta\n" ;
  let middle =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("line_from", `Int 2);
        ("line_to", `Int 3);
      ])
  in
  expect (not middle.Tools.is_error) "read_file should support bounded line ranges";
  expect (middle.Tools.content = "beta\ngamma\n") "read_file should return the selected bounded line range";
  let suffix =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("line_from", `Int 3);
      ])
  in
  expect (not suffix.Tools.is_error) "read_file should support open-ended line_from";
  expect (suffix.Tools.content = "gamma\ndelta\n") "read_file should return content from line_from through EOF";
  let prefix =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("line_to", `Int 2);
      ])
  in
  expect (not prefix.Tools.is_error) "read_file should support open-ended line_to";
  expect (prefix.Tools.content = "alpha\nbeta\n") "read_file should return content from BOF through line_to";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ read_file_supports_line_ranges"

let test_read_file_validates_line_ranges () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "range.txt" in
  write_file file_path "alpha\nbeta\ngamma\n" ;
  let non_positive =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("line_from", `Int 0);
      ])
  in
  expect non_positive.Tools.is_error "read_file should reject non-positive line numbers";
  expect (non_positive.Tools.error_category = Some Tools.InvalidParameters) "read_file should classify invalid line numbers";
  expect (contains non_positive.Tools.content "line_from must be positive") "read_file should explain invalid line_from";
  let reversed =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("line_from", `Int 3);
        ("line_to", `Int 2);
      ])
  in
  expect reversed.Tools.is_error "read_file should reject reversed line ranges";
  expect (reversed.Tools.error_category = Some Tools.InvalidParameters) "read_file should classify reversed ranges";
  expect
    (contains reversed.Tools.content "line_to must be greater than or equal to line_from")
    "read_file should explain reversed ranges";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ read_file_validates_line_ranges"

let test_parallel_flag_validation () =
  let root, skills, registry = temp_registry () in
  let file_path = Filename.concat root "range.txt" in
  write_file file_path "alpha\nbeta\n" ;
  begin
    match Tools.approve_executable registry "echo" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let invalid_read_file =
    Tools.execute registry ~chat_id:1 "read_file"
      (`Assoc [
        ("path", `String file_path);
        ("parallel", `String "yes");
      ])
  in
  expect invalid_read_file.Tools.is_error "read_file should reject non-boolean parallel flags";
  expect (invalid_read_file.Tools.error_category = Some Tools.InvalidParameters) "read_file should classify invalid parallel flags";
  expect (contains invalid_read_file.Tools.content "parallel must be a boolean") "read_file should explain invalid parallel flags";
  let invalid_bash =
    Tools.execute registry ~chat_id:1 "bash"
      (`Assoc [
        ("command", `String "echo ok");
        ("parallel", `Int 1);
      ])
  in
  expect invalid_bash.Tools.is_error "bash should reject non-boolean parallel flags";
  expect (invalid_bash.Tools.error_category = Some Tools.InvalidParameters) "bash should classify invalid parallel flags";
  expect (contains invalid_bash.Tools.content "parallel must be a boolean") "bash should explain invalid parallel flags";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ parallel_flag_validation"

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

let test_bash_requires_shell_approval_for_shell_syntax () =
  let _root, skills, registry = temp_registry () in
  let result =
    Tools.execute registry ~chat_id:1 "bash"
      (`Assoc [("command", `String "printf one | cat")])
  in
  expect result.Tools.is_error "bash should require approval before running shell syntax";
  expect (result.Tools.error_category = Some Tools.ApprovalRequired) "bash should request approval for shell execution";
  begin
    match result.Tools.approval_request with
    | Some request ->
        expect (request.scope = Tools.Execute) "shell approval should target executable scope";
        expect (Filename.basename request.target = "sh") "shell approval should target sh"
    | None -> fail "expected shell approval request"
  end;
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ bash_requires_shell_approval_for_shell_syntax"

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

let test_bash_supports_shell_syntax_after_shell_approval () =
  let _root, skills, registry = temp_registry () in
  begin
    match Tools.approve_executable registry "/bin/sh" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let result =
    Tools.execute registry ~chat_id:1 "bash"
      (`Assoc [("command", `String "printf one | cat")])
  in
  expect (not result.Tools.is_error) "bash should support shell syntax after shell approval";
  expect (contains result.Tools.content "one") "bash shell syntax should produce command output";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ bash_supports_shell_syntax_after_shell_approval"

let test_list_approvals_formatted () =
  let root, skills, registry = temp_registry () in
  let project_root = Unix.realpath root in
  let read_parent = temp_dir () in
  let read_alpha = Filename.concat read_parent "alpha" in
  let read_beta = Filename.concat read_parent "beta" in
  Unix.mkdir read_alpha 0o755;
  Unix.mkdir read_beta 0o755;
  let read_alpha = Unix.realpath read_alpha in
  let read_beta = Unix.realpath read_beta in
  let write_parent = temp_dir () in
  let write_root = Filename.concat write_parent "writes" in
  Unix.mkdir write_root 0o755;
  begin
    match Tools.approve_root registry ~scope:Tools.Read root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_root registry ~scope:Tools.Write root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_root registry ~scope:Tools.Read read_beta with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_root registry ~scope:Tools.Read read_alpha with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_root registry ~scope:Tools.Write write_root with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_executable registry "echo" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  begin
    match Tools.approve_install registry "demo-skill" with
    | Ok _ -> ()
    | Error err -> fail err
  end;
  let output = Tools.list_approvals_formatted registry in
  expect (contains output "Approved tools (executables):") "approval listing should include executables";
  expect (contains output "Approved read roots:") "approval listing should include read roots";
  expect (contains output "Approved write roots:") "approval listing should include write roots";
  expect (contains output "Approved skill installs:") "approval listing should include installs";
  expect
    (count_occurrences output (project_root ^ " (project root, implicit)") = 2)
    "project root should appear once each for read and write";
  expect
    (not (contains output ("- " ^ project_root ^ "\n")))
    "project root should not be duplicated as an explicit raw entry";
  let alpha_idx =
    match find_index output ("- " ^ read_alpha) with
    | Some idx -> idx
    | None -> fail "missing alpha read root in listing"
  in
  let beta_idx =
    match find_index output ("- " ^ read_beta) with
    | Some idx -> idx
    | None -> fail "missing beta read root in listing"
  in
  expect (alpha_idx < beta_idx) "read roots should remain sorted";
  let read_only = Tools.list_approvals_formatted ~scope:Tools.Read registry in
  expect (contains read_only "Approved read roots:") "filtered listing should keep the selected section";
  expect (not (contains read_only "Approved tools (executables):")) "filtered listing should omit other sections";
  expect (not (contains read_only "Approved write roots:")) "filtered listing should omit write roots";
  expect (not (contains read_only "Approved skill installs:")) "filtered listing should omit installs";
  Tools.close registry;
  Agent_skills.Skills.close skills;
  print_endline "  ✓ list_approvals_formatted"

let () =
  print_endline "Running tool approval and error tests...";
  test_read_file_requires_approval ();
  test_read_file_succeeds_after_approval ();
  test_read_file_supports_line_ranges ();
  test_read_file_validates_line_ranges ();
  test_parallel_flag_validation ();
  test_write_allowed_in_project_root ();
  test_write_requires_separate_write_approval_outside_project_root ();
  test_edit_pattern_errors_are_structured ();
  test_bash_timeout_validation ();
  test_bash_requires_executable_approval ();
  test_bash_requires_shell_approval_for_shell_syntax ();
  test_bash_succeeds_after_executable_approval ();
  test_bash_supports_shell_syntax_after_shell_approval ();
  test_list_approvals_formatted ();
  print_endline "[PASS] tool approval and error tests"
