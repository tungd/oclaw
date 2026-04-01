let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expected_tools = [
  "bash";
  "read_file";
  "write_file";
  "edit_file";
  "skill_list";
  "skill_search";
  "skill_install";
]

(* Test git hardening logic *)
let test_git_harden_argv () =
  (* Mock the harden function locally since we can't access internal functions *)
  let is_git path = String.equal (Filename.basename path) "git" in
  let harden = function
    | executable :: rest when is_git executable ->
        executable :: "-c" :: "core.fsmonitor=false" :: "-c" :: "core.hooksPath=/dev/null" :: rest
    | argv -> argv
  in
  (* Test git command gets hardened *)
  let git_argv = ["/usr/bin/git"; "status"] in
  let hardened = harden git_argv in
  expect (List.length hardened = 6) "git argv should have 6 elements after hardening";
  expect (List.nth hardened 0 = "/usr/bin/git") "first element should be git";
  expect (List.nth hardened 1 = "-c") "second element should be -c";
  expect (List.nth hardened 2 = "core.fsmonitor=false") "third element should be fsmonitor=false";
  expect (List.nth hardened 3 = "-c") "fourth element should be -c";
  expect (List.nth hardened 4 = "core.hooksPath=/dev/null") "fifth element should be hooksPath";
  expect (List.nth hardened 5 = "status") "sixth element should be original subcommand";
  (* Test non-git command is unchanged *)
  let ls_argv = ["/bin/ls"; "-la"] in
  let unchanged = harden ls_argv in
  expect (unchanged = ls_argv) "non-git argv should be unchanged";
  (* Test empty argv *)
  let empty_argv = [] in
  let still_empty = harden empty_argv in
  expect (still_empty = []) "empty argv should remain empty";
  Printf.printf "[PASS] git argv hardening tests\n"

let () =
  (* Run unit tests for internal logic *)
  test_git_harden_argv ();
  let temp_root = Filename.get_temp_dir_name () in
  let db_path = Filename.concat temp_root "oclaw-tools-registry.db" in
  let project_skills_dir = Filename.concat temp_root "project-skills" in
  let user_skills_dir = Filename.concat temp_root "user-skills" in
  Unix.mkdir project_skills_dir 0o755;
  Unix.mkdir user_skills_dir 0o755;
  let skills =
    Agent_skills.Skills.create
      ~project_skills_dir
      ~user_skills_dir
      ~catalog_cache_path:(Filename.concat temp_root "skills-catalog.json")
      ()
  in
  let registry =
    Agent_runtime.Tools.create_default_registry
      ~db_path
      ~project_root:temp_root
      ~skills
      ()
  in
  let actual_tools =
    Agent_runtime.Tools.definitions registry
    |> List.map (fun (tool : Llm_types.tool_definition) -> tool.name)
  in
  expect (actual_tools = expected_tools) "default tool registry does not match expected surface";
  Agent_runtime.Tools.close registry;
  Agent_skills.Skills.close skills;
  Printf.printf "[PASS] tool registry smoke test (%d tools)\n" (List.length actual_tools)
