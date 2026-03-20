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

let () =
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
  let actual_tools = Agent_runtime.Tools.definitions registry |> List.map (fun tool -> tool.Llm_types.name) in
  expect (actual_tools = expected_tools) "default tool registry does not match expected surface";
  Agent_runtime.Tools.close registry;
  Agent_skills.Skills.close skills;
  Printf.printf "[PASS] tool registry smoke test (%d tools)\n" (List.length actual_tools)
