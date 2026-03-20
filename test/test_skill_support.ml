module Skills = Agent_skills.Skills
module Tools = Agent_runtime.Tools

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let temp_dir () =
  let path = Filename.temp_file "oclaw-skills-" "" in
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

let sample_skill name description body =
  Printf.sprintf
    "---\nname: %s\ndescription: %s\nallowed-tools: Read Write\n---\n\n%s\n"
    name
    description
    body

let with_registry f =
  let root = temp_dir () in
  let db_path = Filename.concat root "oclaw.db" in
  let project_skills_dir = Filename.concat root ".agents/skills" in
  let user_skills_dir = Filename.concat root "user-skills" in
  let catalog_cache_path = Filename.concat root "skills-catalog.json" in
  Unix.mkdir (Filename.concat root ".agents") 0o755;
  Unix.mkdir project_skills_dir 0o755;
  Unix.mkdir user_skills_dir 0o755;
  let skills =
    Skills.create
      ~db_path
      ~project_root:root
      ~project_skills_dir
      ~user_skills_dir
      ~catalog_cache_path
      ()
  in
  let registry = Tools.create_default_registry ~db_path ~project_root:root ~skills () in
  Fun.protect
    ~finally:(fun () ->
      Tools.close registry;
      Skills.close skills)
    (fun () -> f root skills registry)

let test_project_skill_requires_trust () =
  with_registry (fun root skills _registry ->
    let skill_dir = Filename.concat root ".agents/skills/demo-skill" in
    write_file (Filename.concat skill_dir "SKILL.md")
      (sample_skill "demo-skill" "Project demo skill" "Use this project skill carefully.");
    let visible = Skills.discover_skills skills in
    expect (visible = []) "project skill should be hidden before trust";
    let visible_all = Skills.discover_skills ~include_untrusted:true skills in
    expect (List.length visible_all = 1) "project skill should still be discoverable for diagnostics";
    expect (not (List.hd visible_all).trusted) "project skill should be marked untrusted";
    match Skills.trust_project skills with
    | Error err -> fail err
    | Ok _ ->
        let trusted = Skills.discover_skills skills in
        expect (List.length trusted = 1) "project skill should become available after trust")

let test_activate_skill_tool_and_allowlist () =
  with_registry (fun root skills registry ->
    let skill_dir = Filename.concat root ".agents/skills/demo-skill" in
    write_file (Filename.concat skill_dir "SKILL.md")
      (sample_skill "demo-skill" "Project demo skill" "Follow the instructions in this skill.");
    begin
      match Skills.trust_project skills with
      | Ok _ -> ()
      | Error err -> fail err
    end;
    let tool_names = Tools.definitions registry |> List.map (fun tool -> tool.Llm_types.name) in
    expect (List.mem "activate_skill" tool_names) "activate_skill tool should be exposed when skills are available";
    let result = Tools.execute registry ~chat_id:7 "activate_skill" (`Assoc [("name", `String "demo-skill")]) in
    expect (not result.Tools.is_error) "activate_skill should succeed for trusted skill";
    expect (String.contains result.Tools.content 'F') "activate_skill should return skill content";
    let read_result =
      Tools.execute registry ~chat_id:7 "read_file"
        (`Assoc [("path", `String (Filename.concat skill_dir "SKILL.md"))])
    in
    expect (not read_result.Tools.is_error) "allowed-tools Read should pre-approve the skill directory")

let test_direct_activation_matches_tool_behavior () =
  with_registry (fun root skills registry ->
    let skill_dir = Filename.concat root ".agents/skills/demo-skill" in
    write_file (Filename.concat skill_dir "SKILL.md")
      (sample_skill "demo-skill" "Project demo skill" "Follow the instructions in this skill.");
    begin
      match Skills.trust_project skills with
      | Ok _ -> ()
      | Error err -> fail err
    end;
    let direct = Tools.activate_skill registry ~chat_id:11 "demo-skill" in
    expect (not direct.Tools.is_error) "direct activation helper should succeed";
    let read_result =
      Tools.execute registry ~chat_id:11 "read_file"
        (`Assoc [("path", `String (Filename.concat skill_dir "SKILL.md"))])
    in
    expect (not read_result.Tools.is_error) "direct activation should apply the same allowed-tools approvals")

let test_skill_install_requires_approval () =
  with_registry (fun _root _skills registry ->
    let result = Tools.execute registry ~chat_id:1 "skill_install" (`Assoc [("name", `String "nonexistent")]) in
    expect result.Tools.is_error "skill_install should require approval before installation";
    expect (result.Tools.error_category = Some Tools.ApprovalRequired) "skill_install should request approval";
    match result.Tools.approval_request with
    | Some request -> expect (request.scope = Tools.Install) "skill_install should use install approval scope"
    | None -> fail "expected install approval request")

let () =
  test_project_skill_requires_trust ();
  test_activate_skill_tool_and_allowlist ();
  test_direct_activation_matches_tool_behavior ();
  test_skill_install_requires_approval ();
  Printf.printf "[PASS] skill support tests\n"
