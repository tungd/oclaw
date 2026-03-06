let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let string_contains ~haystack ~needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if n_len = 0 then true
    else if i > h_len - n_len then false
    else if String.sub haystack i n_len = needle then true
    else loop (i + 1)
  in
  loop 0

let required_skill_names = [
  "Git Expert";
  "OCaml Pro";
  "Web Research";
  "Python CLI";
  "Skill Ops";
  "Scheduler";
]

let () =
  let skills = Skills.load_skills () in
  let names = List.map (fun (skill : Skills.Skill.t) -> skill.name) skills in
  List.iter (fun name ->
    expect (List.mem name names) ("missing skill: " ^ name)
  ) required_skill_names;

  let prompt = Skills.skills_to_prompt skills in
  List.iter (fun name ->
    expect (string_contains ~haystack:prompt ~needle:name)
      ("skills prompt missing skill name: " ^ name)
  ) required_skill_names;

  Printf.printf "[PASS] skills smoke test (%d skills loaded)\n" (List.length skills)
