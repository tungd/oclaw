type layout = {
  start_dir : string;
  project_root : string;
  agents_dir : string;
  db_path : string;
  project_skills_dir : string;
  user_agents_dir : string;
  user_skills_dir : string;
  catalog_cache_path : string;
}

let is_directory path =
  try Sys.is_directory path with
  | Sys_error _ -> false

let ensure_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p path

let can_prepare_dir path =
  try
    ensure_dir path;
    true
  with
  | Unix.Unix_error _ -> false
  | Sys_error _ -> false

let normalize_dir path =
  let absolute =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  let candidate =
    if Sys.file_exists absolute && not (is_directory absolute) then
      Filename.dirname absolute
    else
      absolute
  in
  if Sys.file_exists candidate then Unix.realpath candidate else candidate

let find_existing_agents_root ~start_dir =
  let home = normalize_dir (Sys.getenv "HOME") in
  let rec loop current =
    let agents_dir = Filename.concat current ".agents" in
    if is_directory agents_dir then Some current
    else if current = home || current = "/" then None
    else
      let parent = Filename.dirname current in
      if parent = current then None else loop parent
  in
  loop start_dir

let discover ?start_dir () =
  let start_dir = normalize_dir (Option.value ~default:(Sys.getcwd ()) start_dir) in
  let home = normalize_dir (Sys.getenv "HOME") in
  let project_root =
    match find_existing_agents_root ~start_dir with
    | Some root -> root
    | None -> start_dir
  in
  let agents_dir = Filename.concat project_root ".agents" in
  ensure_dir agents_dir;
  let project_skills_dir = Filename.concat agents_dir "skills" in
  ensure_dir project_skills_dir;
  let preferred_user_agents_dir = Filename.concat home ".oclaw" in
  let preferred_user_skills_dir = Filename.concat preferred_user_agents_dir "skills" in
  let fallback_user_agents_dir = Filename.concat agents_dir "user" in
  let user_agents_dir =
    if can_prepare_dir preferred_user_skills_dir then preferred_user_agents_dir
    else (
      ensure_dir fallback_user_agents_dir;
      fallback_user_agents_dir
    )
  in
  let user_skills_dir = Filename.concat user_agents_dir "skills" in
  ensure_dir user_skills_dir;
  {
    start_dir;
    project_root;
    agents_dir;
    db_path = Filename.concat agents_dir "oclaw.db";
    project_skills_dir;
    user_agents_dir;
    user_skills_dir;
    catalog_cache_path = Filename.concat user_agents_dir "skills-catalog.json";
  }
