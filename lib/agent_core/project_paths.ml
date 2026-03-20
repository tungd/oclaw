type layout = {
  start_dir : string;
  project_root : string;
  agents_dir : string;
  db_path : string;
  skills_dir : string;
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
  let project_root =
    match find_existing_agents_root ~start_dir with
    | Some root -> root
    | None -> start_dir
  in
  let agents_dir = Filename.concat project_root ".agents" in
  ensure_dir agents_dir;
  let skills_dir = Filename.concat agents_dir "skills" in
  ensure_dir skills_dir;
  {
    start_dir;
    project_root;
    agents_dir;
    db_path = Filename.concat agents_dir "oclaw.db";
    skills_dir;
  }
