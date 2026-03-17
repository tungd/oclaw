module Http = Http_client

type skill_metadata = {
  name : string;
  description : string;
  path : string;
}

type t = {
  skills_dir : string;
}

let create ~skills_dir =
  { skills_dir }

let skills_dir t =
  t.skills_dir

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

let read_file path =
  try Some (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with _ -> None

let first_description_line content =
  content
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.find_opt (fun line ->
         line <> ""
         && not (String.starts_with ~prefix:"#"
                   line)
         && not (String.starts_with ~prefix:"---" line))
  |> Option.value ~default:"No description available."

let discover_skills t =
  let entries =
    try Sys.readdir t.skills_dir |> Array.to_list
    with _ -> []
  in
  entries
  |> List.filter_map (fun name ->
         let skill_path = Filename.concat t.skills_dir (Filename.concat name "SKILL.md") in
         if Sys.file_exists skill_path then
           let description =
             match read_file skill_path with
             | Some content -> first_description_line content
             | None -> "No description available."
           in
           Some { name; description; path = skill_path }
         else
           None)
  |> List.sort (fun left right -> String.compare left.name right.name)

let build_skills_catalog t =
  let skills = discover_skills t in
  if skills = [] then ""
  else
    let lines =
      skills
      |> List.map (fun skill -> Printf.sprintf "- %s: %s" skill.name skill.description)
    in
    "<available_skills>\n" ^ String.concat "\n" lines ^ "\n</available_skills>"

let list_skills_formatted t =
  let skills = discover_skills t in
  if skills = [] then "No skills available."
  else
    let lines =
      skills
      |> List.map (fun skill -> Printf.sprintf "- %s: %s" skill.name skill.description)
    in
    "Available skills:\n\n" ^ String.concat "\n" lines

let activate_skill t name =
  let path = Filename.concat t.skills_dir (Filename.concat name "SKILL.md") in
  match read_file path with
  | Some content -> Ok content
  | None ->
      let available =
        discover_skills t |> List.map (fun skill -> skill.name)
      in
      let hint =
        match available with
        | [] -> " No skills are currently available."
        | _ -> " Available skills: " ^ String.concat ", " available
      in
      Error ("Skill '" ^ name ^ "' not found." ^ hint)

let sync_skill t ?(repo="vercel-labs/skills") name =
  let url = Printf.sprintf "https://raw.githubusercontent.com/%s/main/skills/%s/SKILL.md" repo name in
  let response = Http.get url [] 30 in
  match response.Http.HttpResponse.error with
  | Some error -> Error error
  | None when response.Http.HttpResponse.status < 200 || response.Http.HttpResponse.status >= 300 ->
      Error (Printf.sprintf "Failed to download skill '%s' from %s (status %d)"
               name url response.Http.HttpResponse.status)
  | None ->
      let target_dir = Filename.concat t.skills_dir name in
      let target_path = Filename.concat target_dir "SKILL.md" in
      begin
        try
          ensure_dir target_dir;
          Stdlib.Out_channel.with_open_bin target_path (fun channel ->
              output_string channel response.Http.HttpResponse.body);
          Ok (Printf.sprintf "Skill '%s' synced to %s" name target_path)
        with exn ->
          Error (Printexc.to_string exn)
      end
