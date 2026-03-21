open Httpkit

module Yaml_lib = Yaml

type scope =
  | Builtin
  | User
  | Project

type allowed_tool =
  | Allowed_read
  | Allowed_write
  | Allowed_bash of string
  | Allowed_other of string

type skill_metadata = {
  name : string;
  description : string;
  license : string option;
  compatibility : string option;
  metadata : (string * string) list;
  allowed_tools : allowed_tool list;
  path : string;
  dir : string;
  body : string;
  scope : scope;
  resources : string list;
}

type remote_skill = {
  name : string;
  description : string;
  repo : string;
  git_ref : string;
  root_path : string;
  files : string list;
}

type activation_result = {
  content : string;
  already_activated : bool;
  allowed_tools : allowed_tool list;
  skill_dir : string;
}

type t = {
  project_skills_dir : string;
  user_skills_dir : string;
  builtin_skills_dirs : string list;
  catalog_cache_path : string;
  activated_by_chat : (int, (string, unit) Hashtbl.t) Hashtbl.t;
}

type parsed_skill = {
  name : string;
  description : string;
  license : string option;
  compatibility : string option;
  metadata : (string * string) list;
  allowed_tools : allowed_tool list;
  body : string;
}

let github_repo = "anthropics/skills"
let github_ref = "main"
let github_tree_url =
  Printf.sprintf "https://api.github.com/repos/%s/git/trees/%s?recursive=1" github_repo github_ref

let normalize_dir path =
  let absolute =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  if Sys.file_exists absolute then Unix.realpath absolute else absolute

let create ~project_skills_dir ~user_skills_dir ~catalog_cache_path ?(builtin_skills_dirs=[]) () =
  {
    project_skills_dir = normalize_dir project_skills_dir;
    user_skills_dir = normalize_dir user_skills_dir;
    builtin_skills_dirs = List.map normalize_dir builtin_skills_dirs;
    catalog_cache_path = normalize_dir catalog_cache_path;
    activated_by_chat = Hashtbl.create 8;
  }

let close t =
  let _ = t in
  ()

let skills_dir t =
  t.project_skills_dir

let user_skills_dir t =
  t.user_skills_dir

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

let write_file_atomic path content =
  ensure_dir (Filename.dirname path);
  let tmp_path = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  Stdlib.Out_channel.with_open_bin tmp_path (fun channel -> output_string channel content);
  Unix.rename tmp_path path

let read_file path =
  try Some (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with _ -> None

let scope_rank = function
  | Builtin -> 0
  | User -> 1
  | Project -> 2

let scope_to_string = function
  | Builtin -> "builtin"
  | User -> "user"
  | Project -> "project"

let yaml_assoc = function
  | `O fields -> fields
  | _ -> []

let yaml_string = function
  | `String value -> Some value
  | `Float value -> Some (string_of_float value)
  | `Bool value -> Some (string_of_bool value)
  | `Null -> Some ""
  | _ -> None

let parse_allowed_tools raw =
  raw
  |> String.split_on_char ' '
  |> List.filter (fun part -> String.trim part <> "")
  |> List.map (fun part ->
         if String.equal part "Read" then Allowed_read
         else if String.equal part "Write" then Allowed_write
         else if String.starts_with ~prefix:"Bash(" part && String.ends_with ~suffix:")" part then
           let inner = String.sub part 5 (String.length part - 6) in
           let command =
             match String.index_opt inner ':' with
             | Some idx -> String.sub inner 0 idx
             | None -> inner
           in
           Allowed_bash command
         else
           Allowed_other part)

let validate_name name =
  let len = String.length name in
  len >= 1
  && len <= 64
  && not (String.starts_with ~prefix:"-" name)
  && not (String.ends_with ~suffix:"-" name)
  &&
  let rec loop idx prev_hyphen =
    if idx >= len then true
    else
      let ch = String.unsafe_get name idx in
      let is_valid =
        (ch >= 'a' && ch <= 'z')
        || (ch >= '0' && ch <= '9')
        || ch = '-'
      in
      if not is_valid then false
      else if ch = '-' && prev_hyphen then false
      else loop (idx + 1) (ch = '-')
  in
  loop 0 false

let parse_frontmatter content =
  if not (String.starts_with ~prefix:"---\n" content) then
    Error "SKILL.md is missing YAML frontmatter"
  else
    let rest = String.sub content 4 (String.length content - 4) in
    let closing =
      let needle = "\n---" in
      let needle_len = String.length needle in
      let rec loop idx =
        if idx > String.length rest - needle_len then None
        else if String.sub rest idx needle_len = needle then Some idx
        else loop (idx + 1)
      in
      loop 0
    in
    match closing with
    | None -> Error "SKILL.md frontmatter is missing a closing ---"
    | Some idx ->
        let yaml_text = String.sub rest 0 idx in
        let body_start = idx + 4 in
        let body_len = String.length rest - body_start in
        let body =
          if body_len <= 0 then ""
          else String.trim (String.sub rest body_start body_len)
        in
        match Yaml_lib.of_string yaml_text with
        | Error (`Msg msg) -> Error msg
        | Ok yaml ->
            let fields = yaml_assoc yaml in
            let lookup key =
              match List.assoc_opt key fields with
              | Some value -> yaml_string value
              | None -> None
            in
            let metadata =
              match List.assoc_opt "metadata" fields with
              | Some (`O metadata_fields) ->
                  metadata_fields
                  |> List.filter_map (fun (key, value) ->
                         yaml_string value |> Option.map (fun v -> (key, v)))
              | _ -> []
            in
            begin
              match lookup "name", lookup "description" with
              | Some name, Some description when validate_name name && String.trim description <> "" ->
                  Ok {
                    name;
                    description;
                    license = lookup "license";
                    compatibility = lookup "compatibility";
                    metadata;
                    allowed_tools =
                      (match lookup "allowed-tools" with
                       | Some value -> parse_allowed_tools value
                       | None -> []);
                    body;
                  }
              | Some name, _ when not (validate_name name) ->
                  Error (Printf.sprintf "invalid skill name: %s" name)
              | _, Some description when String.trim description = "" ->
                  Error "description is required"
              | _ ->
                  Error "name and description are required"
            end

let list_skill_resources dir =
  let rec walk acc root rel =
    let path = if rel = "" then root else Filename.concat root rel in
    if Sys.file_exists path && Sys.is_directory path then
      Sys.readdir path
      |> Array.to_list
      |> List.sort String.compare
      |> List.fold_left (fun acc name ->
             if String.equal name ".git" || String.equal name "node_modules" then acc
             else
               let child_rel = if rel = "" then name else Filename.concat rel name in
               walk acc root child_rel)
           acc
    else if rel = "" || String.equal rel "SKILL.md" then
      acc
    else
      rel :: acc
  in
  if Sys.file_exists dir then
    walk [] dir "" |> List.rev
  else
    []

let scan_root ~scope dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.sort String.compare
    |> List.filter_map (fun entry ->
           let skill_dir = Filename.concat dir entry in
           let skill_path = Filename.concat skill_dir "SKILL.md" in
           if not (Sys.file_exists skill_path) then None
           else
             match read_file skill_path with
             | None -> None
             | Some content ->
                 match parse_frontmatter content with
                 | Error _ -> None
                 | Ok parsed ->
                     Some {
                       name = parsed.name;
                       description = parsed.description;
                       license = parsed.license;
                       compatibility = parsed.compatibility;
                       metadata = parsed.metadata;
                       allowed_tools = parsed.allowed_tools;
                       path = skill_path;
                       dir = skill_dir;
                       body = parsed.body;
                       scope;
                       resources = list_skill_resources skill_dir;
                     })

let dedupe_skills (skills : skill_metadata list) =
  let table = Hashtbl.create 32 in
  List.iter
    (fun (skill : skill_metadata) ->
      match Hashtbl.find_opt table skill.name with
      | None -> Hashtbl.replace table skill.name skill
      | Some (existing : skill_metadata) ->
          if scope_rank skill.scope > scope_rank existing.scope then
            Hashtbl.replace table skill.name skill)
    skills;
  Hashtbl.to_seq_values table
  |> List.of_seq
  |> List.sort (fun (left : skill_metadata) (right : skill_metadata) -> String.compare left.name right.name)

let discover_skills t : skill_metadata list =
  let builtin =
    t.builtin_skills_dirs
    |> List.concat_map (fun dir -> scan_root ~scope:Builtin dir)
  in
  let user = scan_root ~scope:User t.user_skills_dir in
  let project = scan_root ~scope:Project t.project_skills_dir in
  dedupe_skills (builtin @ user @ project)

let available_skill_names t =
  let skills : skill_metadata list = discover_skills t in
  skills |> List.map (fun (skill : skill_metadata) -> skill.name)

let build_skills_catalog t =
  let skills : skill_metadata list = discover_skills t in
  if skills = [] then ""
  else
    let lines =
      skills
      |> List.map (fun (skill : skill_metadata) ->
             Printf.sprintf "- %s: %s" skill.name skill.description)
    in
    "<available_skills>\n" ^ String.concat "\n" lines ^ "\n</available_skills>"

let list_skills_formatted t =
  let skills : skill_metadata list = discover_skills t in
  if skills = [] then "No skills available."
  else
    let lines =
      skills
      |> List.map (fun (skill : skill_metadata) ->
             Printf.sprintf "- %s (%s): %s"
               skill.name
               (scope_to_string skill.scope)
               skill.description)
    in
    "Available skills:\n\n" ^ String.concat "\n" lines

let activated_for_chat t chat_id =
  match Hashtbl.find_opt t.activated_by_chat chat_id with
  | Some table -> table
  | None ->
      let table = Hashtbl.create 8 in
      Hashtbl.add t.activated_by_chat chat_id table;
      table

let activate_skill t ~chat_id name =
  let skills : skill_metadata list = discover_skills t in
  match List.find_opt (fun (skill : skill_metadata) -> String.equal skill.name name) skills with
  | None ->
      let all : string list =
        discover_skills t
        |> List.map (fun (skill : skill_metadata) -> skill.name)
      in
      let hint =
        if all = [] then " No skills are currently available."
        else " Available skills: " ^ String.concat ", " all
      in
      Error ("Skill '" ^ name ^ "' not found." ^ hint)
  | Some skill ->
      let table = activated_for_chat t chat_id in
      let already_activated = Hashtbl.mem table skill.name in
      Hashtbl.replace table skill.name ();
      let resource_lines =
        skill.resources
        |> List.sort String.compare
        |> List.map (fun file -> "  <file>" ^ file ^ "</file>")
      in
      let resources_block =
        if resource_lines = [] then ""
        else
          "\n<skill_resources>\n"
          ^ String.concat "\n" resource_lines
          ^ "\n</skill_resources>"
      in
      Ok {
        content =
          Printf.sprintf
            "<skill_content name=\"%s\">\n%s\n\nSkill directory: %s\nRelative paths in this skill are relative to the skill directory.%s\n</skill_content>"
            skill.name
            skill.body
            skill.dir
            resources_block;
        already_activated;
        allowed_tools = skill.allowed_tools;
        skill_dir = skill.dir;
      }

let string_contains_ci haystack needle =
  let haystack = String.lowercase_ascii haystack in
  let needle = String.lowercase_ascii needle in
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop idx =
      if idx > hay_len - needle_len then false
      else if String.sub haystack idx needle_len = needle then true
      else loop (idx + 1)
    in
    loop 0

let matches_query ~name ~description query =
  query = ""
  || string_contains_ci name query
  || string_contains_ci description query

let json_string_member json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Some value
  | _ -> None

let json_list_member json name =
  match Yojson.Safe.Util.member name json with
  | `List values -> values
  | _ -> []

let parse_remote_catalog_json body =
  let json = Yojson.Safe.from_string body in
  let skills =
    match json with
    | `Assoc fields ->
        begin
          match List.assoc_opt "skills" fields with
          | Some (`List entries) ->
              entries
              |> List.filter_map (function
                     | `Assoc _ as entry ->
                         begin
                           match json_string_member entry "name",
                                 json_string_member entry "description",
                                 json_string_member entry "repo",
                                 json_string_member entry "git_ref",
                                 json_string_member entry "root_path" with
                           | Some name, Some description, Some repo, Some git_ref, Some root_path ->
                               let files =
                                 json_list_member entry "files"
                                 |> List.filter_map (function
                                        | `String value -> Some value
                                        | _ -> None)
                               in
                               Some { name; description; repo; git_ref; root_path; files }
                           | _ -> None
                         end
                     | _ -> None)
          | _ -> []
        end
    | _ -> []
  in
  skills

let save_remote_catalog_cache t (skills : remote_skill list) =
  let json =
    `Assoc [
      ("updated_at", `Float (Unix.gettimeofday ()));
      ("skills",
       `List (
         skills
         |> List.map (fun (skill : remote_skill) ->
                `Assoc [
                  ("name", `String skill.name);
                  ("description", `String skill.description);
                  ("repo", `String skill.repo);
                  ("git_ref", `String skill.git_ref);
                  ("root_path", `String skill.root_path);
                  ("files", `List (List.map (fun file -> `String file) skill.files));
                ])));
    ]
  in
  write_file_atomic t.catalog_cache_path (Yojson.Safe.pretty_to_string json)

let load_remote_catalog_cache t =
  match read_file t.catalog_cache_path with
  | None -> None
  | Some body ->
      try Some (parse_remote_catalog_json body)
      with _ -> None

let fetch_url url =
  let request = H1.Request.create `GET url in
  match Client.execute_request ~timeout:60 request with
  | Error error -> Error error
  | Ok (response, body) ->
      let code = H1.Status.to_code response.H1.Response.status in
      if code >= 200 && code < 300 then Ok body
      else Error (Printf.sprintf "Failed to fetch %s (status %d)" url code)

let raw_github_url ~repo ~git_ref ~path =
  Printf.sprintf "https://raw.githubusercontent.com/%s/%s/%s" repo git_ref path

let remote_tree_entries body =
  let json = Yojson.Safe.from_string body in
  match Yojson.Safe.Util.member "tree" json with
  | `List entries ->
      entries
      |> List.filter_map (function
             | `Assoc _ as entry ->
                 begin
                   match json_string_member entry "path", json_string_member entry "type" with
                   | Some path, Some typ -> Some (path, typ)
                   | _ -> None
                 end
             | _ -> None)
  | _ -> []

let refresh_remote_catalog t =
  match fetch_url github_tree_url with
  | Error err ->
      begin
        match load_remote_catalog_cache t with
        | Some cached -> Ok cached
        | None -> Error err
      end
  | Ok body ->
      let entries = remote_tree_entries body in
      let skill_files =
        entries
        |> List.filter_map (fun (path, typ) ->
               if String.equal typ "blob"
                  && String.starts_with ~prefix:"skills/" path
                  && String.ends_with ~suffix:"/SKILL.md" path
               then
                 let parts = String.split_on_char '/' path in
                 match parts with
                 | "skills" :: name :: _ -> Some (name, path)
                 | _ -> None
               else
                 None)
      in
      let files_by_skill =
        let table = Hashtbl.create 64 in
        List.iter (fun (path, typ) ->
            if String.equal typ "blob" && String.starts_with ~prefix:"skills/" path then
              match String.split_on_char '/' path with
              | "skills" :: name :: _ ->
                  let files = Option.value ~default:[] (Hashtbl.find_opt table name) in
                  Hashtbl.replace table name (path :: files)
              | _ -> ())
          entries;
        table
      in
      let skills : remote_skill list =
        skill_files
        |> List.filter_map (fun (name_hint, skill_path) ->
               match fetch_url (raw_github_url ~repo:github_repo ~git_ref:github_ref ~path:skill_path) with
               | Error _ -> None
               | Ok skill_body ->
                   match parse_frontmatter skill_body with
                   | Error _ -> None
                   | Ok parsed ->
                       let files = Option.value ~default:[] (Hashtbl.find_opt files_by_skill name_hint) |> List.sort String.compare in
                       Some {
                         name = parsed.name;
                         description = parsed.description;
                         repo = github_repo;
                         git_ref = github_ref;
                         root_path = Filename.dirname skill_path;
                         files;
                       })
      in
      let skills =
        List.sort (fun (left : remote_skill) (right : remote_skill) -> String.compare left.name right.name) skills
      in
      save_remote_catalog_cache t skills;
      Ok skills

let remote_catalog t =
  match load_remote_catalog_cache t with
  | Some cached when cached <> [] -> Ok cached
  | _ -> refresh_remote_catalog t

let search_remote_catalog t ~query =
  match remote_catalog t with
  | Error err -> Error err
  | Ok skills ->
      Ok (List.filter
            (fun (skill : remote_skill) ->
              matches_query ~name:skill.name ~description:skill.description query)
            skills)

let install_remote_skill t ~name =
  match remote_catalog t with
  | Error err -> Error err
  | Ok skills ->
      begin
        match List.find_opt (fun (skill : remote_skill) -> String.equal skill.name name) skills with
        | None -> Error (Printf.sprintf "Remote skill '%s' not found." name)
        | Some skill ->
            let target_dir = Filename.concat t.user_skills_dir skill.name in
            begin
              try
                ensure_dir target_dir;
                List.iter (fun path ->
                    let rel =
                      if String.starts_with ~prefix:(skill.root_path ^ "/") path then
                        String.sub path (String.length skill.root_path + 1) (String.length path - String.length skill.root_path - 1)
                      else if String.equal path (skill.root_path ^ "/SKILL.md") then
                        "SKILL.md"
                      else if String.equal path (Filename.concat skill.root_path "SKILL.md") then
                        "SKILL.md"
                      else if String.equal path skill.root_path then
                        Filename.basename path
                      else
                        let prefix = skill.root_path ^ "/" in
                        if String.starts_with ~prefix path then
                          String.sub path (String.length prefix) (String.length path - String.length prefix)
                        else
                          Filename.basename path
                    in
                    let body = raw_github_url ~repo:skill.repo ~git_ref:skill.git_ref ~path in
                    match fetch_url body with
                    | Error err -> raise (Failure err)
                    | Ok content ->
                        let out_path = Filename.concat target_dir rel in
                        write_file_atomic out_path content)
                  skill.files;
                Ok (Printf.sprintf "Installed skill '%s' to %s" skill.name target_dir)
              with
              | Failure err -> Error err
              | exn -> Error (Printexc.to_string exn)
            end
      end
