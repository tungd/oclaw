(** CLI tools with approval-gated command and filesystem access. *)

type error_category =
  | FileNotFound
  | PermissionDenied
  | DirectoryMissing
  | PatternNotFound
  | AmbiguousMatch
  | CommandFailed
  | CommandTimeout
  | CommandNotFound
  | InvalidParameters
  | ApprovalRequired
  | Other

type approval_scope =
  | Read
  | Write
  | Execute
  | Install

type approval_request = {
  scope : approval_scope;
  target : string;
  reason : string;
}

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
  error_category : error_category option;
  recovery_hint : string option;
  approval_request : approval_request option;
}

type t = {
  tools : tool list;
  pool : Domainslib.Task.pool;
  db : Sqlite3.db;
  project_root : string;
  skills : Agent_skills.Skills.t;
}

and tool_exec_fn = registry:t -> chat_id:int -> Yojson.Safe.t -> tool_result

and tool = {
  definition : Llm_types.tool_definition;
  execute : tool_exec_fn;
}

let close registry =
  let _ = Sqlite3.db_close registry.db in
  Domainslib.Task.teardown_pool registry.pool

let pool registry = registry.pool
let project_root registry = registry.project_root

let default_bash_timeout_seconds = 60

let has_path_prefix ~root ~path =
  path = root ||
  let root_len = String.length root in
  let path_len = String.length path in
  path_len > root_len &&
  String.sub path 0 root_len = root &&
  String.unsafe_get path root_len = '/'

let approval_scope_to_string = function
  | Read -> "read"
  | Write -> "write"
  | Execute -> "exec"
  | Install -> "install"

let approval_scope_label = function
  | Read -> "read root"
  | Write -> "write root"
  | Execute -> "executable"
  | Install -> "skill install"

let classify_error ~error_message =
  if String.starts_with ~prefix:"Invalid parameters:" error_message then InvalidParameters
  else if String.starts_with ~prefix:"Approval required:" error_message then ApprovalRequired
  else if String.starts_with ~prefix:"Pattern not found:" error_message then PatternNotFound
  else if String.starts_with ~prefix:"Ambiguous match:" error_message then AmbiguousMatch
  else if String.starts_with ~prefix:"Command timed out:" error_message then CommandTimeout
  else if String.starts_with ~prefix:"Command not found:" error_message then CommandNotFound
  else if String.starts_with ~prefix:"File not found:" error_message then FileNotFound
  else if String.starts_with ~prefix:"Directory missing:" error_message then DirectoryMissing
  else if String.starts_with ~prefix:"Permission denied:" error_message then PermissionDenied
  else if String.starts_with ~prefix:"Command failed:" error_message then CommandFailed
  else Other

let recovery_hint_for_error ~category ~error_message:_ =
  match category with
  | FileNotFound ->
      "Check the path and confirm the file exists inside an approved read or write root."
  | PermissionDenied ->
      "Check filesystem permissions for the target path or executable."
  | DirectoryMissing ->
      "Create or approve the parent directory before retrying the write."
  | PatternNotFound ->
      "Use `read_file` to confirm the current file contents and provide a unique exact match."
  | AmbiguousMatch ->
      "Include more surrounding context in `old_text` so the replacement target is unique."
  | CommandTimeout ->
      "Increase `timeout_seconds` if the command is legitimately long-running, or split the task into smaller steps."
  | CommandNotFound ->
      "Verify the executable exists and is available in PATH, then approve the resolved executable path."
  | CommandFailed ->
      "Inspect the command output and exit status, then correct the arguments or environment before retrying."
  | InvalidParameters ->
      "Review the tool arguments and provide valid values."
  | ApprovalRequired ->
      "Approve the requested executable, root path, or install target, then retry the tool call."
  | Other ->
      "Inspect the error details and retry with corrected inputs."

let success ?status_code ?duration_ms ?error_type content =
  {
    content;
    is_error = false;
    status_code;
    bytes = String.length content;
    duration_ms;
    error_type;
    error_category = None;
    recovery_hint = None;
    approval_request = None;
  }

let failure ?status_code ?duration_ms ?(error_type="tool_error") ?error_category content =
  let category = Option.value error_category ~default:(classify_error ~error_message:content) in
  let hint = Some (recovery_hint_for_error ~category ~error_message:content) in
  {
    content;
    is_error = true;
    status_code;
    bytes = String.length content;
    duration_ms;
    error_type = Some error_type;
    error_category = Some category;
    recovery_hint = hint;
    approval_request = None;
  }

let approval_required request message =
  let hint = Some (recovery_hint_for_error ~category:ApprovalRequired ~error_message:message) in
  {
    content = message;
    is_error = true;
    status_code = None;
    bytes = String.length message;
    duration_ms = None;
    error_type = Some "approval_required";
    error_category = Some ApprovalRequired;
    recovery_hint = hint;
    approval_request = Some request;
  }

let json_assoc_or_empty = function
  | `Assoc fields -> `Assoc fields
  | _ -> `Assoc []

let required_string_arg json name =
  match Yojson.Safe.Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "Invalid parameters: %s is required" name)

let optional_int_arg json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok None
  | `Int value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "Invalid parameters: %s must be an integer" name)

let make_tool name description input_schema execute =
  let definition = {
    Llm_types.name;
    description;
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc input_schema);
      ("required", `List (List.map (fun s -> `String s) (List.map fst input_schema)));
    ];
  } in
  { definition; execute }

let init_db db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc -> failwith (Printf.sprintf "DB error [%s]" (Sqlite3.Rc.to_string rc))
  in
  exec "CREATE TABLE IF NOT EXISTS tool_approvals (scope TEXT NOT NULL, path TEXT NOT NULL, created_at REAL NOT NULL, PRIMARY KEY(scope, path))";
  exec "CREATE INDEX IF NOT EXISTS idx_tool_approvals_scope ON tool_approvals(scope)";
  exec "CREATE TABLE IF NOT EXISTS trusted_projects (path TEXT PRIMARY KEY, created_at REAL NOT NULL)"

let exec_ignore db sql bind =
  let stmt = Sqlite3.prepare db sql in
  bind stmt;
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      let _ = Sqlite3.finalize stmt in
      ()
  | rc ->
      let _ = Sqlite3.finalize stmt in
      failwith (Printf.sprintf "DB error: %s" (Sqlite3.Rc.to_string rc))

let get_all_approved_paths db scope =
  let stmt = Sqlite3.prepare db "SELECT path FROM tool_approvals WHERE scope = ? ORDER BY path" in
  let _ = Sqlite3.bind_text stmt 1 (approval_scope_to_string scope) in
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> loop (Sqlite3.column_text stmt 0 :: acc)
    | Sqlite3.Rc.DONE ->
        let _ = Sqlite3.finalize stmt in
        List.rev acc
    | rc ->
        let _ = Sqlite3.finalize stmt in
        failwith (Printf.sprintf "DB error: %s" (Sqlite3.Rc.to_string rc))
  in
  loop []

let approval_section_title = function
  | Execute -> "Approved tools (executables)"
  | Read -> "Approved read roots"
  | Write -> "Approved write roots"
  | Install -> "Approved skill installs"

let dedupe_display_entries entries =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun (key, display) ->
      if Hashtbl.mem seen key then
        None
      else begin
        Hashtbl.add seen key ();
        Some display
      end)
    entries

let approval_section_entries registry ~scope ~include_project_root =
  let explicit_entries =
    get_all_approved_paths registry.db scope
    |> List.map (fun path -> (path, path))
  in
  let entries =
    match scope with
    | Read | Write when include_project_root ->
        (registry.project_root, registry.project_root ^ " (project root, implicit)") :: explicit_entries
    | Read | Write | Execute | Install ->
        explicit_entries
  in
  dedupe_display_entries entries

let render_approval_section registry ~scope ~include_project_root =
  let entries = approval_section_entries registry ~scope ~include_project_root in
  let lines =
    match entries with
    | [] -> [ "- none" ]
    | values -> List.map (fun value -> "- " ^ value) values
  in
  approval_section_title scope ^ ":\n" ^ String.concat "\n" lines

let get_exact_match_approval db ~scope ~target =
  let stmt = Sqlite3.prepare db "SELECT 1 FROM tool_approvals WHERE scope = ? AND path = ? LIMIT 1" in
  let _ = Sqlite3.bind_text stmt 1 (approval_scope_to_string scope) in
  let _ = Sqlite3.bind_text stmt 2 target in
  let found =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> true
    | Sqlite3.Rc.DONE -> false
    | rc ->
        let _ = Sqlite3.finalize stmt in
        failwith (Printf.sprintf "DB error: %s" (Sqlite3.Rc.to_string rc))
  in
  let _ = Sqlite3.finalize stmt in
  found

let normalize_existing_path path =
  Unix.realpath path

let normalize_lossy_path path =
  let absolute =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  let rec ascend current suffix =
    if Sys.file_exists current then
      let base = normalize_existing_path current in
      match suffix with
      | [] -> base
      | _ -> List.fold_left Filename.concat base (List.rev suffix)
    else
      let parent = Filename.dirname current in
      if parent = current then absolute
      else
        let name = Filename.basename current in
        ascend parent (name :: suffix)
  in
  ascend absolute []

let normalize_project_path registry path =
  normalize_lossy_path (Option.value ~default:registry.project_root path)

let scope_for_root_approval = function
  | Read | Write as scope -> scope
  | Execute | Install -> invalid_arg "root approvals only support read/write"

let normalize_root_path ~scope path =
  let normalized = normalize_lossy_path path in
  match scope with
  | Read | Write ->
      let candidate =
        if Sys.file_exists normalized && not (Sys.is_directory normalized) then
          Filename.dirname normalized
        else
          normalized
      in
      normalize_lossy_path candidate
  | Execute | Install -> invalid_arg "root approvals only support read/write"

let project_root_approved registry ~scope ~path =
  match scope with
  | Read | Write -> has_path_prefix ~root:registry.project_root ~path
  | Execute | Install -> false

let approved registry ~scope ~path =
  project_root_approved registry ~scope ~path ||
  let approved_paths = get_all_approved_paths registry.db scope in
  match scope with
  | Install -> List.exists (fun value -> String.equal value path) approved_paths
  | Read | Write | Execute ->
      List.exists (fun root -> has_path_prefix ~root ~path) approved_paths

let insert_approval registry ~scope ~path =
  exec_ignore registry.db
    "INSERT OR REPLACE INTO tool_approvals (scope, path, created_at) VALUES (?, ?, ?)"
    (fun stmt ->
      let _ = Sqlite3.bind_text stmt 1 (approval_scope_to_string scope) in
      let _ = Sqlite3.bind_text stmt 2 path in
      let _ = Sqlite3.bind_double stmt 3 (Unix.gettimeofday ()) in
      ())

let approve_root_internal registry ~scope path =
  let scope = scope_for_root_approval scope in
  let normalized = normalize_root_path ~scope path in
  insert_approval registry ~scope ~path:normalized;
  normalized

let approve_install_internal registry name =
  insert_approval registry ~scope:Install ~path:name

let resolve_executable_path command_name =
  let is_executable path =
    try
      Unix.access path [Unix.X_OK];
      Sys.file_exists path && not (Sys.is_directory path)
    with Unix.Unix_error _ -> false
  in
  if String.contains command_name '/' then
    let candidate = normalize_lossy_path command_name in
    if is_executable candidate then Ok candidate
    else Error (Printf.sprintf "Command not found: %s" command_name)
  else
    let path_env = Option.value ~default:"" (Sys.getenv_opt "PATH") in
    let entries = if path_env = "" then [] else String.split_on_char ':' path_env in
    match List.find_opt (fun dir -> is_executable (Filename.concat dir command_name)) entries with
    | Some dir -> Ok (Filename.concat dir command_name)
    | None -> Error (Printf.sprintf "Command not found: %s" command_name)

let approve_executable_internal registry executable =
  match resolve_executable_path executable with
  | Error err -> Error err
  | Ok resolved ->
      insert_approval registry ~scope:Execute ~path:resolved;
      Ok resolved

let tokenize_command command =
  let len = String.length command in
  let buffer = Buffer.create len in
  let push_token acc =
    if Buffer.length buffer = 0 then acc
    else
      let token = Buffer.contents buffer in
      Buffer.clear buffer;
      token :: acc
  in
  let rec skip_spaces idx =
    if idx < len then
      match String.unsafe_get command idx with
      | ' ' | '\t' | '\n' | '\r' -> skip_spaces (idx + 1)
      | _ -> idx
    else idx
  in
  let rec parse idx acc quote =
    if idx >= len then
      match quote with
      | Some _ -> Error "Invalid parameters: unterminated quote in command"
      | None ->
          let tokens = List.rev (push_token acc) in
          if tokens = [] then Error "Invalid parameters: command is required" else Ok tokens
    else
      let ch = String.unsafe_get command idx in
      match quote with
      | Some q ->
          if ch = q then parse (idx + 1) acc None
          else if ch = '\\' && idx + 1 < len && q = '"' then (
            Buffer.add_char buffer (String.unsafe_get command (idx + 1));
            parse (idx + 2) acc quote
          ) else (
            Buffer.add_char buffer ch;
            parse (idx + 1) acc quote
          )
      | None ->
          begin
            match ch with
            | ' ' | '\t' | '\n' | '\r' ->
                let acc = push_token acc in
                parse (skip_spaces (idx + 1)) acc None
            | '\'' | '"' -> parse (idx + 1) acc (Some ch)
            | '\\' when idx + 1 < len ->
                Buffer.add_char buffer (String.unsafe_get command (idx + 1));
                parse (idx + 2) acc None
            | _ ->
                Buffer.add_char buffer ch;
                parse (idx + 1) acc None
          end
  in
  parse (skip_spaces 0) [] None

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

let unix_error_result ~op ~path = function
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      begin
        match op with
        | `Read | `Edit -> failure ~error_category:FileNotFound (Printf.sprintf "File not found: %s" path)
        | `Write -> failure ~error_category:DirectoryMissing (Printf.sprintf "Directory missing: %s" (Filename.dirname path))
        | `Exec -> failure ~error_category:CommandNotFound (Printf.sprintf "Command not found: %s" path)
      end
  | Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) ->
      failure ~error_category:PermissionDenied (Printf.sprintf "Permission denied: %s" path)
  | exn ->
      failure ~error_category:Other (Printexc.to_string exn)

let ensure_parent_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path)

let read_file_full path =
  try Ok (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with
  | Unix.Unix_error _ as exn -> Error exn
  | Sys_error msg when String.starts_with ~prefix:"No such file" msg ->
      Error (Unix.Unix_error (Unix.ENOENT, "open", path))
  | Sys_error msg when String.starts_with ~prefix:"Permission denied" msg ->
      Error (Unix.Unix_error (Unix.EACCES, "open", path))
  | exn -> Error exn

let write_file_atomic path content =
  try
    ensure_parent_dir path;
    let tmp_path = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
    Stdlib.Out_channel.with_open_bin tmp_path (fun ch -> Stdlib.Out_channel.output_string ch content);
    Unix.rename tmp_path path;
    Ok ()
  with
  | Unix.Unix_error _ as exn -> Error exn
  | exn -> Error exn

let find_unique_substring content needle =
  let needle_len = String.length needle in
  if needle_len = 0 then Error (failure ~error_category:InvalidParameters "Invalid parameters: old_text must not be empty")
  else
    match String.index_from_opt content 0 (String.unsafe_get needle 0) with
    | None -> Error (failure ~error_category:PatternNotFound "Pattern not found: old_text not found in file")
    | Some first_start ->
        let rec find_from idx found =
          if idx > String.length content - needle_len then found
          else
            match String.index_from_opt content idx (String.unsafe_get needle 0) with
            | None -> found
            | Some pos ->
                if pos > String.length content - needle_len then found
                else if String.sub content pos needle_len = needle then
                  begin
                    match found with
                    | None -> find_from (pos + 1) (Some pos)
                    | Some first_pos ->
                        raise (Invalid_argument (Printf.sprintf "%d:%d" first_pos pos))
                  end
                else
                  find_from (pos + 1) found
        in
        try
          match find_from first_start None with
          | Some pos -> Ok pos
          | None -> Error (failure ~error_category:PatternNotFound "Pattern not found: old_text not found in file")
        with
        | Invalid_argument data when String.contains data ':' ->
            let parts = String.split_on_char ':' data in
            let first_pos = int_of_string (List.hd parts) in
            let second_pos = int_of_string (List.hd (List.tl parts)) in
            Error (failure ~error_category:AmbiguousMatch
                     (Printf.sprintf "Ambiguous match: old_text occurs multiple times (at positions %d and %d)" first_pos second_pos))
        | exn -> Error (failure ~error_category:Other (Printexc.to_string exn))

let validate_timeout registry value =
  let _ = registry in
  if value <= 0 then
    Error "Invalid parameters: timeout_seconds must be positive"
  else
    Ok value

let run_command ~timeout_seconds ~command ~argv =
  let start_time = Unix.gettimeofday () in
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_nonblock read_fd;
  let stdin_fd = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let env = Unix.environment () in
  let argv = Array.of_list argv in
  let pid =
    try Unix.create_process_env command argv env stdin_fd write_fd write_fd
    with exn ->
      Unix.close stdin_fd;
      Unix.close read_fd;
      Unix.close write_fd;
      raise exn
  in
  Unix.close stdin_fd;
  Unix.close write_fd;
  let buffer = Bytes.create 4096 in
  let output = Buffer.create 4096 in
  let drain ready =
    if ready then
      try
        let read_bytes = Unix.read read_fd buffer 0 (Bytes.length buffer) in
        if read_bytes = 0 then false
        else (
          Buffer.add_subbytes output buffer 0 read_bytes;
          true
        )
      with
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> true
    else
      true
  in
  let rec loop status_opt =
    let elapsed = Unix.gettimeofday () -. start_time in
    if elapsed >= float_of_int timeout_seconds then `Timeout
    else
      let ready, _, _ = Unix.select [read_fd] [] [] 0.1 in
      let keep_reading = drain (ready <> []) in
      let status_opt =
        match status_opt with
        | Some _ -> status_opt
        | None ->
            match Unix.waitpid [Unix.WNOHANG] pid with
            | 0, _ -> None
            | _, status -> Some status
      in
      match status_opt, keep_reading with
      | Some status, false -> `Done status
      | _ -> loop status_opt
  in
  let result =
    match loop None with
    | `Timeout ->
        let _ = (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()) in
        let _ = (try Unix.waitpid [] pid with Unix.Unix_error _ -> (0, Unix.WSIGNALED 0)) in
        Error (Printf.sprintf "Command timed out: exceeded %d seconds" timeout_seconds)
    | `Done status ->
        let duration_ms = int_of_float ((Unix.gettimeofday () -. start_time) *. 1000.0) in
        let exit_code =
          match status with
          | Unix.WEXITED code -> code
          | Unix.WSIGNALED signal -> 128 + signal
          | Unix.WSTOPPED signal -> 128 + signal
        in
        Ok (Buffer.contents output, exit_code, duration_ms)
  in
  Unix.close read_fd;
  result

let approval_message request =
  Printf.sprintf
    "Approval required: %s %s\nApprove with: /approve %s %s"
    (approval_scope_label request.scope)
    request.target
    (approval_scope_to_string request.scope)
    request.target

let request_for_root scope path =
  let target = normalize_root_path ~scope path in
  {
    scope;
    target;
    reason = Printf.sprintf "Access to %s is not yet approved." target;
  }

let request_for_executable path =
  {
    scope = Execute;
    target = path;
    reason = "Executable access is not yet approved.";
  }

let request_for_install name =
  {
    scope = Install;
    target = name;
    reason = "Installing this skill is not yet approved.";
  }

let project_is_trusted registry =
  get_exact_match_approval registry.db ~scope:Install ~target:("__skill_trust__:" ^ registry.project_root)

let trust_project ?path registry =
  let normalized = normalize_project_path registry path in
  insert_approval registry ~scope:Install ~path:("__skill_trust__:" ^ normalized);
  Ok (Printf.sprintf "Trusted project: %s" normalized)

let is_skill_trusted registry (skill : Agent_skills.Skills.skill_metadata) =
  match skill.scope with
  | Agent_skills.Skills.Project -> project_is_trusted registry
  | Agent_skills.Skills.Builtin | Agent_skills.Skills.User -> true

let visible_skills ?(include_untrusted=false) registry =
  let skills = Agent_skills.Skills.discover_skills registry.skills in
  if include_untrusted then skills
  else List.filter (fun skill -> is_skill_trusted registry skill) skills

let skill_names registry =
  visible_skills registry |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) -> skill.name)

let build_skills_catalog registry =
  let skills = visible_skills registry in
  if skills = [] then ""
  else
    let lines =
      skills
      |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) ->
             Printf.sprintf "- %s: %s" skill.name skill.description)
    in
    "<available_skills>\n" ^ String.concat "\n" lines ^ "\n</available_skills>"

let list_skills_formatted ?(include_untrusted=false) registry =
  let skills = visible_skills ~include_untrusted registry in
  if skills = [] then "No skills available."
  else
    let lines =
      skills
      |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) ->
             let trust_note = if is_skill_trusted registry skill then "trusted" else "untrusted" in
             let scope =
               match skill.scope with
               | Agent_skills.Skills.Builtin -> "builtin"
               | Agent_skills.Skills.User -> "user"
               | Agent_skills.Skills.Project -> "project"
             in
             Printf.sprintf "- %s (%s, %s): %s" skill.name scope trust_note skill.description)
    in
    "Available skills:\n\n" ^ String.concat "\n" lines

let skill_list_tool =
  make_tool "skill_list" "List discoverable skills with scope and trust status."
    []
    (fun ~registry ~chat_id:_ _args ->
      success ~status_code:200 (list_skills_formatted ~include_untrusted:true registry))

let skill_search_tool =
  make_tool "skill_search" "Search installed skills and the official remote catalog."
    [
      ("query", `Assoc [("type", `String "string"); ("description", `String "Search text for skill name or description")]);
    ]
    (fun ~registry ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "query" with
      | Error err -> failure ~error_category:InvalidParameters err
      | Ok query ->
          let all_local : Agent_skills.Skills.skill_metadata list =
            visible_skills ~include_untrusted:true registry
          in
          let local : Agent_skills.Skills.skill_metadata list =
            List.filter
              (fun (skill : Agent_skills.Skills.skill_metadata) ->
                string_contains_ci skill.Agent_skills.Skills.name query
                || string_contains_ci skill.description query)
              all_local
          in
          match Agent_skills.Skills.search_remote_catalog registry.skills ~query with
          | Error err -> failure ~error_category:Other err
          | Ok remote ->
              let local_lines =
                local
                |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) ->
                       Printf.sprintf "- %s (installed, %s, %s): %s"
                         skill.name
                         (match skill.scope with
                          | Agent_skills.Skills.Builtin -> "builtin"
                          | Agent_skills.Skills.User -> "user"
                          | Agent_skills.Skills.Project -> "project")
                         (if is_skill_trusted registry skill then "trusted" else "untrusted")
                         skill.description)
              in
              let remote_lines =
                remote
                |> List.map (fun (skill : Agent_skills.Skills.remote_skill) ->
                       Printf.sprintf "- %s (remote %s@%s): %s"
                         skill.name
                         skill.repo
                         skill.git_ref
                         skill.description)
              in
              let blocks =
                (if local_lines = [] then [] else ["Local matches:\n" ^ String.concat "\n" local_lines])
                @ (if remote_lines = [] then [] else ["Remote matches:\n" ^ String.concat "\n" remote_lines])
              in
              if blocks = [] then success ~status_code:200 "No matching skills found."
              else success ~status_code:200 (String.concat "\n\n" blocks))

let skill_install_tool =
  make_tool "skill_install" "Install a skill from the official remote catalog. Approval is required."
    [
      ("name", `Assoc [("type", `String "string"); ("description", `String "Skill name to install")]);
    ]
    (fun ~registry ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "name" with
      | Error err -> failure ~error_category:InvalidParameters err
      | Ok name ->
          if not (approved registry ~scope:Install ~path:name) then
            let request = request_for_install name in
            approval_required request (approval_message request)
          else
            match Agent_skills.Skills.install_remote_skill registry.skills ~name with
            | Ok message -> success ~status_code:200 message
            | Error err -> failure ~error_category:Other err)

let activate_skill registry ~chat_id name =
  match List.find_opt (fun (skill : Agent_skills.Skills.skill_metadata) -> String.equal skill.name name) (visible_skills registry) with
  | None ->
      let all =
        visible_skills ~include_untrusted:true registry
        |> List.map (fun (skill : Agent_skills.Skills.skill_metadata) -> skill.name)
      in
      let hint =
        if all = [] then " No skills are currently available."
        else " Available skills: " ^ String.concat ", " all
      in
      let message = "Skill '" ^ name ^ "' not found or is not trusted." ^ hint in
      failure ~error_category:Other message
  | Some _ ->
      match Agent_skills.Skills.activate_skill registry.skills ~chat_id name with
      | Error err -> failure ~error_category:Other err
      | Ok activation ->
          let allowlist_notes =
            activation.allowed_tools
            |> List.filter_map (function
                   | Agent_skills.Skills.Allowed_read ->
                       let _ = approve_root_internal registry ~scope:Read activation.skill_dir in
                       None
                   | Agent_skills.Skills.Allowed_write ->
                       let _ = approve_root_internal registry ~scope:Write activation.skill_dir in
                       None
                   | Agent_skills.Skills.Allowed_bash command ->
                       Some (
                         match approve_executable_internal registry command with
                         | Ok _ -> Printf.sprintf "Pre-approved executable from allowed-tools: %s" command
                         | Error _ -> Printf.sprintf "Unsupported or unavailable allowed-tools executable: %s" command)
                   | Agent_skills.Skills.Allowed_other raw ->
                       Some (Printf.sprintf "Unsupported allowed-tools entry: %s" raw))
          in
          let dedupe_note =
            if activation.already_activated then
              "\n\nSkill already activated in this chat; returning existing instructions without re-approving."
            else ""
          in
          let allowlist_block =
            if allowlist_notes = [] then ""
            else "\n\n" ^ String.concat "\n" allowlist_notes
          in
          success ~status_code:200 (activation.content ^ allowlist_block ^ dedupe_note)

let activate_skill_tool registry =
  let names = skill_names registry in
  {
    definition = {
      Llm_types.name = "activate_skill";
      description = "Activate a trusted skill and load its instructions into context.";
      input_schema = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("name", `Assoc [
            ("type", `String "string");
            ("enum", `List (List.map (fun name -> `String name) names));
          ]);
        ]);
        ("required", `List [`String "name"]);
      ];
    };
    execute = (fun ~registry ~chat_id args ->
      match required_string_arg (json_assoc_or_empty args) "name" with
      | Error err -> failure ~error_category:InvalidParameters err
      | Ok name -> activate_skill registry ~chat_id name);
  }

let read_file_tool =
  make_tool "read_file" "Read the contents of a file."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to read")]);
    ]
    (fun ~registry ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "path" with
      | Error err -> failure ~error_category:InvalidParameters err
      | Ok path ->
          let normalized = normalize_lossy_path path in
          if not (approved registry ~scope:Read ~path:normalized) then
            let request = request_for_root Read path in
            approval_required request (approval_message request)
          else
            match read_file_full normalized with
            | Ok content -> success ~status_code:200 content
            | Error exn -> unix_error_result ~op:`Read ~path:normalized exn)

let write_file_tool =
  make_tool "write_file" "Write content to a file (atomic writes)."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to write")]);
      ("content", `Assoc [("type", `String "string"); ("description", `String "Content to write")]);
    ]
    (fun ~registry ~chat_id:_ args ->
      let json = json_assoc_or_empty args in
      match required_string_arg json "path", required_string_arg json "content" with
      | Error err, _ | _, Error err -> failure ~error_category:InvalidParameters err
      | Ok path, Ok content ->
          let normalized = normalize_lossy_path path in
          if not (approved registry ~scope:Write ~path:normalized) then
            let request = request_for_root Write path in
            approval_required request (approval_message request)
          else
            match write_file_atomic normalized content with
            | Ok () ->
                success ~status_code:200 (Printf.sprintf "Successfully wrote %d bytes to %s" (String.length content) normalized)
            | Error exn -> unix_error_result ~op:`Write ~path:normalized exn)

let edit_file_tool =
  make_tool "edit_file" "Edit a file by replacing one exact text block. The old_text must be unique in the file."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to edit")]);
      ("old_text", `Assoc [("type", `String "string"); ("description", `String "Exact text to find and replace (must be unique in file)")]);
      ("new_text", `Assoc [("type", `String "string"); ("description", `String "New text to replace with")]);
    ]
    (fun ~registry ~chat_id:_ args ->
      let json = json_assoc_or_empty args in
      match required_string_arg json "path", required_string_arg json "old_text", required_string_arg json "new_text" with
      | Error err, _, _ | _, Error err, _ | _, _, Error err -> failure ~error_category:InvalidParameters err
      | Ok path, Ok old_text, Ok new_text ->
          let normalized = normalize_lossy_path path in
          if not (approved registry ~scope:Write ~path:normalized) then
            let request = request_for_root Write path in
            approval_required request (approval_message request)
          else
            match read_file_full normalized with
            | Error exn -> unix_error_result ~op:`Edit ~path:normalized exn
            | Ok content ->
                begin
                  match find_unique_substring content old_text with
                  | Error result -> result
                  | Ok idx ->
                      let old_len = String.length old_text in
                      let prefix = String.sub content 0 idx in
                      let suffix = String.sub content (idx + old_len) (String.length content - idx - old_len) in
                      let new_content = prefix ^ new_text ^ suffix in
                      match write_file_atomic normalized new_content with
                      | Ok () -> success ~status_code:200 "File edited successfully"
                      | Error exn -> unix_error_result ~op:`Write ~path:normalized exn
                end)

let bash_tool =
  {
    definition = {
      Llm_types.name = "bash";
      description = "Execute a command with explicit executable approval.";
      input_schema = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("command", `Assoc [("type", `String "string")]);
          ("timeout_seconds", `Assoc [("type", `String "integer")]);
        ]);
        ("required", `List [`String "command"]);
      ];
    };
    execute = (fun ~registry ~chat_id:_ args ->
      let json = json_assoc_or_empty args in
      match required_string_arg json "command", optional_int_arg json "timeout_seconds" with
      | Error err, _ | _, Error err -> failure ~error_category:InvalidParameters err
      | Ok command, Ok timeout_arg ->
          begin
            match tokenize_command command with
            | Error err -> failure ~error_category:InvalidParameters err
            | Ok argv ->
                let timeout_value = Option.value timeout_arg ~default:default_bash_timeout_seconds in
                begin
                  match validate_timeout registry timeout_value with
                  | Error err -> failure ~error_category:InvalidParameters err
                  | Ok timeout_seconds ->
                      begin
                        match resolve_executable_path (List.hd argv) with
                        | Error err -> failure ~error_category:CommandNotFound err
                        | Ok executable ->
                            if not (approved registry ~scope:Execute ~path:executable) then
                              let request = request_for_executable executable in
                              approval_required request (approval_message request)
                            else
                              begin
                                match run_command ~timeout_seconds ~command:executable ~argv:(executable :: List.tl argv) with
                                | Error err ->
                                    let category =
                                      if classify_error ~error_message:err = CommandTimeout then CommandTimeout else CommandFailed
                                    in
                                    failure ~error_category:category err
                                | Ok (output, exit_code, duration_ms) ->
                                    let content = Printf.sprintf "Command output:\n%s\nExit status: %d" output exit_code in
                                    if exit_code = 0 then
                                      success ~status_code:exit_code ~duration_ms content
                                    else
                                      failure ~status_code:exit_code ~duration_ms ~error_type:"command_failed" ~error_category:CommandFailed
                                        (Printf.sprintf "Command failed: %s" content)
                              end
                      end
                end
          end);
  }

let create_default_registry ~db_path ~project_root ~skills () =
  let db = Sqlite3.db_open db_path in
  init_db db;
  let pool = Domainslib.Task.setup_pool ~num_domains:2 () in
  {
    db;
    pool;
    project_root = normalize_root_path ~scope:Read project_root;
    skills;
    tools = [bash_tool; read_file_tool; write_file_tool; edit_file_tool; skill_list_tool; skill_search_tool; skill_install_tool];
  }

let definitions registry =
  let defs = List.map (fun t -> t.definition) registry.tools in
  let skill_names = skill_names registry in
  if skill_names = [] then defs else defs @ [ (activate_skill_tool registry).definition ]

let execute registry ~chat_id name input =
  let tool_opt =
    if String.equal name "activate_skill" then Some (activate_skill_tool registry)
    else List.find_opt (fun tool -> tool.definition.Llm_types.name = name) registry.tools
  in
  match tool_opt with
  | None -> failure ("Tool not found: " ^ name)
  | Some tool ->
      begin
        try tool.execute ~registry ~chat_id (json_assoc_or_empty input)
        with
        | Unix.Unix_error _ as exn -> unix_error_result ~op:`Exec ~path:name exn
        | exn -> failure (Printf.sprintf "Tool execution exception: %s" (Printexc.to_string exn))
      end

let approve_executable registry executable =
  match approve_executable_internal registry executable with
  | Error err -> Error err
  | Ok resolved -> Ok (Printf.sprintf "Approved executable: %s" resolved)

let approve_root registry ~scope path =
  let scope = scope_for_root_approval scope in
  let normalized = approve_root_internal registry ~scope path in
  Ok (Printf.sprintf "Approved %s: %s" (approval_scope_label scope) normalized)

let approve_install registry name =
  approve_install_internal registry name;
  Ok (Printf.sprintf "Approved %s: %s" (approval_scope_label Install) name)

let list_approvals_formatted ?scope ?(include_project_root=true) registry =
  let scopes =
    match scope with
    | Some scope -> [ scope ]
    | None -> [ Execute; Read; Write; Install ]
  in
  scopes
  |> List.map (fun scope -> render_approval_section registry ~scope ~include_project_root)
  |> String.concat "\n\n"
