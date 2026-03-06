(** Tools module for OClaw - filesystem, shell, task, and safety guards. *)

open Yojson.Safe
open Yojson.Safe.Util

type sandbox_config = {
  workspace_root : string;
  restrict_to_workspace : bool;
  allow_read_paths : string list;
  allow_write_paths : string list;
  exec_timeout_seconds : int;
  exec_enable_deny_patterns : bool;
  exec_custom_deny_patterns : string list;
  exec_custom_allow_patterns : string list;
}

let default_sandbox_config = {
  workspace_root = ".";
  restrict_to_workspace = true;
  allow_read_paths = [];
  allow_write_paths = [];
  exec_timeout_seconds = 60;
  exec_enable_deny_patterns = true;
  exec_custom_deny_patterns = [];
  exec_custom_allow_patterns = [];
}

let active_sandbox_config = ref default_sandbox_config

let set_sandbox_config cfg =
  active_sandbox_config := cfg

(* Tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Safe.t) list;
  execute : Yojson.Safe.t -> string;
}

let trim = String.trim

let string_contains ~haystack ~needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then true
  else if n_len > h_len then false
  else
    let rec loop i =
      if i > h_len - n_len then false
      else if String.sub haystack i n_len = needle then true
      else loop (i + 1)
    in
    loop 0

let ensure_trailing_slash p =
  if p = "/" then "/"
  else if String.ends_with ~suffix:"/" p then p
  else p ^ "/"

let normalize_lexical path =
  let is_abs = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let stack = ref [] in
  List.iter (fun part ->
    match part with
    | "" | "." -> ()
    | ".." ->
        (match !stack with
         | _ :: tl -> stack := tl
         | [] -> ())
    | p -> stack := p :: !stack
  ) parts;
  let body = String.concat "/" (List.rev !stack) in
  if is_abs then
    if body = "" then "/" else "/" ^ body
  else
    if body = "" then "." else body

let absolute_path_from base path =
  let raw =
    if Filename.is_relative path then Filename.concat base path
    else path
  in
  normalize_lexical raw

let is_within ~root ~path =
  path = root || String.starts_with ~prefix:(ensure_trailing_slash root) path

let path_exists path =
  try
    ignore (Unix.lstat path);
    true
  with _ -> false

let rec find_existing_ancestor path =
  if path_exists path then path
  else
    let parent = Filename.dirname path in
    if parent = path then path
    else find_existing_ancestor parent

let workspace_root () =
  let cfg = !active_sandbox_config in
  let cwd = Sys.getcwd () in
  let abs = absolute_path_from cwd cfg.workspace_root in
  try Unix.realpath abs with _ -> abs

let path_matches_allowlist path allowlist =
  let root = workspace_root () in
  List.exists (fun allowed ->
    let abs_allowed = absolute_path_from root allowed in
    is_within ~root:abs_allowed ~path
  ) allowlist

let validate_path ~for_write ~allowlist path =
  let cfg = !active_sandbox_config in
  let root = workspace_root () in
  let root_real = try Unix.realpath root with _ -> root in

  if trim path = "" then Error "path is required"
  else
    let candidate = absolute_path_from root path in
    if path_matches_allowlist candidate allowlist then Ok candidate
    else if not cfg.restrict_to_workspace then Ok candidate
    else if not (is_within ~root ~path:candidate) then
      Error "path is outside workspace"
    else (
      let anchor =
        if for_write then find_existing_ancestor (Filename.dirname candidate)
        else find_existing_ancestor candidate
      in
      let anchor_real = try Unix.realpath anchor with _ -> anchor in
      if not (is_within ~root:root_real ~path:anchor_real) then
        Error "path resolves outside workspace"
      else
        Ok candidate
    )

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then ()
  else (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  )

let read_file_full path =
  try
    let ch = open_in_bin path in
    try
      let len = in_channel_length ch in
      let content = really_input_string ch len in
      close_in ch;
      Ok content
    with exn ->
      close_in_noerr ch;
      Error (Printexc.to_string exn)
  with exn ->
    Error (Printexc.to_string exn)

let write_file_atomic path content =
  let dir = Filename.dirname path in
  mkdir_p dir;
  let tmp =
    Filename.concat dir
      (Printf.sprintf ".tmp-oclaw-%d-%06d" (int_of_float (Unix.gettimeofday ())) (Random.int 1000000))
  in
  let ch = open_out_bin tmp in
  try
    output_string ch content;
    close_out ch;
    Unix.rename tmp path;
    Ok ()
  with exn ->
    close_out_noerr ch;
    (try Sys.remove tmp with _ -> ());
    Error (Printexc.to_string exn)

let split_command command =
  let len = String.length command in
  let buf = Buffer.create len in
  let tokens = ref [] in
  let mode = ref `Normal in
  let flush () =
    if Buffer.length buf > 0 then (
      tokens := Buffer.contents buf :: !tokens;
      Buffer.clear buf
    )
  in
  let rec loop i =
    if i >= len then (
      (match !mode with
       | `Normal -> ()
       | _ -> raise (Invalid_argument "Unclosed quote in command"));
      flush ();
      List.rev !tokens
    ) else
      let c = command.[i] in
      match !mode with
      | `Normal ->
          if c = ' ' || c = '\t' || c = '\n' then (
            flush ();
            loop (i + 1)
          ) else if c = '\'' then (
            mode := `Single;
            loop (i + 1)
          ) else if c = '"' then (
            mode := `Double;
            loop (i + 1)
          ) else (
            Buffer.add_char buf c;
            loop (i + 1)
          )
      | `Single ->
          if c = '\'' then (
            mode := `Normal;
            loop (i + 1)
          ) else (
            Buffer.add_char buf c;
            loop (i + 1)
          )
      | `Double ->
          if c = '"' then (
            mode := `Normal;
            loop (i + 1)
          ) else (
            Buffer.add_char buf c;
            loop (i + 1)
          )
  in
  loop 0

let truncate_output ?(limit=10000) output =
  if String.length output <= limit then output
  else
    let remaining = String.length output - limit in
    String.sub output 0 limit ^ Printf.sprintf "\n... (truncated, %d more chars)" remaining

let default_exec_deny_patterns = [
  "rm -rf";
  "rm -fr";
  " mkfs";
  " format ";
  "dd if=";
  "shutdown";
  "reboot";
  "poweroff";
  " sudo ";
  "curl ";
  "wget ";
  "| sh";
  "| bash";
  " chmod 777";
  " chown ";
  " kill -9";
  " killall ";
  " git push";
]

let safe_device_paths = [
  "/dev/null";
  "/dev/zero";
  "/dev/random";
  "/dev/urandom";
  "/dev/stdin";
  "/dev/stdout";
  "/dev/stderr";
]

let normalize_exec_token token =
  let t = trim token in
  let len = String.length t in
  if len >= 2 && ((t.[0] = '\'' && t.[len - 1] = '\'') || (t.[0] = '"' && t.[len - 1] = '"')) then
    String.sub t 1 (len - 2)
  else
    t

let guard_command command =
  let cfg = !active_sandbox_config in
  let lower = String.lowercase_ascii command in
  let explicitly_allowed =
    cfg.exec_custom_allow_patterns <> []
    && List.exists (fun p -> string_contains ~haystack:lower ~needle:(String.lowercase_ascii p)) cfg.exec_custom_allow_patterns
  in

  if cfg.exec_enable_deny_patterns && not explicitly_allowed then (
    let deny = default_exec_deny_patterns @ cfg.exec_custom_deny_patterns in
    if List.exists (fun p -> string_contains ~haystack:lower ~needle:(String.lowercase_ascii p)) deny then
      Error "Command blocked by safety guard (dangerous pattern detected)"
    else if cfg.restrict_to_workspace && (string_contains ~haystack:command ~needle:"../" || string_contains ~haystack:command ~needle:"..\\") then
      Error "Command blocked by safety guard (path traversal detected)"
    else
      let root = workspace_root () in
      let rec check_tokens = function
        | [] -> Ok ()
        | token :: rest ->
            let t = normalize_exec_token token in
            if t = "" then check_tokens rest
            else if List.mem t safe_device_paths then check_tokens rest
            else if String.length t > 0 && t.[0] = '/' then
              let p = absolute_path_from root t in
              if cfg.restrict_to_workspace && not (is_within ~root ~path:p) then
                Error "Command blocked by safety guard (path outside workspace)"
              else
                check_tokens rest
            else
              check_tokens rest
      in
      (match split_command command with
       | tokens -> check_tokens tokens
       | exception Invalid_argument msg -> Error msg)
  ) else Ok ()

let json_assoc_or_empty = function
  | `Assoc _ as json -> json
  | `Null -> `Assoc []
  | `String s ->
      (try
         match Yojson.Safe.from_string s with
         | `Assoc _ as json -> json
         | other -> `Assoc [ ("value", other) ]
       with _ -> `Assoc [ ("value", `String s) ])
  | _ -> `Assoc []

let string_arg ?(default=None) json name =
  match member name json with
  | `String s -> Some s
  | `Int i -> Some (string_of_int i)
  | `Intlit s -> Some s
  | `Float f -> Some (string_of_float f)
  | `Bool b -> Some (string_of_bool b)
  | `Null -> default
  | _ -> default

let int_arg ?(default=None) json name =
  match member name json with
  | `Int i -> Some i
  | `Intlit s -> (try Some (int_of_string s) with _ -> default)
  | `Float f -> Some (int_of_float f)
  | `String s -> (try Some (int_of_string (trim s)) with _ -> default)
  | _ -> default

let bool_arg ?(default=None) json name =
  match member name json with
  | `Bool b -> Some b
  | `String s ->
      let v = String.lowercase_ascii (trim s) in
      if v = "true" || v = "1" || v = "yes" then Some true
      else if v = "false" || v = "0" || v = "no" then Some false
      else default
  | _ -> default

let required_string_arg json name =
  match string_arg json name with
  | Some v when trim v <> "" -> Ok v
  | _ -> Error (Printf.sprintf "%s is required" name)

let file_read_tool = {
  name = "read_file";
  description = "Read and return the complete contents of a file from the filesystem";
  parameters = [
    "path", `String "string: The absolute or relative path to the file to read"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path" with
    | Error e -> e
    | Ok path ->
        (match validate_path ~for_write:false ~allowlist:(!active_sandbox_config).allow_read_paths path with
         | Error e -> Printf.sprintf "Error reading file %s: %s" path e
         | Ok safe_path ->
             match read_file_full safe_path with
             | Ok content -> content
             | Error e -> Printf.sprintf "Error reading file %s: %s" path e)
  );
}

let write_file_tool = {
  name = "write_file";
  description = "Write content to a file";
  parameters = [
    "path", `String "string: The file path to write";
    "content", `String "string: The content to write"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "content" with
    | Error e, _ | _, Error e -> e
    | Ok path, Ok content ->
        (match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
         | Error e -> Printf.sprintf "Error writing file %s: %s" path e
         | Ok safe_path ->
             (match write_file_atomic safe_path content with
              | Ok () -> Printf.sprintf "File written: %s" path
              | Error e -> Printf.sprintf "Error writing file %s: %s" path e))
  );
}

let edit_file_tool = {
  name = "edit_file";
  description = "Edit a file by replacing old_text with new_text. old_text must exist exactly once.";
  parameters = [
    "path", `String "string: The file path to edit";
    "old_text", `String "string: Exact text to replace";
    "new_text", `String "string: Replacement text"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "old_text", required_string_arg args "new_text" with
    | Error e, _, _ | _, Error e, _ | _, _, Error e -> e
    | Ok path, Ok old_text, Ok new_text ->
        (match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
         | Error e -> Printf.sprintf "Error editing file %s: %s" path e
         | Ok safe_path ->
             (match read_file_full safe_path with
              | Error e -> Printf.sprintf "Error editing file %s: %s" path e
              | Ok content ->
                  let count =
                    let rec count_from idx acc =
                      if idx >= String.length content then acc
                      else if idx + String.length old_text <= String.length content
                           && String.sub content idx (String.length old_text) = old_text
                      then count_from (idx + 1) (acc + 1)
                      else count_from (idx + 1) acc
                    in
                    if old_text = "" then 0 else count_from 0 0
                  in
                  if count = 0 then "Error editing file: old_text not found"
                  else if count > 1 then Printf.sprintf "Error editing file: old_text appears %d times" count
                  else
                    let replaced =
                      let idx =
                        let rec find_from i =
                          if i + String.length old_text > String.length content then -1
                          else if String.sub content i (String.length old_text) = old_text then i
                          else find_from (i + 1)
                        in
                        find_from 0
                      in
                      if idx < 0 then content
                      else
                        String.sub content 0 idx
                        ^ new_text
                        ^ String.sub content (idx + String.length old_text)
                            (String.length content - idx - String.length old_text)
                    in
                    (match write_file_atomic safe_path replaced with
                     | Ok () -> Printf.sprintf "File edited: %s" path
                     | Error e -> Printf.sprintf "Error editing file %s: %s" path e)))
  );
}

let append_file_tool = {
  name = "append_file";
  description = "Append content to the end of a file";
  parameters = [
    "path", `String "string: The file path to append";
    "content", `String "string: The content to append"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "content" with
    | Error e, _ | _, Error e -> e
    | Ok path, Ok content_to_append ->
        (match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
         | Error e -> Printf.sprintf "Error appending file %s: %s" path e
         | Ok safe_path ->
             let existing =
               match read_file_full safe_path with
               | Ok s -> s
               | Error _ -> ""
             in
             (match write_file_atomic safe_path (existing ^ content_to_append) with
              | Ok () -> Printf.sprintf "Appended to file: %s" path
              | Error e -> Printf.sprintf "Error appending file %s: %s" path e))
  );
}

let list_directory_core path =
  match validate_path ~for_write:false ~allowlist:(!active_sandbox_config).allow_read_paths path with
  | Error e -> Printf.sprintf "Error listing directory %s: %s" path e
  | Ok safe_path ->
      try
        if not (Sys.file_exists safe_path) then
          Printf.sprintf "Error listing directory %s: path does not exist" path
        else if not (Sys.is_directory safe_path) then
          Printf.sprintf "Error listing directory %s: path is not a directory" path
        else
          let entries = Sys.readdir safe_path |> Array.to_list |> List.sort String.compare in
          let lines = List.map (fun name ->
            let full = Filename.concat safe_path name in
            let kind =
              try if Sys.is_directory full then "DIR" else "FILE"
              with _ -> "FILE"
            in
            Printf.sprintf "%s: %s" kind name
          ) entries in
          Printf.sprintf "Directory listing for %s:\n%s"
            (if path = "" || path = "." then "current directory" else path)
            (String.concat "\n" lines)
      with exn ->
        Printf.sprintf "Error listing directory %s: %s" path (Printexc.to_string exn)

let list_directory_tool = {
  name = "list_directory";
  description = "List files and directories in the current or specified path";
  parameters = [
    "path", `String "string: The directory path to list (default: current directory)"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    let path = Option.value ~default:"." (string_arg args "path") in
    list_directory_core path
  );
}

let list_dir_tool =
  { list_directory_tool with
    name = "list_dir";
    description = "List files and directories in a path";
  }

let has_timeout_binary =
  lazy (Sys.command "command -v timeout >/dev/null 2>&1" = 0)

let run_command ~timeout_seconds command =
  let command = trim command in
  if command = "" then Error "command is required"
  else
    let shell_command = command ^ " 2>&1" in
    let launcher, argv =
      if timeout_seconds > 0 && Lazy.force has_timeout_binary then
        ("timeout", [| "timeout"; string_of_int timeout_seconds; "sh"; "-lc"; shell_command |])
      else
        ("sh", [| "sh"; "-lc"; shell_command |])
    in
    try
      let ch = Unix.open_process_args_in launcher argv in
      let output = Buffer.create 1024 in
      (try
         while true do
           let line = input_line ch in
           Buffer.add_string output line;
           Buffer.add_char output '\n'
         done
       with End_of_file -> ());
      let status = Unix.close_process_in ch in
      let exit_code =
        match status with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> -signal
        | Unix.WSTOPPED signal -> -signal
      in
      Ok (Buffer.contents output, exit_code)
    with exn -> Error (Printexc.to_string exn)

let shell_tool = {
  name = "execute_command";
  description = "Execute a command and return the output";
  parameters = [
    "command", `String "string: The command to execute"
  ];
  execute = (fun args ->
    let cfg = !active_sandbox_config in
    let args = json_assoc_or_empty args in
    match required_string_arg args "command" with
    | Error e -> e
    | Ok command ->
        (match guard_command command with
         | Error e -> e
         | Ok () ->
             match run_command ~timeout_seconds:cfg.exec_timeout_seconds command with
             | Error e -> Printf.sprintf "Error executing command: %s" e
             | Ok (output, exit_code) ->
                 let output = truncate_output output in
                 Printf.sprintf "Command output:\n%s\nExit status: %d" output exit_code)
  );
}

let exec_tool = {
  shell_tool with
  name = "exec";
  description = "Execute a shell command and return its output. Use with caution.";
}

let task_api_result_to_string = function
  | Ok json -> Yojson.Safe.pretty_to_string json
  | Error err -> "Error: " ^ err

let task_create_tool = {
  name = "task_create";
  description = "Create a task in the central OClaw task API";
  parameters = [
    "title", `String "string: Task title";
    "description", `String "string: Optional task description";
    "kind", `String "string: Optional kind (general|subagent)";
    "status", `String "string: Optional status";
    "priority", `String "integer: Optional priority (0-4)";
    "assignee", `String "string: Optional assignee";
    "parent_id", `String "string: Optional parent task ID";
    "session_id", `String "string: Optional session ID"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "title" with
    | Error e -> e
    | Ok title ->
        let fields =
          [
            ("title", `String title);
          ]
          @ (match string_arg args "description" with Some s -> [("description", `String s)] | None -> [])
          @ (match string_arg args "kind" with Some s -> [("kind", `String s)] | None -> [])
          @ (match string_arg args "status" with Some s -> [("status", `String s)] | None -> [])
          @ (match int_arg args "priority" with Some i -> [("priority", `Int i)] | None -> [])
          @ (match string_arg args "assignee" with Some s -> [("assignee", `String s)] | None -> [])
          @ (match string_arg args "parent_id" with Some s -> [("parent_id", `String s)] | None -> [])
          @ (match string_arg args "session_id" with Some s -> [("session_id", `String s)] | None -> [])
          @ [("actor", `String "tool")]
        in
        task_api_result_to_string (Task_api_client.create_task (`Assoc fields))
  );
}

let task_list_tool = {
  name = "task_list";
  description = "List tasks from the central OClaw task API";
  parameters = [
    "status", `String "string: Optional status filter";
    "assignee", `String "string: Optional assignee filter";
    "kind", `String "string: Optional kind filter";
    "limit", `String "integer: Optional max results";
    "cursor", `String "float: Optional pagination cursor (created_at)"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    let cursor =
      match string_arg args "cursor" with
      | Some s -> (try Some (float_of_string (trim s)) with _ -> None)
      | None -> None
    in
    task_api_result_to_string
      (Task_api_client.list_tasks
         ?status:(string_arg args "status")
         ?assignee:(string_arg args "assignee")
         ?kind:(string_arg args "kind")
         ?limit:(int_arg args "limit")
         ?cursor
         ())
  );
}

let task_show_tool = {
  name = "task_show";
  description = "Show one task by ID";
  parameters = [
    "id", `String "string: Task ID"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id -> task_api_result_to_string (Task_api_client.get_task id)
  );
}

let task_update_tool = {
  name = "task_update";
  description = "Patch fields for an existing task";
  parameters = [
    "id", `String "string: Task ID";
    "title", `String "string: Optional title";
    "description", `String "string: Optional description";
    "status", `String "string: Optional status";
    "priority", `String "integer: Optional priority";
    "assignee", `String "string: Optional assignee";
    "close_reason", `String "string: Optional close reason";
    "result", `String "string: Optional result";
    "error", `String "string: Optional error"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id ->
        let fields =
          []
          @ (match string_arg args "title" with Some s -> [("title", `String s)] | None -> [])
          @ (match string_arg args "description" with Some s -> [("description", `String s)] | None -> [])
          @ (match string_arg args "status" with Some s -> [("status", `String s)] | None -> [])
          @ (match int_arg args "priority" with Some i -> [("priority", `Int i)] | None -> [])
          @ (match string_arg args "assignee" with Some s -> [("assignee", `String s)] | None -> [])
          @ (match string_arg args "close_reason" with Some s -> [("close_reason", `String s)] | None -> [])
          @ (match string_arg args "result" with Some s -> [("result", `String s)] | None -> [])
          @ (match string_arg args "error" with Some s -> [("error", `String s)] | None -> [])
          @ [("actor", `String "tool")]
        in
        task_api_result_to_string (Task_api_client.update_task id (`Assoc fields))
  );
}

let task_ready_tool = {
  name = "task_ready";
  description = "List dependency-unblocked ready tasks";
  parameters = [
    "limit", `String "integer: Optional max results"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    task_api_result_to_string
      (Task_api_client.ready_tasks ?limit:(int_arg args "limit") ())
  );
}

let task_claim_tool = {
  name = "task_claim";
  description = "Claim a task and move it to in_progress";
  parameters = [
    "id", `String "string: Task ID";
    "assignee", `String "string: Optional assignee"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id ->
        task_api_result_to_string
          (Task_api_client.claim_task id ?assignee:(string_arg args "assignee") ~actor:"tool" ())
  );
}

let task_close_tool = {
  name = "task_close";
  description = "Close a task";
  parameters = [
    "id", `String "string: Task ID";
    "close_reason", `String "string: Optional close reason"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id ->
        task_api_result_to_string
          (Task_api_client.close_task id ?reason:(string_arg args "close_reason") ~actor:"tool" ())
  );
}

let task_cancel_tool = {
  name = "task_cancel";
  description = "Cancel a task";
  parameters = [
    "id", `String "string: Task ID"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id ->
        task_api_result_to_string
          (Task_api_client.cancel_task id ~actor:"tool" ())
  );
}

let task_dep_add_tool = {
  name = "task_dep_add";
  description = "Add a dependency to task: from_task_id blocks/relates to id";
  parameters = [
    "id", `String "string: Target task ID";
    "from_task_id", `String "string: Upstream task ID";
    "dep_type", `String "string: blocks|related|parent_child|discovered_from"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id", required_string_arg args "from_task_id" with
    | Error e, _ | _, Error e -> e
    | Ok id, Ok from_task_id ->
        task_api_result_to_string
          (Task_api_client.add_dependency id ~from_task_id ?dep_type:(string_arg args "dep_type") ~actor:"tool" ())
  );
}

let task_dep_list_tool = {
  name = "task_dep_list";
  description = "List dependencies for a task";
  parameters = [
    "id", `String "string: Task ID"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id -> task_api_result_to_string (Task_api_client.list_dependencies id)
  );
}

let task_events_tool = {
  name = "task_events";
  description = "List task lifecycle events";
  parameters = [
    "id", `String "string: Task ID";
    "after_seq", `String "integer: Optional event cursor";
    "limit", `String "integer: Optional max events"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "id" with
    | Error e -> e
    | Ok id ->
        task_api_result_to_string
          (Task_api_client.list_events id ?after_seq:(int_arg args "after_seq") ?limit:(int_arg args "limit") ())
  );
}

let spawn_tool = {
  name = "spawn";
  description = "Spawn a background subagent to handle a task asynchronously";
  parameters = [
    "task", `String "string: Task description for the subagent"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "task" with
    | Error e -> e
    | Ok _ ->
        "Spawn request accepted. (The agent runtime handles execution and tracking.)"
  );
}

let subagent_list_tool = {
  name = "subagent_list";
  description = "List background subagent tasks";
  parameters = [];
  execute = (fun _ ->
    "Subagent list is provided by the agent runtime."
  );
}

let subagent_manage_tool = {
  name = "subagent_manage";
  description = "Manage a subagent task: status or kill";
  parameters = [
    "action", `String "string: status | kill";
    "id", `String "string: Subagent task ID"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "action", required_string_arg args "id" with
    | Error e, _ | _, Error e -> e
    | Ok _, Ok _ ->
        "Subagent management is provided by the agent runtime."
  );
}

(* Tool registry *)
let tool_registry = ref []

let register_tool tool =
  tool_registry := tool :: !tool_registry

let get_tool name =
  List.find_opt (fun tool -> tool.name = name) !tool_registry

let get_all_tools () =
  !tool_registry
  |> List.rev
  |> List.map (fun tool -> (tool.name, tool.description))

let execute_tool tool_name arguments =
  match get_tool tool_name with
  | Some tool ->
      let normalized = json_assoc_or_empty arguments in
      (try tool.execute normalized
       with exn -> Printf.sprintf "Error executing tool %s: %s" tool_name (Printexc.to_string exn))
  | None ->
      Printf.sprintf "Tool %s not found" tool_name

let parse_parameter_schema = function
  | `String desc ->
      let parts = String.split_on_char ':' desc in
      (match parts with
       | ty :: rest when rest <> [] ->
           let param_type = trim ty in
           let param_desc = trim (String.concat ":" rest) in
           (if param_type = "" then "string" else param_type), param_desc
       | _ -> "string", trim desc)
  | _ -> "string", ""

let init_default_tools ?sandbox_config () =
  Random.self_init ();
  Option.iter set_sandbox_config sandbox_config;
  tool_registry := [];
  register_tool file_read_tool;
  register_tool write_file_tool;
  register_tool edit_file_tool;
  register_tool append_file_tool;
  register_tool shell_tool;
  register_tool exec_tool;
  register_tool list_directory_tool;
  register_tool list_dir_tool;
  register_tool task_create_tool;
  register_tool task_list_tool;
  register_tool task_show_tool;
  register_tool task_update_tool;
  register_tool task_ready_tool;
  register_tool task_claim_tool;
  register_tool task_close_tool;
  register_tool task_cancel_tool;
  register_tool task_dep_add_tool;
  register_tool task_dep_list_tool;
  register_tool task_events_tool;
  register_tool spawn_tool;
  register_tool subagent_list_tool;
  register_tool subagent_manage_tool

let tools_to_json () =
  let tool_list =
    !tool_registry
    |> List.rev
    |> List.map (fun tool ->
      let properties =
        tool.parameters
        |> List.map (fun (name, schema) ->
             let param_type, param_desc = parse_parameter_schema schema in
             (name, `Assoc [
               ("type", `String param_type);
               ("description", `String param_desc)
             ]))
      in
      let required = tool.parameters |> List.map (fun (name, _) -> `String name) in
      `Assoc [
        ("type", `String "function");
        ("function", `Assoc [
          ("name", `String tool.name);
          ("description", `String tool.description);
          ("parameters", `Assoc [
            ("type", `String "object");
            ("properties", `Assoc properties);
            ("required", `List required)
          ])
        ])
      ])
  in
  `List tool_list

let parse_tool_calls json =
  try
    let choices = member "choices" json |> to_list in
    match choices with
    | first_choice :: _ ->
        (try
           let message = member "message" first_choice in
           let tool_calls = member "tool_calls" message |> to_list in
           Some tool_calls
         with _ -> None)
    | [] -> None
  with _ -> None

let extract_tool_arguments tool_call =
  try
    let function_ = member "function" tool_call in
    let arguments_json = member "arguments" function_ in
    Some (to_string arguments_json)
  with _ -> None
