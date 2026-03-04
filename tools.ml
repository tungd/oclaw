(** Tools module for OClaw - web search, file operations, and safety guards. *)

open Yojson.Safe
open Yojson.Safe.Util

module Http = Http_client

type sandbox_config = {
  workspace_root : string;
  restrict_to_workspace : bool;
  allow_read_paths : string list;
  allow_write_paths : string list;
  exec_timeout_seconds : int;
  exec_enable_deny_patterns : bool;
  exec_custom_deny_patterns : string list;
  exec_custom_allow_patterns : string list;
  web_fetch_max_chars : int;
  web_fetch_max_bytes : int;
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
  web_fetch_max_chars = 50000;
  web_fetch_max_bytes = 10 * 1024 * 1024;
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

let html_to_text html =
  let buf = Buffer.create (String.length html) in
  let in_tag = ref false in
  String.iter (fun c ->
    if c = '<' then in_tag := true
    else if c = '>' then in_tag := false
    else if not !in_tag then Buffer.add_char buf c
  ) html;
  let raw = Buffer.contents buf in
  let compact = Buffer.create (String.length raw) in
  let prev_space = ref false in
  String.iter (fun c ->
    let is_space = c = ' ' || c = '\n' || c = '\r' || c = '\t' in
    if is_space then (
      if not !prev_space then Buffer.add_char compact ' ';
      prev_space := true
    ) else (
      Buffer.add_char compact c;
      prev_space := false
    )
  ) raw;
  trim (Buffer.contents compact)

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

(* Web search tool - intentionally kept as mock per user request. *)
let web_search_tool = {
  name = "web_search";
  description = "Search the web for current information on any topic";
  parameters = [
    "query", `String "string: The search query to look up"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    let query = Option.value ~default:"" (string_arg args "query") in
    Printf.sprintf
      "Search results for: %s\n\n[Mock results - would call real API in production]\n1. Result 1\n2. Result 2\n3. Result 3"
      query
  );
}

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
  match split_command command with
  | [] -> Error "command is required"
  | prog :: argv_tail ->
      let argv =
        if timeout_seconds > 0 && Lazy.force has_timeout_binary then
          Array.of_list ("timeout" :: string_of_int timeout_seconds :: prog :: argv_tail)
        else
          Array.of_list (prog :: argv_tail)
      in
      let launcher =
        if timeout_seconds > 0 && Lazy.force has_timeout_binary then "timeout" else prog
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

let web_fetch_tool = {
  name = "web_fetch";
  description = "Fetch a URL and return extracted text content";
  parameters = [
    "url", `String "string: URL to fetch (http/https)";
    "max_chars", `String "integer: Optional maximum output characters"
  ];
  execute = (fun args ->
    let cfg = !active_sandbox_config in
    let args = json_assoc_or_empty args in
    match required_string_arg args "url" with
    | Error e -> e
    | Ok url ->
        let lower = String.lowercase_ascii (trim url) in
        if not (String.starts_with ~prefix:"http://" lower || String.starts_with ~prefix:"https://" lower) then
          "Error fetching URL: only http/https URLs are allowed"
        else
          let max_chars =
            match int_arg args "max_chars" with
            | Some n when n > 100 -> n
            | _ -> cfg.web_fetch_max_chars
          in
          let response = Http.get url [ ("User-Agent", "OClaw/1.0") ] 30 in
          (match response.Http.HttpResponse.error with
           | Some e -> Printf.sprintf "Error fetching URL: %s" e
           | None ->
               if response.Http.HttpResponse.status < 200 || response.Http.HttpResponse.status >= 300 then
                 Printf.sprintf "Error fetching URL: HTTP %d" response.Http.HttpResponse.status
               else if String.length response.Http.HttpResponse.body > cfg.web_fetch_max_bytes then
                 Printf.sprintf "Error fetching URL: response exceeds %d bytes" cfg.web_fetch_max_bytes
               else
                 let content_type =
                   response.Http.HttpResponse.headers
                   |> List.find_map (fun (k, v) ->
                        if String.lowercase_ascii (trim k) = "content-type" then Some v else None)
                   |> Option.value ~default:""
                 in
                let text =
                  if string_contains ~haystack:(String.lowercase_ascii content_type) ~needle:"text/html" then
                    html_to_text response.Http.HttpResponse.body
                  else
                    response.Http.HttpResponse.body
                in
                let was_truncated = String.length text > max_chars in
                let text =
                  if was_truncated then String.sub text 0 max_chars else text
                in
                let result =
                  `Assoc [
                    ("url", `String url);
                    ("status", `Int response.Http.HttpResponse.status);
                    ("truncated", `Bool was_truncated);
                    ("length", `Int (String.length text));
                    ("text", `String text)
                  ]
                in
                 Yojson.Safe.pretty_to_string result)
  );
}

let discover_skill_dirs () =
  let workspace_skills = Filename.concat (workspace_root ()) "skills" in
  let local_skills = "skills" in
  [workspace_skills; local_skills]
  |> List.filter (fun d -> Sys.file_exists d && (try Sys.is_directory d with _ -> false))

let parse_skill_metadata skill_md_path =
  match read_file_full skill_md_path with
  | Error _ -> None
  | Ok content ->
      let lines = String.split_on_char '\n' content in
      let rec find_field key = function
        | [] -> None
        | line :: rest ->
            let line = trim line in
            if String.starts_with ~prefix:(key ^ ":") (String.lowercase_ascii line) then
              Some (trim (String.sub line (String.length key + 1) (String.length line - String.length key - 1)))
            else
              find_field key rest
      in
      let name =
        match find_field "name" lines with
        | Some s when s <> "" -> s
        | _ -> Filename.basename (Filename.dirname skill_md_path)
      in
      let description =
        match find_field "description" lines with
        | Some s -> s
        | None -> "No description"
      in
      Some (name, description, skill_md_path)

let find_skills_tool = {
  name = "find_skills";
  description = "Find locally available skills";
  parameters = [
    "query", `String "string: Optional query to filter skills"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    let query = Option.map String.lowercase_ascii (string_arg args "query") in
    let skills =
      discover_skill_dirs ()
      |> List.map Sys.readdir
      |> List.map Array.to_list
      |> List.flatten
      |> List.sort_uniq String.compare
      |> List.filter_map (fun skill_name ->
          let found =
            discover_skill_dirs ()
            |> List.find_map (fun root ->
                let md = Filename.concat (Filename.concat root skill_name) "SKILL.md" in
                if Sys.file_exists md then Some md else None)
          in
          match found with
          | None -> None
          | Some md -> parse_skill_metadata md)
      |> List.filter (fun (name, desc, _) ->
          match query with
          | None -> true
          | Some q ->
              let n = String.lowercase_ascii name in
              let d = String.lowercase_ascii desc in
              string_contains ~haystack:n ~needle:q || string_contains ~haystack:d ~needle:q)
    in
    `List (
      List.map (fun (name, description, location) ->
        `Assoc [
          ("name", `String name);
          ("description", `String description);
          ("location", `String location)
        ]) skills
    )
    |> Yojson.Safe.pretty_to_string
  );
}

let rec copy_tree src dst =
  let stats = Unix.lstat src in
  match stats.Unix.st_kind with
  | Unix.S_DIR ->
      mkdir_p dst;
      Sys.readdir src
      |> Array.to_list
      |> List.iter (fun name ->
          if name <> "." && name <> ".." then
            let s = Filename.concat src name in
            let d = Filename.concat dst name in
            copy_tree s d)
  | Unix.S_REG ->
      (match read_file_full src with
       | Ok content -> ignore (write_file_atomic dst content)
       | Error _ -> ())
  | _ -> ()

let install_skill_tool = {
  name = "install_skill";
  description = "Install a skill from a local directory or SKILL.md path into workspace/skills";
  parameters = [
    "source", `String "string: Local path to skill directory or SKILL.md";
    "name", `String "string: Optional installed skill name"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "source" with
    | Error e -> e
    | Ok source ->
        if String.starts_with ~prefix:"http://" source || String.starts_with ~prefix:"https://" source then
          "Error installing skill: remote URLs are not supported"
        else
          let cwd = Sys.getcwd () in
          let source_abs = absolute_path_from cwd source in
          if not (Sys.file_exists source_abs) then
            Printf.sprintf "Error installing skill: source does not exist: %s" source
          else
            let source_dir, skill_md =
              if (try Sys.is_directory source_abs with _ -> false) then
                let md = Filename.concat source_abs "SKILL.md" in
                (source_abs, md)
              else
                (Filename.dirname source_abs, source_abs)
            in
            if Filename.basename skill_md <> "SKILL.md" || not (Sys.file_exists skill_md) then
              "Error installing skill: source must contain SKILL.md"
            else
              let install_name =
                match string_arg args "name" with
                | Some n when trim n <> "" -> trim n
                | _ -> Filename.basename source_dir
              in
              let dst_root = Filename.concat (Filename.concat (workspace_root ()) "skills") install_name in
              (try
                 copy_tree source_dir dst_root;
                 Printf.sprintf "Skill installed: %s -> %s" source dst_root
               with exn ->
                 Printf.sprintf "Error installing skill: %s" (Printexc.to_string exn))
  );
}

type cron_job = {
  id : string;
  task : string;
  at_seconds : int option;
  every_seconds : int option;
  cron_expr : string option;
  enabled : bool;
  created_at : float;
}

let cron_job_to_json job =
  `Assoc [
    ("id", `String job.id);
    ("task", `String job.task);
    ("at_seconds", match job.at_seconds with Some n -> `Int n | None -> `Null);
    ("every_seconds", match job.every_seconds with Some n -> `Int n | None -> `Null);
    ("cron_expr", match job.cron_expr with Some s -> `String s | None -> `Null);
    ("enabled", `Bool job.enabled);
    ("created_at", `Float job.created_at)
  ]

let cron_job_of_json = function
  | `Assoc fields ->
      let get name = List.assoc_opt name fields in
      let id = match get "id" with Some (`String s) -> s | _ -> "" in
      let task = match get "task" with Some (`String s) -> s | _ -> "" in
      let at_seconds = match get "at_seconds" with Some (`Int n) -> Some n | _ -> None in
      let every_seconds = match get "every_seconds" with Some (`Int n) -> Some n | _ -> None in
      let cron_expr = match get "cron_expr" with Some (`String s) -> Some s | _ -> None in
      let enabled = match get "enabled" with Some (`Bool b) -> b | _ -> true in
      let created_at = match get "created_at" with Some (`Float f) -> f | Some (`Int n) -> float_of_int n | _ -> Unix.gettimeofday () in
      if id = "" || task = "" then None
      else Some { id; task; at_seconds; every_seconds; cron_expr; enabled; created_at }
  | _ -> None

let cron_store_path () =
  Filename.concat (Filename.concat (workspace_root ()) "cron") "jobs.json"

let load_cron_jobs () =
  let path = cron_store_path () in
  if not (Sys.file_exists path) then []
  else
    match read_file_full path with
    | Error _ -> []
    | Ok content ->
        (try
           Yojson.Safe.from_string content
           |> to_list
           |> List.filter_map cron_job_of_json
         with _ -> [])

let save_cron_jobs jobs =
  let path = cron_store_path () in
  let json = `List (List.map cron_job_to_json jobs) in
  write_file_atomic path (Yojson.Safe.pretty_to_string json)

let new_cron_id () =
  Printf.sprintf "job-%d-%06d" (int_of_float (Unix.gettimeofday ())) (Random.int 1000000)

let cron_tool = {
  name = "cron";
  description = "Manage scheduled jobs (add/list/remove/enable/disable)";
  parameters = [
    "action", `String "string: add | list | remove | enable | disable";
    "id", `String "string: Job ID for remove/enable/disable";
    "task", `String "string: Task text for add";
    "at_seconds", `String "integer: One-shot execution after N seconds";
    "every_seconds", `String "integer: Recurring execution every N seconds";
    "cron_expr", `String "string: Cron expression for recurring schedule"
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "action" with
    | Error e -> e
    | Ok action ->
        let action = String.lowercase_ascii (trim action) in
        let jobs = load_cron_jobs () in
        (match action with
         | "list" ->
             `Assoc [
               ("count", `Int (List.length jobs));
               ("jobs", `List (List.map cron_job_to_json jobs))
             ]
             |> Yojson.Safe.pretty_to_string
         | "add" ->
             (match required_string_arg args "task" with
              | Error e -> e
              | Ok task ->
                  let at_seconds = int_arg args "at_seconds" in
                  let every_seconds = int_arg args "every_seconds" in
                  let cron_expr = string_arg args "cron_expr" in
                  let schedules = [Option.is_some at_seconds; Option.is_some every_seconds; Option.is_some cron_expr] |> List.filter (fun b -> b) |> List.length in
                  if schedules <> 1 then
                    "Error adding cron job: provide exactly one of at_seconds, every_seconds, or cron_expr"
                  else
                    let job = {
                      id = new_cron_id ();
                      task;
                      at_seconds;
                      every_seconds;
                      cron_expr;
                      enabled = true;
                      created_at = Unix.gettimeofday ();
                    } in
                    (match save_cron_jobs (job :: jobs) with
                     | Ok () -> `Assoc [ ("status", `String "ok"); ("job", cron_job_to_json job) ] |> Yojson.Safe.pretty_to_string
                     | Error err -> Printf.sprintf "Error adding cron job: %s" err))
         | "remove" | "enable" | "disable" ->
             (match required_string_arg args "id" with
              | Error e -> e
              | Ok id ->
                  let exists = List.exists (fun j -> j.id = id) jobs in
                  if not exists then Printf.sprintf "Error: cron job not found: %s" id
                  else
                    let updated =
                      if action = "remove" then
                        List.filter (fun j -> j.id <> id) jobs
                      else
                        List.map (fun j -> if j.id = id then { j with enabled = (action = "enable") } else j) jobs
                    in
                    (match save_cron_jobs updated with
                     | Ok () -> Printf.sprintf "Cron job %s: %s" action id
                     | Error err -> Printf.sprintf "Error updating cron jobs: %s" err))
         | _ -> "Invalid action. Expected: add | list | remove | enable | disable")
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
  register_tool web_search_tool;
  register_tool web_fetch_tool;
  register_tool file_read_tool;
  register_tool write_file_tool;
  register_tool edit_file_tool;
  register_tool append_file_tool;
  register_tool shell_tool;
  register_tool exec_tool;
  register_tool list_directory_tool;
  register_tool list_dir_tool;
  register_tool find_skills_tool;
  register_tool install_skill_tool;
  register_tool cron_tool;
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
