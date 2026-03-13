(** Primitive filesystem and shell tools for the CLI assistant. *)

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
  List.iter (function
    | "" | "." -> ()
    | ".." ->
        begin
          match !stack with
          | _ :: tl -> stack := tl
          | [] -> ()
        end
    | part -> stack := part :: !stack) parts;
  let body = String.concat "/" (List.rev !stack) in
  if is_abs then if body = "" then "/" else "/" ^ body
  else if body = "" then "." else body

let absolute_path_from base path =
  let raw = if Filename.is_relative path then Filename.concat base path else path in
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
    if parent = path then path else find_existing_ancestor parent

let workspace_root () =
  let cfg = !active_sandbox_config in
  let cwd = Sys.getcwd () in
  let abs = absolute_path_from cwd cfg.workspace_root in
  try Unix.realpath abs with _ -> abs

let path_matches_allowlist path allowlist =
  let root = workspace_root () in
  List.exists
    (fun allowed ->
      let abs_allowed = absolute_path_from root allowed in
      is_within ~root:abs_allowed ~path)
    allowlist

let validate_path ~for_write ~allowlist path =
  let cfg = !active_sandbox_config in
  let root = workspace_root () in
  let root_real = try Unix.realpath root with _ -> root in
  if trim path = "" then Error "path is required"
  else
    let candidate = absolute_path_from root path in
    if path_matches_allowlist candidate allowlist then Ok candidate
    else if not cfg.restrict_to_workspace then Ok candidate
    else if not (is_within ~root ~path:candidate) then Error "path is outside workspace"
    else
      let anchor =
        if for_write then find_existing_ancestor (Filename.dirname candidate)
        else find_existing_ancestor candidate
      in
      let anchor_real = try Unix.realpath anchor with _ -> anchor in
      if not (is_within ~root:root_real ~path:anchor_real) then
        Error "path resolves outside workspace"
      else
        Ok candidate

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
      (match !mode with `Normal -> () | _ -> raise (Invalid_argument "Unclosed quote in command"));
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
      match split_command command with
      | tokens -> check_tokens tokens
      | exception Invalid_argument msg -> Error msg
  ) else
    Ok ()

let json_assoc_or_empty = function
  | `Assoc _ as json -> json
  | `Null -> `Assoc []
  | `String s ->
      begin
        try
          match Yojson.Safe.from_string s with
          | `Assoc _ as json -> json
          | other -> `Assoc [ ("value", other) ]
        with _ -> `Assoc [ ("value", `String s) ]
      end
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

let required_string_arg json name =
  match string_arg json name with
  | Some value when trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "%s is required" name)

let file_read_tool = {
  name = "read_file";
  description = "Read and return the complete contents of a file from the filesystem";
  parameters = [ "path", `String "string: The absolute or relative path to the file to read" ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path" with
    | Error err -> err
    | Ok path ->
        begin
          match validate_path ~for_write:false ~allowlist:(!active_sandbox_config).allow_read_paths path with
          | Error err -> Printf.sprintf "Error reading file %s: %s" path err
          | Ok safe_path ->
              begin
                match read_file_full safe_path with
                | Ok content -> content
                | Error err -> Printf.sprintf "Error reading file %s: %s" path err
              end
        end);
}

let write_file_tool = {
  name = "write_file";
  description = "Write content to a file";
  parameters = [
    "path", `String "string: The file path to write";
    "content", `String "string: The content to write";
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "content" with
    | Error err, _ | _, Error err -> err
    | Ok path, Ok content ->
        begin
          match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
          | Error err -> Printf.sprintf "Error writing file %s: %s" path err
          | Ok safe_path ->
              begin
                match write_file_atomic safe_path content with
                | Ok () -> Printf.sprintf "File written: %s" path
                | Error err -> Printf.sprintf "Error writing file %s: %s" path err
              end
        end);
}

let edit_file_tool = {
  name = "edit_file";
  description = "Edit a file by replacing old_text with new_text. old_text must exist exactly once.";
  parameters = [
    "path", `String "string: The file path to edit";
    "old_text", `String "string: Exact text to replace";
    "new_text", `String "string: Replacement text";
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "old_text", required_string_arg args "new_text" with
    | Error err, _, _ | _, Error err, _ | _, _, Error err -> err
    | Ok path, Ok old_text, Ok new_text ->
        begin
          match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
          | Error err -> Printf.sprintf "Error editing file %s: %s" path err
          | Ok safe_path ->
              begin
                match read_file_full safe_path with
                | Error err -> Printf.sprintf "Error editing file %s: %s" path err
                | Ok content ->
                    let count =
                      let rec count_from idx acc =
                        if idx >= String.length content then acc
                        else if idx + String.length old_text <= String.length content
                                && String.sub content idx (String.length old_text) = old_text then
                          count_from (idx + 1) (acc + 1)
                        else
                          count_from (idx + 1) acc
                      in
                      if old_text = "" then 0 else count_from 0 0
                    in
                    if count = 0 then "Error editing file: old_text not found"
                    else if count > 1 then Printf.sprintf "Error editing file: old_text appears %d times" count
                    else
                      let idx =
                        let rec find_from i =
                          if i + String.length old_text > String.length content then -1
                          else if String.sub content i (String.length old_text) = old_text then i
                          else find_from (i + 1)
                        in
                        find_from 0
                      in
                      let replaced =
                        String.sub content 0 idx
                        ^ new_text
                        ^ String.sub content (idx + String.length old_text)
                            (String.length content - idx - String.length old_text)
                      in
                      begin
                        match write_file_atomic safe_path replaced with
                        | Ok () -> Printf.sprintf "File edited: %s" path
                        | Error err -> Printf.sprintf "Error editing file %s: %s" path err
                      end
              end
        end);
}

let append_file_tool = {
  name = "append_file";
  description = "Append content to the end of a file";
  parameters = [
    "path", `String "string: The file path to append";
    "content", `String "string: The content to append";
  ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    match required_string_arg args "path", required_string_arg args "content" with
    | Error err, _ | _, Error err -> err
    | Ok path, Ok content_to_append ->
        begin
          match validate_path ~for_write:true ~allowlist:(!active_sandbox_config).allow_write_paths path with
          | Error err -> Printf.sprintf "Error appending file %s: %s" path err
          | Ok safe_path ->
              let existing = match read_file_full safe_path with Ok value -> value | Error _ -> "" in
              begin
                match write_file_atomic safe_path (existing ^ content_to_append) with
                | Ok () -> Printf.sprintf "Appended to file: %s" path
                | Error err -> Printf.sprintf "Error appending file %s: %s" path err
              end
        end);
}

let list_directory_core path =
  match validate_path ~for_write:false ~allowlist:(!active_sandbox_config).allow_read_paths path with
  | Error err -> Printf.sprintf "Error listing directory %s: %s" path err
  | Ok safe_path ->
      begin
        try
          if not (Sys.file_exists safe_path) then
            Printf.sprintf "Error listing directory %s: path does not exist" path
          else if not (Sys.is_directory safe_path) then
            Printf.sprintf "Error listing directory %s: path is not a directory" path
          else
            let entries = Sys.readdir safe_path |> Array.to_list |> List.sort String.compare in
            let lines =
              List.map
                (fun name ->
                  let full = Filename.concat safe_path name in
                  let kind = try if Sys.is_directory full then "DIR" else "FILE" with _ -> "FILE" in
                  Printf.sprintf "%s: %s" kind name)
                entries
            in
            Printf.sprintf "Directory listing for %s:\n%s"
              (if path = "" || path = "." then "current directory" else path)
              (String.concat "\n" lines)
        with exn ->
          Printf.sprintf "Error listing directory %s: %s" path (Printexc.to_string exn)
      end

let list_directory_tool = {
  name = "list_directory";
  description = "List files and directories in the current or specified path";
  parameters = [ "path", `String "string: The directory path to list (default: current directory)" ];
  execute = (fun args ->
    let args = json_assoc_or_empty args in
    list_directory_core (Option.value ~default:"." (string_arg args "path")));
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
    with exn ->
      Error (Printexc.to_string exn)

let shell_tool = {
  name = "execute_command";
  description = "Execute a command and return the output";
  parameters = [ "command", `String "string: The command to execute" ];
  execute = (fun args ->
    let cfg = !active_sandbox_config in
    let args = json_assoc_or_empty args in
    match required_string_arg args "command" with
    | Error err -> err
    | Ok command ->
        begin
          match guard_command command with
          | Error err -> err
          | Ok () ->
              begin
                match run_command ~timeout_seconds:cfg.exec_timeout_seconds command with
                | Error err -> Printf.sprintf "Error executing command: %s" err
                | Ok (output, exit_code) ->
                    let output = truncate_output output in
                    Printf.sprintf "Command output:\n%s\nExit status: %d" output exit_code
              end
        end);
}

let exec_tool =
  { shell_tool with
    name = "exec";
    description = "Execute a shell command and return its output. Use with caution.";
  }

let tool_registry = ref []

let register_tool tool =
  tool_registry := tool :: !tool_registry

let get_tool name =
  List.find_opt (fun tool -> tool.name = name) !tool_registry

let get_all_tools () =
  !tool_registry |> List.rev |> List.map (fun tool -> (tool.name, tool.description))

let execute_tool tool_name arguments =
  match get_tool tool_name with
  | Some tool ->
      let normalized = json_assoc_or_empty arguments in
      begin
        try tool.execute normalized
        with exn -> Printf.sprintf "Error executing tool %s: %s" tool_name (Printexc.to_string exn)
      end
  | None -> Printf.sprintf "Tool %s not found" tool_name

let parse_parameter_schema = function
  | `String desc ->
      begin
        match String.split_on_char ':' desc with
        | ty :: rest when rest <> [] ->
            let param_type = trim ty in
            let param_desc = trim (String.concat ":" rest) in
            (if param_type = "" then "string" else param_type), param_desc
        | _ -> "string", trim desc
      end
  | _ -> "string", ""

let init_default_tools ?sandbox_config () =
  Random.self_init ();
  Option.iter set_sandbox_config sandbox_config;
  tool_registry := [];
  List.iter register_tool
    [
      file_read_tool;
      write_file_tool;
      edit_file_tool;
      append_file_tool;
      shell_tool;
      exec_tool;
      list_directory_tool;
      list_dir_tool;
    ]

let tools_to_json () =
  let tool_list =
    !tool_registry
    |> List.rev
    |> List.map (fun tool ->
         let properties =
           tool.parameters
           |> List.map (fun (name, schema) ->
                let param_type, param_desc = parse_parameter_schema schema in
                (name, `Assoc [ ("type", `String param_type); ("description", `String param_desc) ]))
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
               ("required", `List required);
             ]);
           ]);
         ])
  in
  `List tool_list

let parse_tool_calls json =
  try
    match member "choices" json |> to_list with
    | first_choice :: _ ->
        begin
          try
            let message = member "message" first_choice in
            Some (member "tool_calls" message |> to_list)
          with _ -> None
        end
    | [] -> None
  with _ -> None

let extract_tool_arguments tool_call =
  try
    let function_ = member "function" tool_call in
    Some (to_string (member "arguments" function_))
  with _ -> None
