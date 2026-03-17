(** Registry-driven CLI tools. *)

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

type tool_result = {
  content : string;
  is_error : bool;
  status_code : int option;
  bytes : int;
  duration_ms : int option;
  error_type : string option;
}

type tool = {
  definition : Llm_types.tool_definition;
  execute : chat_id:int -> Yojson.Safe.t -> tool_result;
}

type t = {
  sandbox : sandbox_config;
  data_dir : string;
  skills_dir : string;
  db : Db.t;
  memory : Memory.t;
  skills : Skills.t;
  tools : tool list;
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

let success ?status_code ?duration_ms ?error_type content =
  {
    content;
    is_error = false;
    status_code;
    bytes = String.length content;
    duration_ms;
    error_type;
  }

let failure ?status_code ?duration_ms ?(error_type="tool_error") content =
  {
    content;
    is_error = true;
    status_code;
    bytes = String.length content;
    duration_ms;
    error_type = Some error_type;
  }

let trim = String.trim

let string_contains ~haystack ~needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then true
  else if n_len > h_len then false
  else
    let rec loop index =
      if index > h_len - n_len then false
      else if String.sub haystack index n_len = needle then true
      else loop (index + 1)
    in
    loop 0

let ensure_trailing_slash path =
  if path = "/" then "/"
  else if String.ends_with ~suffix:"/" path then path
  else path ^ "/"

let normalize_lexical path =
  let is_abs = String.length path > 0 && path.[0] = '/' in
  let parts = String.split_on_char '/' path in
  let stack = ref [] in
  List.iter
    (function
      | "" | "." -> ()
      | ".." ->
          begin
            match !stack with
            | _ :: rest -> stack := rest
            | [] -> ()
          end
      | part -> stack := part :: !stack)
    parts;
  let body = String.concat "/" (List.rev !stack) in
  if is_abs then if body = "" then "/" else "/" ^ body
  else if body = "" then "." else body

let absolute_path_from base path =
  let raw = if Filename.is_relative path then Filename.concat base path else path in
  normalize_lexical raw

let is_within ~root ~path =
  path = root || String.starts_with ~prefix:(ensure_trailing_slash root) path

let workspace_root sandbox =
  let cwd = Sys.getcwd () in
  let abs = absolute_path_from cwd sandbox.workspace_root in
  try Unix.realpath abs with _ -> abs

let path_matches_allowlist sandbox path allowlist =
  let root = workspace_root sandbox in
  List.exists
    (fun allowed ->
      let allowed_abs = absolute_path_from root allowed in
      is_within ~root:allowed_abs ~path)
    allowlist

let path_exists path =
  try
    ignore (Unix.lstat path);
    true
  with _ ->
    false

let rec find_existing_ancestor path =
  if path_exists path then path
  else
    let parent = Filename.dirname path in
    if parent = path then path else find_existing_ancestor parent

let validate_path sandbox ~for_write ~allowlist path =
  if trim path = "" then Error "path is required"
  else
    let root = workspace_root sandbox in
    let candidate = absolute_path_from root path in
    if path_matches_allowlist sandbox candidate allowlist then Ok candidate
    else if not sandbox.restrict_to_workspace then Ok candidate
    else if not (is_within ~root ~path:candidate) then Error "path is outside workspace"
    else
      let anchor =
        if for_write then find_existing_ancestor (Filename.dirname candidate)
        else find_existing_ancestor candidate
      in
      let root_real = try Unix.realpath root with _ -> root in
      let anchor_real = try Unix.realpath anchor with _ -> anchor in
      if is_within ~root:root_real ~path:anchor_real then Ok candidate
      else Error "path resolves outside workspace"

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
  with exn -> Error (Printexc.to_string exn)

let write_file_atomic path content =
  try
    ensure_parent_dir path;
    let dir = Filename.dirname path in
    let tmp =
      Filename.concat dir
        (Printf.sprintf ".tmp-oclaw-%d-%06d"
           (int_of_float (Unix.gettimeofday ()))
           (Random.int 1_000_000))
    in
    Stdlib.Out_channel.with_open_bin tmp (fun channel -> output_string channel content);
    Unix.rename tmp path;
    Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let json_assoc_or_empty = function
  | `Assoc _ as json -> json
  | `Null -> `Assoc []
  | other -> other

let string_arg json name =
  match member name json with
  | `String value -> Some value
  | `Int value -> Some (string_of_int value)
  | `Intlit value -> Some value
  | `Float value -> Some (string_of_float value)
  | `Bool value -> Some (string_of_bool value)
  | _ -> None

let required_string_arg json name =
  match string_arg json name with
  | Some value when trim value <> "" -> Ok value
  | _ -> Error (name ^ " is required")

let int_arg json name =
  match member name json with
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | `String value -> int_of_string_opt (trim value)
  | _ -> None

let list_arg json name =
  match member name json with
  | `List values -> values
  | _ -> []

let default_exec_deny_patterns = [
  "rm -rf";
  "rm -fr";
  "mkfs";
  "shutdown";
  "reboot";
  "poweroff";
  "sudo ";
  "curl ";
  "wget ";
  "| sh";
  "| bash";
  "chown ";
  "kill -9";
  "git push";
]

let normalize_exec_token token =
  let token = trim token in
  let len = String.length token in
  if len >= 2
     && ((token.[0] = '\'' && token.[len - 1] = '\'')
         || (token.[0] = '"' && token.[len - 1] = '"'))
  then
    String.sub token 1 (len - 2)
  else
    token

let split_command command =
  let len = String.length command in
  let buffer = Buffer.create len in
  let tokens = ref [] in
  let mode = ref `Normal in
  let flush () =
    if Buffer.length buffer > 0 then (
      tokens := Buffer.contents buffer :: !tokens;
      Buffer.clear buffer
    )
  in
  let rec loop index =
    if index >= len then (
      begin
        match !mode with
        | `Normal -> ()
        | _ -> raise (Invalid_argument "Unclosed quote in command")
      end;
      flush ();
      List.rev !tokens
    ) else
      let char = command.[index] in
      match !mode with
      | `Normal ->
          if List.mem char [ ' '; '\t'; '\n' ] then (
            flush ();
            loop (index + 1)
          ) else if char = '\'' then (
            mode := `Single;
            loop (index + 1)
          ) else if char = '"' then (
            mode := `Double;
            loop (index + 1)
          ) else (
            Buffer.add_char buffer char;
            loop (index + 1)
          )
      | `Single ->
          if char = '\'' then (
            mode := `Normal;
            loop (index + 1)
          ) else (
            Buffer.add_char buffer char;
            loop (index + 1)
          )
      | `Double ->
          if char = '"' then (
            mode := `Normal;
            loop (index + 1)
          ) else (
            Buffer.add_char buffer char;
            loop (index + 1)
          )
  in
  loop 0

let guard_command sandbox command =
  let lower = String.lowercase_ascii command in
  let explicitly_allowed =
    sandbox.exec_custom_allow_patterns <> []
    && List.exists
         (fun pattern ->
           string_contains ~haystack:lower ~needle:(String.lowercase_ascii pattern))
         sandbox.exec_custom_allow_patterns
  in
  if sandbox.exec_enable_deny_patterns && not explicitly_allowed then (
    let deny = default_exec_deny_patterns @ sandbox.exec_custom_deny_patterns in
    if List.exists (fun pattern ->
           string_contains ~haystack:lower ~needle:(String.lowercase_ascii pattern))
         deny
    then
      Error "Command blocked by safety guard"
    else
      match split_command command with
      | exception Invalid_argument err -> Error err
      | tokens ->
          let root = workspace_root sandbox in
          let rec check = function
            | [] -> Ok ()
            | token :: rest ->
                let token = normalize_exec_token token in
                if token = "" then check rest
                else if String.length token > 0 && token.[0] = '/' then
                  let path = absolute_path_from root token in
                  if sandbox.restrict_to_workspace && not (is_within ~root ~path) then
                    Error "Command blocked by path restriction"
                  else
                    check rest
                else
                  check rest
          in
          check tokens
  ) else
    Ok ()

let truncate_output ?(limit=10_000) output =
  if String.length output <= limit then output
  else
    let remaining = String.length output - limit in
    String.sub output 0 limit ^ Printf.sprintf "\n... (truncated, %d more chars)" remaining

let has_timeout_binary =
  lazy (Sys.command "command -v timeout >/dev/null 2>&1" = 0)

let run_command ~timeout_seconds command =
  let shell_command = command ^ " 2>&1" in
  let launcher, argv =
    if timeout_seconds > 0 && Lazy.force has_timeout_binary then
      ("timeout", [| "timeout"; string_of_int timeout_seconds; "sh"; "-lc"; shell_command |])
    else
      ("sh", [| "sh"; "-lc"; shell_command |])
  in
  try
    let started = Unix.gettimeofday () in
    let channel = Unix.open_process_args_in launcher argv in
    let buffer = Buffer.create 1024 in
    (try
       while true do
         let line = input_line channel in
         Buffer.add_string buffer line;
         Buffer.add_char buffer '\n'
       done
     with End_of_file -> ());
    let status = Unix.close_process_in channel in
    let exit_code =
      match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED signal -> -signal
      | Unix.WSTOPPED signal -> -signal
    in
    Ok (Buffer.contents buffer, exit_code, int_of_float ((Unix.gettimeofday () -. started) *. 1000.0))
  with exn ->
    Error (Printexc.to_string exn)

let run_process_args program argv =
  try
    let started = Unix.gettimeofday () in
    let channel = Unix.open_process_args_in program argv in
    let buffer = Buffer.create 1024 in
    (try
       while true do
         let line = input_line channel in
         Buffer.add_string buffer line;
         Buffer.add_char buffer '\n'
       done
     with End_of_file -> ());
    let status = Unix.close_process_in channel in
    let exit_code =
      match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED signal -> -signal
      | Unix.WSTOPPED signal -> -signal
    in
    Ok (Buffer.contents buffer, exit_code, int_of_float ((Unix.gettimeofday () -. started) *. 1000.0))
  with exn ->
    Error (Printexc.to_string exn)

let relative_path ~root path =
  let root = ensure_trailing_slash root in
  if String.starts_with ~prefix:root path then
    String.sub path (String.length root) (String.length path - String.length root)
  else
    path

let glob_regex pattern =
  let buffer = Buffer.create (String.length pattern * 2) in
  Buffer.add_char buffer '^';
  let rec loop index =
    if index >= String.length pattern then Buffer.add_char buffer '$'
    else
      match pattern.[index] with
      | '*' ->
          if index + 1 < String.length pattern && pattern.[index + 1] = '*' then (
            Buffer.add_string buffer ".*";
            loop (index + 2)
          ) else (
            Buffer.add_string buffer "[^/]*";
            loop (index + 1)
          )
      | '?' ->
          Buffer.add_char buffer '.';
          loop (index + 1)
      | '.' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '^' | '$' | '\\' as char ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer char;
          loop (index + 1)
      | char ->
          Buffer.add_char buffer char;
          loop (index + 1)
  in
  loop 0;
  Str.regexp (Buffer.contents buffer)

let rec walk_files acc path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      Sys.readdir path
      |> Array.to_list
      |> List.fold_left (fun current name -> walk_files current (Filename.concat path name)) acc
    else
      path :: acc
  else
    acc

let markdown_of_messages messages =
  messages
  |> List.map (fun (message : Db.stored_message) ->
         let title =
           match message.Db.role with
           | "assistant" -> "Assistant"
           | "user" -> "User"
           | role -> String.capitalize_ascii role
         in
         "## " ^ title ^ "\n\n" ^ message.Db.content)
  |> String.concat "\n\n"

let schema properties required =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc properties);
    ("required", `List (List.map (fun name -> `String name) required));
  ]

let make_tool name description input_schema execute =
  {
    definition = { Llm_types.name; description; input_schema };
    execute;
  }

let read_file_tool sandbox =
  make_tool
    "read_file"
    "Read and return the contents of a file."
    (schema [ ("path", `Assoc [ ("type", `String "string") ]) ] [ "path" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "path" with
      | Error err -> failure err
      | Ok path ->
          begin
            match validate_path sandbox ~for_write:false ~allowlist:sandbox.allow_read_paths path with
            | Error err -> failure ("Error reading file: " ^ err)
            | Ok safe_path ->
                begin
                  match read_file_full safe_path with
                  | Ok content -> success content
                  | Error err -> failure ("Error reading file: " ^ err)
                end
          end)

let write_file_tool sandbox =
  make_tool
    "write_file"
    "Write content to a file."
    (schema
       [
         ("path", `Assoc [ ("type", `String "string") ]);
         ("content", `Assoc [ ("type", `String "string") ]);
       ]
       [ "path"; "content" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "path", required_string_arg args "content" with
      | Error err, _ | _, Error err -> failure err
      | Ok path, Ok content ->
          begin
            match validate_path sandbox ~for_write:true ~allowlist:sandbox.allow_write_paths path with
            | Error err -> failure ("Error writing file: " ^ err)
            | Ok safe_path ->
                begin
                  match write_file_atomic safe_path content with
                  | Ok () -> success ("File written: " ^ path)
                  | Error err -> failure ("Error writing file: " ^ err)
                end
          end)

let edit_file_tool sandbox =
  make_tool
    "edit_file"
    "Edit a file by replacing one exact text block with another."
    (schema
       [
         ("path", `Assoc [ ("type", `String "string") ]);
         ("old_text", `Assoc [ ("type", `String "string") ]);
         ("new_text", `Assoc [ ("type", `String "string") ]);
       ]
       [ "path"; "old_text"; "new_text" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "path", required_string_arg args "old_text", required_string_arg args "new_text" with
      | Error err, _, _ | _, Error err, _ | _, _, Error err -> failure err
      | Ok path, Ok old_text, Ok new_text ->
          begin
            match validate_path sandbox ~for_write:true ~allowlist:sandbox.allow_write_paths path with
            | Error err -> failure ("Error editing file: " ^ err)
            | Ok safe_path ->
                begin
                  match read_file_full safe_path with
                  | Error err -> failure ("Error editing file: " ^ err)
                  | Ok content ->
                      let count = ref 0 in
                      let index = ref None in
                      let start = ref 0 in
                      while !start <= String.length content - String.length old_text && old_text <> "" do
                        if String.sub content !start (String.length old_text) = old_text then (
                          incr count;
                          if !index = None then index := Some !start
                        );
                        incr start
                      done;
                      if !count = 0 then failure "Error editing file: old_text not found"
                      else if !count > 1 then failure (Printf.sprintf "Error editing file: old_text appears %d times" !count)
                      else
                        let idx = Option.value ~default:0 !index in
                        let replaced =
                          String.sub content 0 idx
                          ^ new_text
                          ^ String.sub content (idx + String.length old_text)
                              (String.length content - idx - String.length old_text)
                        in
                        begin
                          match write_file_atomic safe_path replaced with
                          | Ok () -> success ("File edited: " ^ path)
                          | Error err -> failure ("Error editing file: " ^ err)
                        end
                end
          end)

let bash_tool sandbox =
  make_tool
    "bash"
    "Execute a shell command inside the configured workspace."
    (schema [ ("command", `Assoc [ ("type", `String "string") ]) ] [ "command" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "command" with
      | Error err -> failure err
      | Ok command ->
          begin
            match guard_command sandbox command with
            | Error err -> failure err
            | Ok () ->
                begin
                  match run_command ~timeout_seconds:sandbox.exec_timeout_seconds command with
                  | Error err -> failure ("Error executing command: " ^ err)
                  | Ok (output, exit_code, duration_ms) ->
                      let content =
                        Printf.sprintf "Command output:\n%s\nExit status: %d" (truncate_output output) exit_code
                      in
                      if exit_code = 0 then success ~status_code:exit_code ~duration_ms content
                      else failure ~status_code:exit_code ~duration_ms ~error_type:"command_failed" content
                end
          end)

let glob_tool sandbox =
  make_tool
    "glob"
    "Find files matching a glob pattern inside the workspace."
    (schema
       [
         ("pattern", `Assoc [ ("type", `String "string") ]);
         ("path", `Assoc [ ("type", `String "string") ]);
       ]
       [ "pattern" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "pattern" with
      | Error err -> failure err
      | Ok pattern ->
          let base = Option.value ~default:"." (string_arg args "path") in
          begin
            match validate_path sandbox ~for_write:false ~allowlist:sandbox.allow_read_paths base with
            | Error err -> failure ("Error running glob: " ^ err)
            | Ok safe_base ->
                let files = walk_files [] safe_base in
                let regex = glob_regex pattern in
                let root = workspace_root sandbox in
                let matches =
                  files
                  |> List.map (relative_path ~root)
                  |> List.filter (fun path -> Str.string_match regex path 0)
                  |> List.sort String.compare
                in
                success
                  (if matches = [] then "No files matched."
                   else String.concat "\n" matches)
          end)

let grep_tool sandbox =
  make_tool
    "grep"
    "Search file contents with ripgrep."
    (schema
       [
         ("pattern", `Assoc [ ("type", `String "string") ]);
         ("path", `Assoc [ ("type", `String "string") ]);
       ]
       [ "pattern" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "pattern" with
      | Error err -> failure err
      | Ok pattern ->
          let path = Option.value ~default:"." (string_arg args "path") in
          begin
            match validate_path sandbox ~for_write:false ~allowlist:sandbox.allow_read_paths path with
            | Error err -> failure ("Error running grep: " ^ err)
            | Ok safe_path ->
                begin
                  match run_process_args "rg" [| "rg"; "-n"; "--no-heading"; "--color"; "never"; pattern; safe_path |] with
                  | Ok (output, 0, duration_ms) ->
                      success ~status_code:0 ~duration_ms (truncate_output output)
                  | Ok (output, 1, duration_ms) ->
                      success ~status_code:1 ~duration_ms "No matches found."
                  | Ok (output, exit_code, duration_ms) ->
                      failure ~status_code:exit_code ~duration_ms (truncate_output output)
                  | Error err ->
                      failure ("Error running grep: " ^ err)
                end
          end)

let read_memory_tool memory =
  make_tool
    "read_memory"
    "Read global or chat memory from AGENTS.md files."
    (schema
       [
         ("scope", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "global"; `String "chat" ]) ]);
       ]
       [ "scope" ])
    (fun ~chat_id args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "scope" with
      | Error err -> failure err
      | Ok "global" ->
          begin
            match Memory.read_global_memory memory with
            | Some content when String.trim content <> "" -> success content
            | _ -> success "No global memory file found."
          end
      | Ok "chat" ->
          begin
            match Memory.read_chat_memory memory chat_id with
            | Some content when String.trim content <> "" -> success content
            | _ -> success "No chat memory file found."
          end
      | Ok _ -> failure "scope must be 'global' or 'chat'")

let write_memory_tool db memory =
  make_tool
    "write_memory"
    "Write global or chat memory to AGENTS.md files."
    (schema
       [
         ("scope", `Assoc [ ("type", `String "string"); ("enum", `List [ `String "global"; `String "chat" ]) ]);
         ("content", `Assoc [ ("type", `String "string") ]);
       ]
       [ "scope"; "content" ])
    (fun ~chat_id args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "scope", required_string_arg args "content" with
      | Error err, _ | _, Error err -> failure err
      | Ok scope, Ok content ->
          let write_result =
            match scope with
            | "global" -> Memory.write_global_memory memory content
            | "chat" -> Memory.write_chat_memory memory chat_id content
            | _ -> Error "scope must be 'global' or 'chat'"
          in
          begin
            match write_result with
            | Error err -> failure ("Failed to write memory: " ^ err)
            | Ok () ->
                ignore (Db.insert_memory db ~chat_id:(if scope = "chat" then Some chat_id else None) ~scope ~content ~source:"write_memory");
                success ("Memory saved to " ^ scope ^ " scope.")
          end)

let todo_read_tool db =
  make_tool
    "todo_read"
    "Read the current per-chat todo list."
    (schema [] [])
    (fun ~chat_id _args ->
      match Db.load_todo db ~chat_id with
      | Ok (Some json) -> success json
      | Ok None -> success "[]"
      | Error err -> failure ("Failed to read todo list: " ^ err))

let todo_write_tool db =
  make_tool
    "todo_write"
    "Replace the current per-chat todo list."
    (schema [ ("items", `Assoc [ ("type", `String "array") ]) ] [ "items" ])
    (fun ~chat_id args ->
      let items = list_arg args "items" in
      let todo_json = `List items |> Yojson.Safe.to_string in
      match Db.save_todo db ~chat_id ~todo_json with
      | Ok () -> success "Todo list updated."
      | Error err -> failure ("Failed to write todo list: " ^ err))

let activate_skill_tool skills =
  make_tool
    "activate_skill"
    "Load a skill's full instructions from workspace/skills."
    (schema [ ("name", `Assoc [ ("type", `String "string") ]) ] [ "name" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "name" with
      | Error err -> failure err
      | Ok name ->
          begin
            match Skills.activate_skill skills name with
            | Ok content -> success content
            | Error err -> failure err
          end)

let sync_skills_tool skills =
  make_tool
    "sync_skills"
    "Download a skill from a GitHub skills repository into the local skills directory."
    (schema
       [
         ("name", `Assoc [ ("type", `String "string") ]);
         ("repo", `Assoc [ ("type", `String "string") ]);
       ]
       [ "name" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "name" with
      | Error err -> failure err
      | Ok name ->
          let repo = Option.value ~default:"vercel-labs/skills" (string_arg args "repo") in
          begin
            match Skills.sync_skill skills ~repo name with
            | Ok message -> success message
            | Error err -> failure err
          end)

let export_chat_tool sandbox db =
  make_tool
    "export_chat"
    "Export the current chat history to a markdown file."
    (schema [ ("path", `Assoc [ ("type", `String "string") ]) ] [])
    (fun ~chat_id args ->
      let args = json_assoc_or_empty args in
      let default_path =
        Filename.concat
          (Filename.concat (Filename.concat (workspace_root sandbox) "workspace") "runtime")
          (Printf.sprintf "chat-%d.md" chat_id)
      in
      let target = Option.value ~default:default_path (string_arg args "path") in
      match Db.get_all_messages db ~chat_id with
      | Error err -> failure ("Failed to export chat: " ^ err)
      | Ok messages ->
          begin
            match validate_path sandbox ~for_write:true ~allowlist:sandbox.allow_write_paths target with
            | Error err -> failure ("Failed to export chat: " ^ err)
            | Ok safe_path ->
                begin
                  match write_file_atomic safe_path (markdown_of_messages messages) with
                  | Ok () -> success ("Chat exported to " ^ safe_path)
                  | Error err -> failure ("Failed to export chat: " ^ err)
                end
          end)

let create_default_registry ?(sandbox_config=default_sandbox_config) ~data_dir ~skills_dir ~db () =
  Random.self_init ();
  let runtime_dir = Filename.concat data_dir "runtime" in
  let memory = Memory.create ~data_dir ~runtime_dir in
  let skills = Skills.create ~skills_dir in
  let tools =
    [
      bash_tool sandbox_config;
      read_file_tool sandbox_config;
      write_file_tool sandbox_config;
      edit_file_tool sandbox_config;
      glob_tool sandbox_config;
      grep_tool sandbox_config;
      read_memory_tool memory;
      write_memory_tool db memory;
      todo_read_tool db;
      todo_write_tool db;
      activate_skill_tool skills;
      sync_skills_tool skills;
      export_chat_tool sandbox_config db;
    ]
  in
  {
    sandbox = sandbox_config;
    data_dir;
    skills_dir;
    db;
    memory;
    skills;
    tools;
  }

let definitions registry =
  List.map (fun tool -> tool.definition) registry.tools

let find_tool registry name =
  List.find_opt (fun tool -> String.equal tool.definition.name name) registry.tools

let execute registry ~chat_id name input =
  match find_tool registry name with
  | Some tool ->
      begin
        try tool.execute ~chat_id (json_assoc_or_empty input)
        with exn -> failure (Printf.sprintf "Error executing tool %s: %s" name (Printexc.to_string exn))
      end
  | None ->
      failure ("Tool not found: " ^ name)

let active_registry = ref None

let fallback_db data_dir =
  let path = Filename.concat (Filename.concat data_dir "runtime") "default-tools.db" in
  match Db.create path with
  | Ok db -> db
  | Error err -> failwith err

let init_default_tools ?sandbox_config ?(data_dir="workspace") ?skills_dir ?db () =
  let skills_dir = Option.value ~default:(Filename.concat data_dir "skills") skills_dir in
  let db = Option.value ~default:(fallback_db data_dir) db in
  let registry = create_default_registry ?sandbox_config ~data_dir ~skills_dir ~db () in
  active_registry := Some registry

let with_active_registry f =
  match !active_registry with
  | Some registry -> f registry
  | None ->
      init_default_tools ();
      match !active_registry with
      | Some registry -> f registry
      | None -> failwith "tool registry initialization failed"

let get_all_tools () =
  with_active_registry (fun registry ->
      registry.tools
      |> List.map (fun tool -> (tool.definition.name, tool.definition.description)))

let execute_tool ?(chat_id=1) name input =
  with_active_registry (fun registry ->
      let result = execute registry ~chat_id name input in
      result.content)

let tools_to_json () =
  with_active_registry (fun registry ->
      `List (List.map Llm_types.tool_definition_to_yojson (definitions registry)))
