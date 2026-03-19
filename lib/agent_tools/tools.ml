(** Minimal CLI tools: read_file, write_file, edit_file, bash. No sandbox restrictions. *)

[@@@warning "-69"]
[@@@warning "-32"]
[@@@warning "-27"]

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
  tools : tool list;
}

let success ?status_code ?duration_ms ?error_type content =
  { content; is_error = false; status_code; bytes = String.length content; duration_ms; error_type }

let failure ?status_code ?duration_ms ?(error_type="tool_error") content =
  { content; is_error = true; status_code; bytes = String.length content; duration_ms; error_type = Some error_type }

let trim = String.trim

let ensure_parent_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (mkdir_p (Filename.dirname dir); Unix.mkdir dir 0o755)
  in
  mkdir_p (Filename.dirname path)

let read_file_full path =
  try Ok (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with exn -> Error (Printexc.to_string exn)

let write_file_atomic path content =
  try
    ensure_parent_dir path;
    let tmp_path = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
    Stdlib.Out_channel.with_open_bin tmp_path (fun ch -> Stdlib.Out_channel.output_string ch content);
    Unix.rename tmp_path path;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

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
  { definition; execute = fun ~chat_id:_ args -> execute args }

let required_string_arg json name =
  match Yojson.Safe.Util.member name json with
  | `String value when trim value <> "" -> Ok value
  | _ -> Error (name ^ " is required")

let json_assoc_or_empty = function | `Assoc fields -> `Assoc fields | _ -> `Assoc []

let truncate_output output =
  if String.length output > 4096 then String.sub output 0 4096 ^ "\n... (output truncated)" else output

let run_command ~timeout_seconds command =
  let start_time = Unix.gettimeofday () in
  try
    let cmd = Printf.sprintf "timeout %d %s 2>&1" timeout_seconds command in
    let ic = Unix.open_process_in cmd in
    let output = Stdlib.In_channel.input_all ic in
    let status = Unix.close_process_in ic in
    let end_time = Unix.gettimeofday () in
    let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
    let exit_code = match status with Unix.WEXITED c -> c | _ -> 1 in
    Ok (output, exit_code, duration_ms)
  with exn -> Error (Printexc.to_string exn)

let bash_tool =
  make_tool "bash" "Execute a shell command."
    [("command", `Assoc [("type", `String "string")])]
    (fun args ->
      match required_string_arg (json_assoc_or_empty args) "command" with
      | Error err -> failure err
      | Ok command ->
          match run_command ~timeout_seconds:60 command with
          | Error err -> failure ("Error executing command: " ^ err)
          | Ok (output, exit_code, duration_ms) ->
              let content = Printf.sprintf "Command output:\n%s\nExit status: %d" (truncate_output output) exit_code in
              if exit_code = 0 then success ~status_code:exit_code ~duration_ms content
              else failure ~status_code:exit_code ~duration_ms ~error_type:"command_failed" content)

let read_file_tool =
  make_tool "read_file" "Read the contents of a file."
    [("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to read")])]
    (fun args ->
      match required_string_arg (json_assoc_or_empty args) "path" with
      | Error err -> failure err
      | Ok path ->
          match read_file_full path with
          | Error err -> failure ("Error reading file: " ^ err)
          | Ok content -> success ~status_code:200 content)

let write_file_tool =
  make_tool "write_file" "Write content to a file (atomic writes)."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to write")]);
      ("content", `Assoc [("type", `String "string"); ("description", `String "Content to write")]);
    ]
    (fun args ->
      let json = json_assoc_or_empty args in
      match required_string_arg json "path", required_string_arg json "content" with
      | Error err, _ | _, Error err -> failure err
      | Ok path, Ok content ->
          match write_file_atomic path content with
          | Error err -> failure ("Error writing file: " ^ err)
          | Ok () -> success ~status_code:200 (Printf.sprintf "Successfully wrote %d bytes to %s" (String.length content) path))

let edit_file_tool =
  make_tool "edit_file" "Edit a file by replacing one exact text block."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to edit")]);
      ("old_text", `Assoc [("type", `String "string"); ("description", `String "Exact text to find and replace")]);
      ("new_text", `Assoc [("type", `String "string"); ("description", `String "New text to replace with")]);
    ]
    (fun args ->
      let json = json_assoc_or_empty args in
      match required_string_arg json "path", required_string_arg json "old_text", required_string_arg json "new_text" with
      | Error err, _, _ | _, Error err, _ | _, _, Error err -> failure err
      | Ok path, Ok old_text, Ok new_text ->
          match read_file_full path with
          | Error err -> failure ("Error reading file: " ^ err)
          | Ok content ->
              let len = String.length old_text in
              let rec find_sub s sub start =
                if start > String.length s - String.length sub then -1
                else if String.sub s start (String.length sub) = sub then start
                else find_sub s sub (start + 1)
              in
              match find_sub content old_text 0 with
              | -1 -> failure "old_text not found in file"
              | idx ->
                  let prefix = String.sub content 0 idx in
                  let suffix = String.sub content (idx + len) (String.length content - idx - len) in
                  let new_content = prefix ^ new_text ^ suffix in
                  match write_file_atomic path new_content with
                  | Error err -> failure ("Error writing file: " ^ err)
                  | Ok () -> success ~status_code:200 "File edited successfully")

let create_default_registry () =
  { tools = [bash_tool; read_file_tool; write_file_tool; edit_file_tool] }

let definitions _registry = List.map (fun t -> t.definition) [bash_tool; read_file_tool; write_file_tool; edit_file_tool]

let execute registry ~chat_id name input =
  match List.find_opt (fun tool -> tool.definition.Llm_types.name = name) registry.tools with
  | None -> failure ("Tool not found: " ^ name)
  | Some tool ->
      begin try tool.execute ~chat_id (json_assoc_or_empty input)
      with exn -> failure (Printf.sprintf "Error executing tool %s: %s" name (Printexc.to_string exn)) end
