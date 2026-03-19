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

type tool_exec_fn = pool:Domainslib.Task.pool -> chat_id:int -> Yojson.Safe.t -> tool_result

type tool = {
  definition : Llm_types.tool_definition;
  execute : tool_exec_fn;
}

type t = {
  tools : tool list;
  pool : Domainslib.Task.pool;
}

let close registry =
  Domainslib.Task.teardown_pool registry.pool

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
  { definition; execute = fun ~pool:_ ~chat_id:_ args -> execute args }

let required_string_arg json name =
  match Yojson.Safe.Util.member name json with
  | `String value when trim value <> "" -> Ok value
  | _ -> Error (name ^ " is required")

let json_assoc_or_empty = function | `Assoc fields -> `Assoc fields | _ -> `Assoc []

let truncate_output output =
  if String.length output > 4096 then String.sub output 0 4096 ^ "\n... (output truncated)" else output

let run_command ~pool:_ ~timeout_seconds command =
  let start_time = Unix.gettimeofday () in
  let deadline = start_time +. float_of_int timeout_seconds in
  
  (* Use open_process_in which is domain-safe *)
  let cmd = Printf.sprintf "%s 2>&1" command in
  let ic = Unix.open_process_in cmd in
  let fd = Unix.descr_of_in_channel ic in
  
  (* Create iomux poller for this operation - it's lightweight, no cleanup needed *)
  let poller = Iomux.Poll.create ~maxfds:1 () in
  Iomux.Poll.set_index poller 0 fd Iomux.Poll.Flags.(pollin + pollpri);
  
  (* Read output using iomux for timeout-aware reading *)
  let output = Buffer.create 4096 in
  let timed_out = ref false in
  
  (* Read loop with timeout *)
  begin
    try
      while not !timed_out do
        let remaining_ms = int_of_float ((deadline -. Unix.gettimeofday ()) *. 1000.0) in
        if remaining_ms <= 0 then
          timed_out := true
        else
          (* Poll with timeout *)
          let timeout : Iomux.Poll.poll_timeout = Iomux.Poll.Milliseconds (max 0 remaining_ms) in
          let nready = Iomux.Poll.poll poller 1 timeout in
          if nready = 0 then
            (* No events - check if we've exceeded deadline *)
            if Unix.gettimeofday () >= deadline then
              timed_out := true
          else
            (* Data available, read one line *)
            let line = input_line ic in
            Buffer.add_string output line;
            Buffer.add_char output '\n'
      done
    with End_of_file ->
      (* Process finished *)
      ()
  end;
  
  (* Get exit status *)
  let status = Unix.close_process_in ic in
  let end_time = Unix.gettimeofday () in
  let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
  let exit_code = match status with Unix.WEXITED c -> c | _ -> -1 in
  
  if !timed_out then
    Error (Printf.sprintf "Command timed out after %d seconds" timeout_seconds)
  else
    Ok (Buffer.contents output, exit_code, duration_ms)

let bash_tool =
  {
    definition = {
      Llm_types.name = "bash";
      description = "Execute a shell command.";
      input_schema = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [("command", `Assoc [("type", `String "string")])]);
        ("required", `List [`String "command"]);
      ];
    };
    execute = fun ~pool ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "command" with
      | Error err -> failure err
      | Ok command ->
          match run_command ~pool ~timeout_seconds:60 command with
          | Error err -> failure ("Error executing command: " ^ err)
          | Ok (output, exit_code, duration_ms) ->
              let content = Printf.sprintf "Command output:\n%s\nExit status: %d" (truncate_output output) exit_code in
              if exit_code = 0 then success ~status_code:exit_code ~duration_ms content
              else failure ~status_code:exit_code ~duration_ms ~error_type:"command_failed" content
  }

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

(** Search for [sub] in [s] starting at [start] using character-by-character comparison.
    Returns the index of first match, or -1 if not found.
    This is O(1) in allocations - no intermediate strings created. *)
let find_sub_no_alloc s sub start =
  let s_len = String.length s in
  let sub_len = String.length sub in
  if sub_len = 0 || start > s_len - sub_len then -1
  else
    let rec search i =
      if i > s_len - sub_len then -1
      else
        (* Check if sub matches at position i *)
        let rec check_match j =
          if j >= sub_len then true
          else if String.unsafe_get s (i + j) <> String.unsafe_get sub j then false
          else check_match (j + 1)
        in
        if check_match 0 then i
        else search (i + 1)
    in
    search start

let edit_file_tool =
  make_tool "edit_file" "Edit a file by replacing one exact text block. The old_text must be unique in the file."
    [
      ("path", `Assoc [("type", `String "string"); ("description", `String "Path to the file to edit")]);
      ("old_text", `Assoc [("type", `String "string"); ("description", `String "Exact text to find and replace (must be unique in file)")]);
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
              (* First search: find the initial match *)
              match find_sub_no_alloc content old_text 0 with
              | -1 -> failure "old_text not found in file"
              | first_idx ->
                  (* Second search: check for ambiguity starting from next character *)
                  match find_sub_no_alloc content old_text (first_idx + 1) with
                  | second_idx when second_idx >= 0 ->
                      failure (Printf.sprintf "Ambiguity error: 'old_text' occurs multiple times (at positions %d and %d). Please provide more context." first_idx second_idx)
                  | _ ->
                      (* Unique match found - perform replacement *)
                      let old_len = String.length old_text in
                      let prefix = String.sub content 0 first_idx in
                      let suffix = String.sub content (first_idx + old_len) (String.length content - first_idx - old_len) in
                      let new_content = prefix ^ new_text ^ suffix in
                      match write_file_atomic path new_content with
                      | Error err -> failure ("Error writing file: " ^ err)
                      | Ok () -> success ~status_code:200 "File edited successfully")

let create_default_registry () =
  let pool = Domainslib.Task.setup_pool ~num_domains:2 () in
  { pool; tools = [bash_tool; read_file_tool; write_file_tool; edit_file_tool] }

let definitions _registry = List.map (fun t -> t.definition) [bash_tool; read_file_tool; write_file_tool; edit_file_tool]

let execute registry ~chat_id name input =
  match List.find_opt (fun tool -> tool.definition.Llm_types.name = name) registry.tools with
  | None -> failure ("Tool not found: " ^ name)
  | Some tool ->
      begin try tool.execute ~pool:registry.pool ~chat_id (json_assoc_or_empty input)
      with exn -> failure (Printf.sprintf "Error executing tool %s: %s" name (Printexc.to_string exn)) end
