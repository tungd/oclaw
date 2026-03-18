(** Minimal CLI tools: read_file, write_file, edit_file, bash. No sandbox restrictions. *)

open Yojson.Safe
open Yojson.Safe.Util

type web_config = {
  request_timeout_seconds : int;
  fetch_max_bytes : int;
  search_max_results : int;
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
  web : web_config;
  data_dir : string;
  skills_dir : string;
  db : Db.t;
  skills : Skills.t;
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
    let dir = Filename.dirname path in
    let tmp = Filename.concat dir (Printf.sprintf ".tmp-oclaw-%d-%06d"
      (int_of_float (Unix.gettimeofday ())) (Random.int 1_000_000)) in
    Stdlib.Out_channel.with_open_bin tmp (fun channel -> output_string channel content);
    Unix.rename tmp path;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let json_assoc_or_empty = function | `Assoc _ as json -> json | _ -> `Assoc []

let required_string_arg json name =
  match Yojson.Safe.Util.member name json with
  | `String value when trim value <> "" -> Ok value
  | _ -> Error (name ^ " is required")

let string_arg json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Some value
  | `Int value -> Some (string_of_int value)
  | `Intlit value -> Some value
  | `Float value -> Some (string_of_float value)
  | `Bool value -> Some (string_of_bool value)
  | _ -> None

let make_tool name description schema execute =
  { definition = { Llm_types.name; description; input_schema = schema }; execute }

let schema fields required =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc fields);
    ("required", `List (List.map (fun s -> `String s) required));
  ]

(* ============================================================================
   Core Tools
   ============================================================================ *)

let read_file_tool =
  make_tool "read_file" "Read and return the contents of a file."
    (schema [ ("path", `Assoc [ ("type", `String "string") ]) ] [ "path" ])
    (fun ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "path" with
      | Error err -> failure err
      | Ok path ->
          match read_file_full path with
          | Ok content -> success content
          | Error err -> failure ("Error reading file: " ^ err))

let write_file_tool =
  make_tool "write_file" "Write content to a file."
    (schema [
      ("path", `Assoc [ ("type", `String "string") ]);
      ("content", `Assoc [ ("type", `String "string") ]);
    ] [ "path"; "content" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "path", required_string_arg args "content" with
      | Error err, _ | _, Error err -> failure err
      | Ok path, Ok content ->
          match write_file_atomic path content with
          | Ok () -> success ("File written: " ^ path)
          | Error err -> failure ("Error writing file: " ^ err))

let edit_file_tool =
  make_tool "edit_file" "Edit a file by replacing one exact text block with another."
    (schema [
      ("path", `Assoc [ ("type", `String "string") ]);
      ("old_text", `Assoc [ ("type", `String "string") ]);
      ("new_text", `Assoc [ ("type", `String "string") ]);
    ] [ "path"; "old_text"; "new_text" ])
    (fun ~chat_id:_ args ->
      let args = json_assoc_or_empty args in
      match required_string_arg args "path", required_string_arg args "old_text", required_string_arg args "new_text" with
      | Error err, _, _ | _, Error err, _ | _, _, Error err -> failure err
      | Ok path, Ok old_text, Ok new_text ->
          match read_file_full path with
          | Error err -> failure ("Error editing file: " ^ err)
          | Ok content ->
              let count = ref 0 and index = ref None and start = ref 0 in
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
                let replaced = String.sub content 0 idx ^ new_text ^
                  String.sub content (idx + String.length old_text)
                    (String.length content - idx - String.length old_text) in
                match write_file_atomic path replaced with
                | Ok () -> success ("File edited: " ^ path)
                | Error err -> failure ("Error editing file: " ^ err))

let truncate_output output =
  let max_len = 10000 in
  if String.length output <= max_len then output
  else String.sub output 0 max_len ^ "\n... (truncated)"

let quote_shell_arg arg =
  (* Simple shell quoting - wrap in single quotes and escape any single quotes inside *)
  let escape s =
    let b = Buffer.create (String.length s + 10) in
    String.iter (function '\'' -> Buffer.add_string b "'\\''" | c -> Buffer.add_char b c) s;
    Buffer.contents b
  in
  "'" ^ escape arg ^ "'"

let run_command ~timeout_seconds command =
  try
    let start_time = Unix.gettimeofday () in
    let cmd = "bash -c " ^ quote_shell_arg command in
    let (output, status) =
      Unix.open_process_in cmd
      |> fun chan ->
          let buf = Buffer.create 1024 in
          (try
             while true do
               Buffer.add_channel buf chan 1
             done
           with End_of_file -> ());
          let status = Unix.close_process_in chan in
          (Buffer.contents buf, status)
    in
    let end_time = Unix.gettimeofday () in
    let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
    let exit_code = match status with Unix.WEXITED c -> c | _ -> 1 in
    Ok (output, exit_code, duration_ms)
  with exn -> Error (Printexc.to_string exn)

let bash_tool =
  make_tool "bash" "Execute a shell command."
    (schema [ ("command", `Assoc [ ("type", `String "string") ]) ] [ "command" ])
    (fun ~chat_id:_ args ->
      match required_string_arg (json_assoc_or_empty args) "command" with
      | Error err -> failure err
      | Ok command ->
          match run_command ~timeout_seconds:60 command with
          | Error err -> failure ("Error executing command: " ^ err)
          | Ok (output, exit_code, duration_ms) ->
              let content = Printf.sprintf "Command output:\n%s\nExit status: %d" (truncate_output output) exit_code in
              if exit_code = 0 then success ~status_code:exit_code ~duration_ms content
              else failure ~status_code:exit_code ~duration_ms ~error_type:"command_failed" content)

(* ============================================================================
   Stub Tools (to be implemented)
   ============================================================================ *)

let todo_read_tool _db =
  make_tool "todo_read" "Read todos."
    (schema [] [])
    (fun ~chat_id:_ _ -> failure "todo_read not yet implemented")

let todo_write_tool _db =
  make_tool "todo_write" "Write todos."
    (schema [ ("todos", `Assoc [ ("type", `String "array") ]) ] [ "todos" ])
    (fun ~chat_id:_ _ -> failure "todo_write not yet implemented")

let activate_skill_tool _skills =
  make_tool "activate_skill" "Activate a skill."
    (schema [ ("name", `Assoc [ ("type", `String "string") ]) ] [ "name" ])
    (fun ~chat_id:_ _ -> failure "activate_skill not yet implemented")

let sync_skills_tool _skills =
  make_tool "sync_skills" "Sync skills."
    (schema [] [])
    (fun ~chat_id:_ _ -> failure "sync_skills not yet implemented")

let schedule_task_tool _db =
  make_tool "schedule_task" "Schedule a task."
    (schema [
      ("prompt", `Assoc [ ("type", `String "string") ]);
      ("run_at", `Assoc [ ("type", `String "string") ]);
    ] [ "prompt" ])
    (fun ~chat_id:_ _ -> failure "schedule_task not yet implemented")

let list_scheduled_tasks_tool _db =
  make_tool "list_scheduled_tasks" "List scheduled tasks."
    (schema [] [])
    (fun ~chat_id:_ _ -> failure "list_scheduled_tasks not yet implemented")

let pause_scheduled_task_tool _db =
  make_tool "pause_scheduled_task" "Pause a scheduled task."
    (schema [ ("task_id", `Assoc [ ("type", `String "integer") ]) ] [ "task_id" ])
    (fun ~chat_id:_ _ -> failure "pause_scheduled_task not yet implemented")

let resume_scheduled_task_tool _db =
  make_tool "resume_scheduled_task" "Resume a scheduled task."
    (schema [ ("task_id", `Assoc [ ("type", `String "integer") ]) ] [ "task_id" ])
    (fun ~chat_id:_ _ -> failure "resume_scheduled_task not yet implemented")

let cancel_scheduled_task_tool _db =
  make_tool "cancel_scheduled_task" "Cancel a scheduled task."
    (schema [ ("task_id", `Assoc [ ("type", `String "integer") ]) ] [ "task_id" ])
    (fun ~chat_id:_ _ -> failure "cancel_scheduled_task not yet implemented")

let get_task_history_tool _db =
  make_tool "get_task_history" "Get task history."
    (schema [ ("task_id", `Assoc [ ("type", `String "integer") ]) ] [ "task_id" ])
    (fun ~chat_id:_ _ -> failure "get_task_history not yet implemented")

let export_chat_tool _db =
  make_tool "export_chat" "Export chat to markdown."
    (schema [ ("path", `Assoc [ ("type", `String "string") ]) ] [])
    (fun ~chat_id:_ _ -> failure "export_chat not yet implemented")

(* ============================================================================
   Tool Registry
   ============================================================================ *)

let default_web_config = {
  request_timeout_seconds = 20;
  fetch_max_bytes = 20000;
  search_max_results = 5;
}

let create_default_registry ~data_dir ~skills_dir ~db () =
  Random.self_init ();
  let skills = Skills.create ~skills_dir in
  let tools = [
    bash_tool;
    read_file_tool;
    write_file_tool;
    edit_file_tool;
    (* Web tools *)
    (let web = default_web_config in
     make_tool "web_search" "Search the web."
       (schema [ ("query", `Assoc [ ("type", `String "string") ]) ] [ "query" ])
       (fun ~chat_id:_ _ -> failure "web_search not yet implemented"));
    (let web = default_web_config in
     make_tool "web_fetch" "Fetch a URL."
       (schema [ ("url", `Assoc [ ("type", `String "string") ]) ] [ "url" ])
       (fun ~chat_id:_ _ -> failure "web_fetch not yet implemented"));
    (* Task tools *)
    todo_read_tool db;
    todo_write_tool db;
    activate_skill_tool skills;
    sync_skills_tool skills;
    schedule_task_tool db;
    list_scheduled_tasks_tool db;
    pause_scheduled_task_tool db;
    resume_scheduled_task_tool db;
    cancel_scheduled_task_tool db;
    get_task_history_tool db;
    export_chat_tool db;
  ] in
  { web = default_web_config; data_dir; skills_dir; db; skills; tools }

let definitions registry = List.map (fun tool -> tool.definition) registry.tools

let find_tool registry name =
  List.find_opt (fun tool -> String.equal tool.definition.name name) registry.tools

let execute registry ~chat_id name input =
  match find_tool registry name with
  | Some tool ->
      begin try tool.execute ~chat_id (json_assoc_or_empty input)
      with exn -> failure (Printf.sprintf "Error executing tool %s: %s" name (Printexc.to_string exn)) end
  | None -> failure ("Tool not found: " ^ name)

let active_registry = ref None

let fallback_db data_dir =
  let path = Filename.concat (Filename.concat data_dir "runtime") "default-tools.db" in
  match Db.create path with | Ok db -> db | Error err -> failwith err

let init_default_tools ?(data_dir="workspace") ?skills_dir ?db () =
  let skills_dir = Option.value ~default:(Filename.concat data_dir "skills") skills_dir in
  let db = Option.value ~default:(fallback_db data_dir) db in
  let registry = create_default_registry ~data_dir ~skills_dir ~db () in
  active_registry := Some registry

let with_active_registry f =
  match !active_registry with
  | Some registry -> f registry
  | None -> init_default_tools (); match !active_registry with Some registry -> f registry | None -> failwith "tool registry init failed"

let get_all_tools () =
  with_active_registry (fun registry ->
    List.map (fun tool -> (tool.definition.name, tool.definition.description)) registry.tools)

let execute_tool ?(chat_id=1) name args =
  with_active_registry (fun registry ->
    let result = execute registry ~chat_id name args in
    result.content)

let tools_to_json () =
  with_active_registry (fun registry ->
    `List (List.map (fun tool ->
      `Assoc [
        ("name", `String tool.definition.name);
        ("description", `String tool.definition.description);
        ("schema", tool.definition.input_schema);
      ]) registry.tools))
