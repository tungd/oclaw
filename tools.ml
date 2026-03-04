(** Tools module for OClaw - web search, file operations, etc. *)

open Http_client
open Yojson.Safe
open Yojson.Safe.Util

(* Tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Safe.t) list;
  execute : string -> string -> string
}

(* Web search tool *)
let web_search_tool = {
  name = "web_search";
  description = "Search the web for current information on any topic";
  parameters = [
    "query", `String "string: The search query to look up"
  ];
  execute = fun _ query ->
    (* In a real implementation, this would call a search API *)
    Printf.sprintf "Search results for: %s\n\n[Mock results - would call real API in production]\n1. Result 1\n2. Result 2\n3. Result 3" query
}

(* File read tool *)
let file_read_tool = {
  name = "read_file";
  description = "Read and return the complete contents of a file from the filesystem";
  parameters = [
    "path", `String "string: The absolute or relative path to the file to read"
  ];
  execute = fun _ path ->
    try
      let channel = open_in path in
      let content = really_input_string channel (in_channel_length channel) in
      close_in channel;
      content
    with exn ->
      Printf.sprintf "Error reading file %s: %s" path (Printexc.to_string exn)
}

(* Shell command tool *)
let shell_tool = {
  name = "execute_command";
  description = "Execute a shell command in the current directory and return the output";
  parameters = [
    "command", `String "string: The shell command to execute (e.g., 'ls -la', 'pwd', 'echo hello')"
  ];
  execute = fun _ cmd ->
    try
      let channel = Unix.open_process_in cmd in
      let output = Buffer.create 1024 in
      (try
        while true do
          let line = input_line channel in
          Buffer.add_string output line;
          Buffer.add_char output '\n'
        done
      with End_of_file -> ());
      let status = Unix.close_process_in channel in
      let exit_code = match status with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> -signal
        | Unix.WSTOPPED signal -> -signal
      in
      Printf.sprintf "Command output:\n%s\nExit status: %d" (Buffer.contents output) exit_code
    with exn ->
      Printf.sprintf "Error executing command: %s" (Printexc.to_string exn)
}

(* Directory list tool - added to answer "what files are in current directory" *)
let list_directory_tool = {
  name = "list_directory";
  description = "List files and directories in the current or specified path";
  parameters = [
    "path", `String "string: The directory path to list (default: current directory)"
  ];
  execute = fun _ path ->
    let cmd = if path = "" || path = "." then "ls -la" else "ls -la " ^ path in
    try
      let channel = Unix.open_process_in cmd in
      let output = Buffer.create 1024 in
      (try
        while true do
          let line = input_line channel in
          Buffer.add_string output line;
          Buffer.add_char output '\n'
        done
      with End_of_file -> ());
      let status = Unix.close_process_in channel in
      let exit_code = match status with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> -signal
        | Unix.WSTOPPED signal -> -signal
      in
      Printf.sprintf "Directory listing for %s:\n%s\nExit status: %d"
        (if path = "" || path = "." then "current directory" else path)
        (Buffer.contents output) exit_code
    with exn ->
      Printf.sprintf "Error listing directory: %s" (Printexc.to_string exn)
}

(* Tool registry *)
let tool_registry = ref []

(* Register a tool *)
let register_tool tool =
  tool_registry := tool :: !tool_registry

(* Get tool by name *)
let get_tool name =
  List.find_opt (fun tool -> tool.name = name) !tool_registry

(* Get all tools as (name, description) list *)
let get_all_tools () =
  List.map (fun tool -> (tool.name, tool.description)) !tool_registry

(* Execute a tool call *)
let execute_tool tool_name arguments =
  match get_tool tool_name with
  | Some tool ->
      (* Parse arguments from JSON *)
      let arg_string = match arguments with
        | `Assoc args ->
            (* Extract parameter values - for single param tools, just get the value *)
            if List.length args = 1 then
              (* Single parameter - return just the value *)
              Yojson.Safe.Util.to_string (snd (List.hd args))
            else
              (* Multiple parameters - could be enhanced to pass named args *)
              List.map (fun (k, v) -> Printf.sprintf "%s=%s" k (Yojson.Safe.Util.to_string v)) args |> String.concat ", "
        | `String s -> s
        | _ -> Yojson.Safe.Util.to_string arguments
      in
      tool.execute tool_name arg_string
  | None ->
      Printf.sprintf "Tool %s not found" tool_name

(* Initialize default tools *)
let init_default_tools () =
  register_tool web_search_tool;
  register_tool file_read_tool;
  register_tool shell_tool;
  register_tool list_directory_tool

(* Convert tools to JSON for LLM API - OpenAI format *)
let tools_to_json () =
  let tool_list = List.map (fun tool ->
    (* Build JSON Schema for parameters *)
    let parameters_obj = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc (
        List.map (fun (name, schema) ->
          (* Each parameter has type and description *)
          match schema with
          | `String desc ->
              (* Parse "type: description" format if present *)
              let parts = String.split_on_char ':' desc in
              if List.length parts >= 2 then
                let param_type = String.trim (List.hd (List.rev parts)) in
                let param_desc = String.trim (String.concat ":" (List.rev (List.tl (List.rev parts)))) in
                (name, `Assoc [
                  ("type", `String param_type);
                  ("description", `String param_desc)
                ])
              else
                (name, `Assoc [
                  ("type", `String "string");
                  ("description", `String desc)
                ])
          | _ -> (name, `Assoc [("type", `String "string"); ("description", `String "")])
        ) tool.parameters
      ));
      ("required", `List (
        List.map (fun (name, _) -> `String name) tool.parameters
      ))
    ] in
    `Assoc [
      ("type", `String "function");
      ("function", `Assoc [
        ("name", `String tool.name);
        ("description", `String tool.description);
        ("parameters", parameters_obj)
      ])
    ]
  ) !tool_registry in
  `List tool_list

(* Parse tool calls from LLM response *)
let parse_tool_calls json =
  try
    let choices = Yojson.Safe.Util.member "choices" json in
    let choices_list = Yojson.Safe.Util.to_list choices in
    if choices_list <> [] then
      let first_choice = List.hd choices_list in
      let message = Yojson.Safe.Util.member "message" first_choice in
      try
        let tool_calls = Yojson.Safe.Util.member "tool_calls" message in
        Some (Yojson.Safe.Util.to_list tool_calls)
      with _ -> None
    else
      None
  with _ -> None

(* Extract tool call arguments *)
let extract_tool_arguments tool_call =
  try
    let function_ = Yojson.Safe.Util.member "function" tool_call in
    let arguments_json = Yojson.Safe.Util.member "arguments" function_ in
    Some (Yojson.Safe.Util.to_string arguments_json)
  with _ -> None
