(** Tools module for nanobot - web search, file operations, etc. *)

open Http_client
open Yojson.Basic.Util

(* Tool definition type *)
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * Yojson.Basic.t) list;
  execute : string -> string -> string
}

(* Web search tool *)
let web_search_tool = {
  name = "web_search";
  description = "Search the web for information";
  parameters = [
    "query", `String "The search query"
  ];
  execute = fun _ query ->
    (* In a real implementation, this would call a search API *)
    Printf.sprintf "Search results for: %s\n\n[Mock results - would call real API in production]\n1. Result 1\n2. Result 2\n3. Result 3" query
}

(* File read tool *)
let file_read_tool = {
  name = "read_file";
  description = "Read the contents of a file";
  parameters = [
    "path", `String "The file path"
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
  description = "Execute a shell command";
  parameters = [
    "command", `String "The command to execute"
  ];
  execute = fun _ cmd ->
    try
      let channel = Unix.open_process_in cmd in
      let output = really_input_string channel (in_channel_length channel) in
      let status = Unix.close_process_in channel in
      let exit_code = match status with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> -signal
        | Unix.WSTOPPED signal -> -signal
      in
      Printf.sprintf "Command output:\n%s\n\nExit status: %d" output exit_code
    with exn ->
      Printf.sprintf "Error executing command: %s" (Printexc.to_string exn)
}

(* Tool registry *)
let tool_registry = ref []

(* Register a tool *)
let register_tool tool =
  tool_registry := tool :: !tool_registry

(* Get tool by name *)
let get_tool name =
  List.find_opt (fun tool -> tool.name = name) !tool_registry

(* Execute a tool call *)
let execute_tool tool_name arguments =
  match get_tool tool_name with
  | Some tool ->
      (* Parse arguments - simple implementation for now *)
      let arg_string = match arguments with
        | `Assoc args ->
            List.map (fun (k, v) -> Printf.sprintf "%s=%s" k (to_string v)) args |> String.concat ", "
        | _ -> to_string arguments
      in
      tool.execute tool_name arg_string
  | None ->
      Printf.sprintf "Tool %s not found" tool_name

(* Initialize default tools *)
let init_default_tools () =
  register_tool web_search_tool;
  register_tool file_read_tool;
  register_tool shell_tool

(* Convert tools to JSON for LLM API *)
let tools_to_json () =
  let tool_list = List.map (fun tool ->
    `Assoc [
      ("type", `String "function");
      ("function", `Assoc [
        ("name", `String tool.name);
        ("description", `String tool.description);
        ("parameters", `Assoc (
          List.map (fun (name, schema) -> (name, schema)) tool.parameters
        ))
      ])
    ]
  ) !tool_registry in
  `List tool_list

(* Parse tool calls from LLM response *)
let parse_tool_calls json =
  try
    let choices = json |> member "choices" |> to_list in
    if choices <> [] then
      let first_choice = List.hd choices in
      let message = first_choice |> member "message" in
      try
        let tool_calls = message |> member "tool_calls" in
        Some (tool_calls |> to_list)
      with _ -> None
    else
      None
  with _ -> None

(* Extract tool call arguments *)
let extract_tool_arguments tool_call =
  try
    let arguments_json = tool_call |> member "function" |> member "arguments" in
    Some (to_string arguments_json)
  with _ -> None