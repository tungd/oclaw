module Yaml_lib = Yaml

type locator =
  | Local of string
  | Remote of string

type tool_language =
  | Bash
  | Python
  | Javascript

type embedded_tool = {
  name : string;
  description : string;
  language : tool_language;
  code : string;
}

type document = {
  name : string;
  description : string option;
  uses : (string * string) list;
  prompt : string;
  tools : embedded_tool list;
  locator : locator;
}

type resolved = {
  document : document;
  children : (string * resolved) list;
}

type lock_entry = {
  alias : string;
  source : string;
  resolved : string;
  digest : string;
}

let max_dependency_depth = 8

let yaml_assoc = function
  | `O fields -> fields
  | _ -> []

let yaml_string = function
  | `String value -> Some value
  | `Float value -> Some (string_of_float value)
  | `Bool value -> Some (string_of_bool value)
  | `Null -> Some ""
  | _ -> None

let locator_to_string = function
  | Local path -> path
  | Remote url -> url

let is_remote_source source =
  String.starts_with ~prefix:"http://" source
  || String.starts_with ~prefix:"https://" source
  || String.starts_with ~prefix:"file://" source

let trim = String.trim

let normalize_local_path path =
  let absolute =
    if Filename.is_relative path then
      Filename.concat (Sys.getcwd ()) path
    else
      path
  in
  if Sys.file_exists absolute then Unix.realpath absolute else absolute

let lock_path path =
  if String.ends_with ~suffix:".md" path then
    String.sub path 0 (String.length path - 3) ^ ".lock.json"
  else
    path ^ ".lock.json"

let remote_lock_url url =
  if String.ends_with ~suffix:".md" url then
    String.sub url 0 (String.length url - 3) ^ ".lock.json"
  else
    url ^ ".lock.json"

let read_file path =
  try Ok (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with exn -> Error (Printexc.to_string exn)

let fetch_url url =
  let request = H1.Request.create `GET url in
  match Httpkit.Client.execute_request ~timeout:30 request with
  | Error error -> Error error
  | Ok (response, body) ->
      let code = H1.Status.to_code response.H1.Response.status in
      if code >= 200 && code < 300 then Ok body
      else Error (Printf.sprintf "Failed to fetch %s (status %d)" url code)

let digest_string content =
  Digest.string content |> Digest.to_hex

let split_once text needle =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop idx =
    if idx > text_len - needle_len then
      None
    else if String.sub text idx needle_len = needle then
      Some
        ( String.sub text 0 idx,
          String.sub text (idx + needle_len) (text_len - idx - needle_len) )
    else
      loop (idx + 1)
  in
  if needle_len = 0 then Some ("", text) else loop 0

let normalize_segments segments =
  let rec loop acc = function
    | [] -> List.rev acc
    | "" :: rest
    | "." :: rest -> loop acc rest
    | ".." :: rest ->
        let acc =
          match acc with
          | [] -> []
          | _ :: tail -> tail
        in
        loop acc rest
    | segment :: rest -> loop (segment :: acc) rest
  in
  loop [] segments

let resolve_relative_url ~base rel =
  if is_remote_source rel then
    rel
  else
    let scheme_idx =
      match split_once base "://" with
      | Some (scheme, rest) -> Some (scheme, rest)
      | None -> None
    in
    match scheme_idx with
    | None -> rel
    | Some (scheme, rest) ->
        let host, path =
          match String.index_opt rest '/' with
          | Some idx ->
              ( String.sub rest 0 idx,
                String.sub rest idx (String.length rest - idx) )
          | None -> (rest, "/")
        in
        let origin = scheme ^ "://" ^ host in
        if String.starts_with ~prefix:"/" rel then
          origin ^ "/" ^ String.concat "/" (normalize_segments (String.split_on_char '/' rel))
        else
          let base_dir =
            let parts = String.split_on_char '/' path in
            match List.rev parts with
            | _file :: rest -> List.rev rest
            | [] -> [""]
          in
          let combined = base_dir @ String.split_on_char '/' rel |> normalize_segments in
          origin ^ "/" ^ String.concat "/" combined

let validate_identifier kind value =
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  if String.length value > 0 && String.for_all is_valid_char value then
    true
  else
    failwith (Printf.sprintf "invalid %s %S" kind value)

let ensure_identifier kind value =
  try
    let _ = validate_identifier kind value in
    Ok value
  with Failure err ->
    Error err

let parse_frontmatter content =
  if not (String.starts_with ~prefix:"---\n" content) then
    Error "Agent file is missing YAML front matter"
  else
    let rest = String.sub content 4 (String.length content - 4) in
    match split_once rest "\n---\n" with
    | None -> Error "Agent file front matter is missing a closing ---"
    | Some (yaml_text, body) ->
        begin
          match Yaml_lib.of_string yaml_text with
          | Error (`Msg msg) -> Error msg
          | Ok yaml ->
              let fields = yaml_assoc yaml in
              let lookup key =
                match List.assoc_opt key fields with
                | Some value -> yaml_string value
                | None -> None
              in
              let uses =
                match List.assoc_opt "uses" fields with
                | None -> Ok []
                | Some (`O entries) ->
                    let seen = Hashtbl.create 8 in
                    let rec loop acc = function
                      | [] -> Ok (List.rev acc)
                      | (alias, value) :: rest ->
                          if Hashtbl.mem seen alias then
                            Error (Printf.sprintf "Duplicate uses alias: %s" alias)
                          else
                            begin
                              Hashtbl.add seen alias ();
                              match ensure_identifier "uses alias" alias, yaml_string value with
                              | Error err, _ -> Error err
                              | _, None ->
                                  Error (Printf.sprintf "Use %s must be a string source" alias)
                              | Ok alias, Some source when trim source <> "" ->
                                  loop ((alias, source) :: acc) rest
                              | Ok alias, _ ->
                                  Error (Printf.sprintf "Use %s must not be empty" alias)
                            end
                    in
                    loop [] entries
                | Some _ -> Error "uses must be a YAML object of alias -> source"
              in
              begin
                match lookup "name", uses with
                | Some name, Ok uses when trim name <> "" ->
                    Ok (name, lookup "description", uses, trim body)
                | Some _, Error err -> Error err
                | _ -> Error "name is required"
              end
        end

let is_top_level_heading line =
  String.starts_with ~prefix:"# " line

let is_second_level_heading line =
  String.starts_with ~prefix:"## " line

let is_fence line =
  String.starts_with ~prefix:"```" (trim line)

let split_prompt_and_tools body =
  let lines = String.split_on_char '\n' body in
  let rec loop prompt_rev tools_rev in_tools in_fence = function
    | [] -> (List.rev prompt_rev, List.rev tools_rev)
    | line :: rest ->
        let trimmed = trim line in
        let next_in_fence =
          if is_fence line then not in_fence else in_fence
        in
        if in_fence then
          if in_tools then loop prompt_rev (line :: tools_rev) in_tools next_in_fence rest
          else loop (line :: prompt_rev) tools_rev in_tools next_in_fence rest
        else if is_top_level_heading trimmed then
          if String.equal trimmed "# Tools" then
            loop prompt_rev tools_rev true next_in_fence rest
          else if in_tools then
            loop (line :: prompt_rev) tools_rev false next_in_fence rest
          else
            loop (line :: prompt_rev) tools_rev false next_in_fence rest
        else if in_tools then
          loop prompt_rev (line :: tools_rev) in_tools next_in_fence rest
        else
          loop (line :: prompt_rev) tools_rev in_tools next_in_fence rest
  in
  let prompt_lines, tools_lines = loop [] [] false false lines in
  (trim (String.concat "\n" prompt_lines), tools_lines)

let language_of_string = function
  | "bash" | "sh" -> Some Bash
  | "python" | "python3" -> Some Python
  | "javascript" | "js" -> Some Javascript
  | _ -> None

let parse_tool_block name lines =
  let description =
    let rec loop acc started = function
      | [] -> acc
      | line :: _ when is_fence line -> acc
      | line :: rest ->
          let stripped = trim line in
          if stripped = "" then
            if started then acc else loop acc false rest
          else
            let next = acc @ [ stripped ] in
            loop next true rest
    in
    match loop [] false lines with
    | [] -> Printf.sprintf "Execute embedded tool %s." name
    | parts -> String.concat " " parts
  in
  let rec find_code_block in_fence language current = function
    | [] -> Error (Printf.sprintf "Tool %s is missing an executable code block" name)
    | line :: rest ->
        let stripped = trim line in
        if in_fence then
          if is_fence stripped then
            begin
              match language with
              | Some language -> Ok (language, String.concat "\n" (List.rev current))
              | None -> find_code_block false None [] rest
            end
          else
            find_code_block true language (line :: current) rest
        else if is_fence stripped then
          let info =
            String.sub stripped 3 (String.length stripped - 3)
            |> trim
            |> String.lowercase_ascii
          in
          let language = language_of_string info in
          find_code_block true language [] rest
        else
          find_code_block false None [] rest
  in
  match find_code_block false None [] lines with
  | Error err -> Error err
  | Ok (language, code) -> Ok { name; description; language; code }

let parse_tools tools_lines =
  let rec collect current_name current_rev acc in_fence = function
    | [] ->
        begin
          match current_name with
          | None -> Ok (List.rev acc)
          | Some name ->
              Result.map (fun tool -> List.rev (tool :: acc)) (parse_tool_block name (List.rev current_rev))
        end
    | line :: rest ->
        let trimmed = trim line in
        let next_in_fence = if is_fence line then not in_fence else in_fence in
        if not in_fence && is_second_level_heading trimmed then
          let name = trim (String.sub trimmed 3 (String.length trimmed - 3)) in
          begin
            match ensure_identifier "tool name" name with
            | Error err -> Error err
            | Ok name ->
                begin
                  match current_name with
                  | None -> collect (Some name) [] acc next_in_fence rest
                  | Some current ->
                      begin
                        match parse_tool_block current (List.rev current_rev) with
                        | Error err -> Error err
                        | Ok tool -> collect (Some name) [] (tool :: acc) next_in_fence rest
                      end
                end
          end
        else
          collect current_name (line :: current_rev) acc next_in_fence rest
  in
  let tools =
    match collect None [] [] false tools_lines with
    | Error err -> Error err
    | Ok tools ->
        let seen = Hashtbl.create 8 in
        let rec dedupe acc = function
          | [] -> Ok (List.rev acc)
          | (tool : embedded_tool) :: rest ->
              if Hashtbl.mem seen tool.name then
                Error (Printf.sprintf "Duplicate tool name: %s" tool.name)
              else begin
                Hashtbl.add seen tool.name ();
                dedupe (tool :: acc) rest
              end
        in
        dedupe [] tools
  in
  tools

let parse_string ~locator content =
  match parse_frontmatter content with
  | Error err -> Error err
  | Ok (name, description, uses, body) ->
      let prompt, tools_lines = split_prompt_and_tools body in
      begin
        match parse_tools tools_lines with
        | Error err -> Error err
        | Ok tools ->
            Ok {
              name;
              description;
              uses;
              prompt;
              tools;
              locator;
            }
      end

let parse_lockfile body =
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields ->
      begin
        match List.assoc_opt "remote_uses" fields with
        | Some (`List entries) ->
            entries
            |> List.fold_left
                 (fun acc entry ->
                   match acc, entry with
                   | Error _ as err, _ -> err
                   | Ok acc, `Assoc fields ->
                       begin
                         match List.assoc_opt "alias" fields,
                               List.assoc_opt "source" fields,
                               List.assoc_opt "resolved" fields,
                               List.assoc_opt "digest" fields with
                         | Some (`String alias), Some (`String source), Some (`String resolved), Some (`String digest) ->
                             Ok ({ alias; source; resolved; digest } :: acc)
                         | _ -> Error "Invalid lockfile entry"
                       end
                   | Ok _, _ -> Error "Invalid lockfile entry")
                 (Ok [])
            |> Result.map List.rev
        | Some _ -> Error "remote_uses must be a list"
        | None -> Ok []
      end
  | _ -> Error "Lockfile must contain a top-level object"

let load_lockfile locator =
  let source =
    match locator with
    | Local path ->
        let path = lock_path path in
        if Sys.file_exists path then read_file path else Ok ""
    | Remote url ->
        let url = remote_lock_url url in
        fetch_url url
  in
  match source with
  | Error err -> Error err
  | Ok "" -> Ok []
  | Ok body -> parse_lockfile body

let resolve_child_locator parent_locator source =
  match parent_locator with
  | Local parent_path ->
      if is_remote_source source then
        Remote source
      else
        let base = Filename.dirname parent_path in
        Local (normalize_local_path (Filename.concat base source))
  | Remote parent_url ->
      if is_remote_source source then
        Remote source
      else
        Remote (resolve_relative_url ~base:parent_url source)

let find_lock_entry entries ~alias ~source =
  List.find_opt
    (fun entry -> String.equal entry.alias alias && String.equal entry.source source)
    entries

let load_content locator =
  match locator with
  | Local path -> read_file path
  | Remote url -> fetch_url url

let rec load_locator_internal ?(stack=[]) ?(depth=0) locator =
  let key = locator_to_string locator in
  if depth > max_dependency_depth then
    Error (Printf.sprintf "Agent dependency depth exceeded at %s" key)
  else if List.exists (String.equal key) stack then
    Error (Printf.sprintf "Agent dependency cycle detected at %s" key)
  else
    match load_content locator with
    | Error err -> Error err
    | Ok body ->
        begin
          match parse_string ~locator body with
          | Error err -> Error (Printf.sprintf "%s: %s" key err)
          | Ok document ->
              let remote_children =
                document.uses
                |> List.filter (fun (_alias, source) ->
                       match resolve_child_locator locator source with
                       | Remote _ -> true
                       | Local _ -> false)
              in
              let lock_entries =
                if remote_children = [] then Ok []
                else load_lockfile locator
              in
              begin
                match lock_entries with
                | Error err ->
                    Error (Printf.sprintf "Failed to load lockfile for %s: %s" key err)
                | Ok lock_entries ->
                    let rec load_children acc = function
                      | [] -> Ok (List.rev acc)
                      | (alias, source) :: rest ->
                          let base_locator = resolve_child_locator locator source in
                          let resolved_locator =
                            match base_locator with
                            | Local _ -> Ok base_locator
                            | Remote _ ->
                                begin
                                  match find_lock_entry lock_entries ~alias ~source with
                                  | None ->
                                      Error (Printf.sprintf "Remote use %s (%s) is not pinned in the lockfile" alias source)
                                  | Some entry -> Ok (Remote entry.resolved)
                                end
                          in
                          begin
                            match resolved_locator with
                            | Error err -> Error err
                            | Ok resolved_locator ->
                                match load_content resolved_locator with
                                | Error err -> Error err
                                | Ok child_body ->
                                    let child_digest = digest_string child_body in
                                    let digest_check =
                                      match base_locator with
                                      | Remote _ ->
                                          begin
                                            match find_lock_entry lock_entries ~alias ~source with
                                            | Some entry when String.equal entry.digest child_digest -> Ok ()
                                            | Some _ ->
                                                Error (Printf.sprintf "Digest mismatch for remote use %s" alias)
                                            | None ->
                                                Error (Printf.sprintf "Remote use %s (%s) is not pinned in the lockfile" alias source)
                                          end
                                      | Local _ -> Ok ()
                                    in
                                    begin
                                      match digest_check with
                                      | Error err -> Error err
                                      | Ok () ->
                                          begin
                                            match load_locator_internal ~stack:(key :: stack) ~depth:(depth + 1) resolved_locator with
                                            | Ok child -> load_children ((alias, child) :: acc) rest
                                            | Error err -> Error err
                                          end
                                    end
                          end
                    in
                    match load_children [] document.uses with
                    | Error err -> Error err
                    | Ok children -> Ok { document; children }
              end
        end

let load ~path =
  load_locator_internal (Local (normalize_local_path path))

let load_locator locator =
  load_locator_internal locator

let child_summary (alias, child) =
  let description =
    match child.document.description with
    | Some value when trim value <> "" -> value
    | _ -> child.document.name
  in
  Printf.sprintf "- `%s`: %s" alias description

let system_prompt resolved =
  let meta_lines =
    [
      Some ("Name: " ^ resolved.document.name);
      Option.map (fun description -> "Description: " ^ description) resolved.document.description;
    ]
    |> List.filter_map (fun value -> value)
  in
  let delegation_block =
    match resolved.children with
    | [] -> ""
    | children ->
        "\n\n# Child Agents\n\nUse the `delegate` tool to call these child agents when needed:\n"
        ^ String.concat "\n" (List.map child_summary children)
  in
  String.concat "\n" meta_lines
  ^ (if resolved.document.prompt = "" then "" else "\n\n" ^ resolved.document.prompt)
  ^ delegation_block

let input_string_schema description =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "input",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String description);
                ] );
          ] );
      ("required", `List [ `String "input" ]);
    ]

let delegate_schema aliases =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "alias",
              `Assoc
                [
                  ("type", `String "string");
                  ("enum", `List (List.map (fun alias -> `String alias) aliases));
                ] );
            ( "task",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Task for the child agent.");
                ] );
            ( "attachments",
              `Assoc
                [
                  ("type", `String "array");
                  ( "items",
                    `Assoc [ ("type", `String "string") ] );
                ] );
          ] );
      ("required", `List [ `String "alias"; `String "task" ]);
    ]

let shell_quote value =
  "'" ^ String.concat "'\"'\"'" (String.split_on_char '\'' value) ^ "'"

let path_env_entries () =
  match Sys.getenv_opt "PATH" with
  | None -> []
  | Some value when trim value = "" -> []
  | Some value -> String.split_on_char ':' value

let resolve_executable name =
  let is_executable path =
    try
      Unix.access path [ Unix.X_OK ];
      Sys.file_exists path && not (Sys.is_directory path)
    with Unix.Unix_error _ -> false
  in
  if String.contains name '/' then
    let candidate = normalize_local_path name in
    if is_executable candidate then Some candidate else None
  else
    List.find_map
      (fun dir ->
        let candidate = Filename.concat dir name in
        if is_executable candidate then Some candidate else None)
      (path_env_entries ())

let interpreter_command language =
  match language with
  | Bash -> Ok "bash"
  | Python ->
      begin
        match resolve_executable "python3", resolve_executable "python" with
        | Some _, _ -> Ok "python3"
        | None, Some _ -> Ok "python"
        | None, None -> Error "No Python interpreter found for embedded tool"
      end
  | Javascript ->
      begin
        match resolve_executable "node" with
        | Some _ -> Ok "node"
        | None -> Error "No Node.js interpreter found for embedded tool"
      end

let create_temp_dir () =
  let path = Filename.temp_file "af-tool-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let write_file_atomic path content =
  let tmp_path = path ^ ".tmp" in
  Stdlib.Out_channel.with_open_bin tmp_path (fun channel -> output_string channel content);
  Unix.rename tmp_path path

let rec cleanup_path path =
  if Sys.file_exists path then
    try
      if Sys.is_directory path then
        Sys.readdir path
        |> Array.iter (fun child -> cleanup_path (Filename.concat path child));
      if Sys.is_directory path then Unix.rmdir path else Sys.remove path
    with _ -> ()

let strip_command_wrappers content =
  let prefixes = [ "Command failed: Command output:\n"; "Command output:\n" ] in
  let strip_prefix prefix value =
    if String.starts_with ~prefix value then
      Some (String.sub value (String.length prefix) (String.length value - String.length prefix))
    else
      None
  in
  let stripped =
    List.find_map (fun prefix -> strip_prefix prefix content) prefixes
    |> Option.value ~default:content
  in
  match split_once stripped "\nExit status: " with
  | Some (body, _status) -> body
  | None -> stripped

let embedded_tool_result ~registry ~chat_id tool input =
  match interpreter_command tool.language with
  | Error err ->
      Agent_runtime.Tools.failure ~error_category:Agent_runtime.Tools.CommandNotFound err
  | Ok interpreter ->
      let temp_dir = create_temp_dir () in
      let script_path =
        let ext =
          match tool.language with
          | Bash -> ".sh"
          | Python -> ".py"
          | Javascript -> ".js"
        in
        Filename.concat temp_dir ("tool" ^ ext)
      in
      let input_path = Filename.concat temp_dir "input.txt" in
      Fun.protect
        ~finally:(fun () -> cleanup_path temp_dir)
        (fun () ->
          write_file_atomic script_path tool.code;
          write_file_atomic input_path input;
          let command =
            Printf.sprintf "%s %s < %s"
              interpreter
              (shell_quote script_path)
              (shell_quote input_path)
          in
          let result =
            Agent_runtime.Tools.execute
              registry
              ~chat_id
              "bash"
              (`Assoc [ ("command", `String command) ])
          in
          if result.Agent_runtime.Tools.approval_request <> None then
            result
          else if result.Agent_runtime.Tools.is_error then
            Agent_runtime.Tools.failure
              ?status_code:result.status_code
              ?duration_ms:result.duration_ms
              ?error_type:result.error_type
              ?error_category:result.error_category
              (strip_command_wrappers result.content)
          else
            Agent_runtime.Tools.success
              ?status_code:result.status_code
              ?duration_ms:result.duration_ms
              (strip_command_wrappers result.content))

let extract_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value when trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "%s is required" name)

let extract_string_list_field json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok []
  | `List values ->
      values
      |> List.fold_left
           (fun acc value ->
             match acc, value with
             | Error _ as err, _ -> err
             | Ok acc, `String value -> Ok (acc @ [ value ])
             | Ok _, _ -> Error (Printf.sprintf "%s must be an array of strings" name))
           (Ok [])
  | _ -> Error (Printf.sprintf "%s must be an array of strings" name)

let rec run_child ?llm_call config child ~parent_name ~task ~attachments =
  let attachment_block =
    match attachments with
    | [] -> ""
    | values ->
        "\n\nAttachments:\n" ^ String.concat "\n" (List.map (fun value -> "- " ^ value) values)
  in
  let prompt =
    Printf.sprintf "Delegated task from `%s`:\n\n%s%s" parent_name task attachment_block
  in
  match create_app ?llm_call config child with
  | Error err -> Agent_runtime.Tools.failure err
  | Ok app ->
      let chat_id =
        Agent_runtime.Session.create_conversation app ~title:("delegate:" ^ child.document.name) ()
      in
      let final_message = ref None in
      let runtime_error = ref None in
      let emit = function
        | Acp.Message.Agent_message { content; _ } -> final_message := Some content
        | Acp.Message.Error { message; _ } -> runtime_error := Some message
        | _ -> ()
      in
      Fun.protect
        ~finally:(fun () -> Agent_runtime.App.close app)
        (fun () ->
          match Agent_runtime.Session.process ~emit app ~chat_id ~persistent:false prompt with
          | Error err ->
              Agent_runtime.Tools.failure err
          | Ok () ->
              let pending =
                Hashtbl.find_opt
                  (Agent_runtime.App.internal_state app).Agent_runtime.Runtime.pending_permissions
                  chat_id
              in
              begin
                match pending with
                | Some pending ->
                    Agent_runtime.Tools.approval_required
                      pending.request
                      ("Approval required: " ^ pending.request.reason)
                | None ->
                    begin
                      match !runtime_error, !final_message with
                      | Some err, _ -> Agent_runtime.Tools.failure err
                      | None, Some text -> Agent_runtime.Tools.success text
                      | None, None ->
                          Agent_runtime.Tools.failure
                            "Child agent finished without returning a final message"
                    end
              end)

and extra_tools ?llm_call config resolved =
  let embedded_tools =
    resolved.document.tools
    |> List.map (fun (tool : embedded_tool) ->
           {
             Agent_runtime.Tools.definition =
               {
                 Llm_types.name = tool.name;
                 description = tool.description;
                 input_schema = input_string_schema "Single string input for the embedded tool.";
               };
             execute =
               (fun ~registry ~chat_id input ->
                 match extract_string_field input "input" with
                 | Error err ->
                     Agent_runtime.Tools.failure ~error_category:Agent_runtime.Tools.InvalidParameters ("Invalid parameters: " ^ err)
                 | Ok value ->
                     embedded_tool_result ~registry ~chat_id tool value);
           })
  in
  let delegate_tools =
    match resolved.children with
    | [] -> []
    | children ->
        let aliases = List.map fst children in
        [
          {
            Agent_runtime.Tools.definition =
              {
                Llm_types.name = "delegate";
                description = "Delegate a task to a child agent declared in uses.";
                input_schema = delegate_schema aliases;
              };
            execute =
              (fun ~registry:_ ~chat_id:_ input ->
                match extract_string_field input "alias", extract_string_field input "task", extract_string_list_field input "attachments" with
                | Error err, _, _ | _, Error err, _ | _, _, Error err ->
                    Agent_runtime.Tools.failure ~error_category:Agent_runtime.Tools.InvalidParameters ("Invalid parameters: " ^ err)
                | Ok alias, Ok task, Ok attachments ->
                    begin
                      match List.assoc_opt alias children with
                      | None ->
                          Agent_runtime.Tools.failure ~error_category:Agent_runtime.Tools.InvalidParameters
                            (Printf.sprintf "Invalid parameters: unknown child agent alias %s" alias)
                      | Some child ->
                          run_child ?llm_call config child ~parent_name:resolved.document.name ~task ~attachments
                    end);
          };
        ]
  in
  embedded_tools @ delegate_tools

and create_app ?llm_call config resolved =
  let extra_tools = extra_tools ?llm_call config resolved in
  Agent_runtime.App.create
    ?llm_call
    ~system_prompt_override:(system_prompt resolved)
    ~extra_tools
    config
