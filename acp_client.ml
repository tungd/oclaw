(** ACP client for the OClaw CLI. *)

open Yojson.Safe.Util

module Log = (val Logs.src_log (Logs.Src.create "acp_client") : Logs.LOG)
module Transport = Acp_transport

type session_update =
  | Agent_message_chunk of string
  | Tool_call of { tool_call_id : string; title : string; status : string option }
  | Tool_call_update of { tool_call_id : string; title : string option; status : string option }
  | Plan of string list
  | Other of Yojson.Safe.t

type prompt_result = {
  content : string;
  stop_reason : string;
}

type t = {
  conn : Transport.connection;
  endpoint : Transport.endpoint;
  mutable session_id : string option;
}

let request_json ~id ~method_ ~params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
    ("params", params);
  ]

let parse_error_message error_json =
  let message =
    match error_json |> member "message" with
    | `String s -> s
    | _ -> "Unknown ACP error"
  in
  let details =
    match error_json |> member "data" with
    | `Null -> None
    | data -> Some (Yojson.Safe.to_string data)
  in
  match details with
  | None -> message
  | Some data -> message ^ " (" ^ data ^ ")"

let extract_text_content = function
  | `Assoc _ as content ->
      begin
        match content |> member "type", content |> member "text" with
        | `String "text", `String text -> Some text
        | _ -> None
      end
  | _ -> None

let json_list = function
  | `List items -> items
  | _ -> []

let parse_plan update =
  update
  |> member "entries"
  |> json_list
  |> List.filter_map (fun entry ->
       match entry |> member "content" |> member "type", entry |> member "content" |> member "text" with
       | `String "text", `String text -> Some text
       | _ ->
           begin
             match entry |> member "title" with
             | `String title -> Some title
             | _ -> None
           end)

let parse_session_update json =
  match json |> member "params" with
  | `Assoc _ as params ->
      let session_id = params |> member "sessionId" |> to_string_option in
      let update = params |> member "update" in
      let parsed =
        match update |> member "sessionUpdate" with
        | `String "agent_message_chunk" ->
            begin
              match extract_text_content (update |> member "content") with
              | Some text -> Agent_message_chunk text
              | None -> Other update
            end
        | `String "tool_call" ->
            Tool_call {
              tool_call_id = update |> member "toolCallId" |> to_string;
              title = update |> member "title" |> to_string;
              status = update |> member "status" |> to_string_option;
            }
        | `String "tool_call_update" ->
            Tool_call_update {
              tool_call_id = update |> member "toolCallId" |> to_string;
              title = update |> member "title" |> to_string_option;
              status = update |> member "status" |> to_string_option;
            }
        | `String "plan" ->
            Plan (parse_plan update)
        | _ -> Other update
      in
      Some (session_id, parsed)
  | _ -> None

let wait_for_response client request_id ~on_update ?session_id () =
  let buffer = Buffer.create 256 in
  let rec loop () =
    let msg = Transport.recv_json client.conn in
    match msg |> member "method" with
    | `String "session/update" ->
        begin
          match parse_session_update msg with
          | Some (received_session_id, update) ->
              let relevant =
                match session_id, received_session_id with
                | Some wanted, Some got -> String.equal wanted got
                | None, _ -> true
                | _, None -> false
              in
              if relevant then (
                (match update with
                 | Agent_message_chunk text -> Buffer.add_string buffer text
                 | _ -> ());
                on_update update
              );
              loop ()
          | None -> loop ()
        end
    | _ ->
        if msg |> member "id" = `Int request_id then
          match msg |> member "error" with
          | `Assoc _ as error_json -> Error (parse_error_message error_json)
          | _ ->
              begin
                match msg |> member "result" with
                | `Assoc _ as result -> Ok (result, Buffer.contents buffer)
                | _ -> Error "ACP response missing result"
              end
        else
          loop ()
  in
  loop ()

let connect endpoint =
  let conn = Transport.connect endpoint in
  { conn; endpoint; session_id = None }

let close client =
  Transport.close client.conn

let initialize client =
  let id = Transport.next_request_id client.conn in
  let params =
    `Assoc [
      ("protocolVersion", `Int 1);
      ("clientCapabilities", `Assoc [
        ("fs", `Assoc [
          ("readTextFile", `Bool false);
          ("writeTextFile", `Bool false);
        ]);
        ("terminal", `Bool false);
      ]);
      ("clientInfo", `Assoc [
        ("name", `String "oclaw");
        ("version", `String "1.0.0");
      ]);
    ]
  in
  Transport.send_json client.conn (request_json ~id ~method_:"initialize" ~params);
  match wait_for_response client id ~on_update:(fun _ -> ()) () with
  | Error err -> Error err
  | Ok (result, _) ->
      begin
        match result |> member "protocolVersion" with
        | `Int 1 -> Ok ()
        | `Int version -> Error (Printf.sprintf "Unsupported ACP protocol version: %d" version)
        | _ -> Error "ACP initialize response missing protocolVersion"
      end

let new_session client ~cwd =
  let id = Transport.next_request_id client.conn in
  let params =
    `Assoc [
      ("cwd", `String cwd);
      ("mcpServers", `List []);
    ]
  in
  Transport.send_json client.conn (request_json ~id ~method_:"session/new" ~params);
  match wait_for_response client id ~on_update:(fun _ -> ()) () with
  | Error err -> Error err
  | Ok (result, _) ->
      begin
        match result |> member "sessionId" with
        | `String session_id ->
            client.session_id <- Some session_id;
            Ok session_id
        | _ -> Error "ACP session/new response missing sessionId"
      end

let prompt client ?(on_update=(fun _ -> ())) content =
  match client.session_id with
  | None -> Error "ACP session not initialized"
  | Some session_id ->
      let id = Transport.next_request_id client.conn in
      let params =
        `Assoc [
          ("sessionId", `String session_id);
          ("prompt", `List [
            `Assoc [
              ("type", `String "text");
              ("text", `String content);
            ]
          ]);
        ]
      in
      Transport.send_json client.conn (request_json ~id ~method_:"session/prompt" ~params);
      match wait_for_response client id ~on_update ~session_id () with
      | Error err -> Error err
      | Ok (result, streamed_content) ->
          let stop_reason =
            match result |> member "stopReason" with
            | `String reason -> reason
            | _ -> "end_turn"
          in
          Ok { content = streamed_content; stop_reason }

let connect_and_initialize endpoint =
  let client = connect endpoint in
  match initialize client with
  | Ok () ->
      begin
        match new_session client ~cwd:(Sys.getcwd ()) with
        | Ok _ -> Ok client
        | Error err ->
            close client;
            Error err
      end
  | Error err ->
      close client;
      Error err

let print_session_update = function
  | Agent_message_chunk text ->
      print_string text;
      flush stdout
  | Tool_call { title; status; _ } ->
      let status = Option.value ~default:"pending" status in
      Printf.printf "\n[tool:%s] %s\n" status title;
      flush stdout
  | Tool_call_update { title; status; _ } ->
      let label = Option.value ~default:"tool update" title in
      let status = Option.value ~default:"in_progress" status in
      Printf.printf "\n[tool:%s] %s\n" status label;
      flush stdout
  | Plan entries ->
      if entries <> [] then (
        Printf.printf "\n[plan] %s\n" (String.concat " | " entries);
        flush stdout
      )
  | Other _ -> ()

let run_prompt client content =
  let streamed = ref false in
  let on_update update =
    match update with
    | Agent_message_chunk _ ->
        streamed := true;
        print_session_update update
    | _ -> print_session_update update
  in
  match prompt client ~on_update content with
  | Error err -> Error err
  | Ok result ->
      if not !streamed then print_string result.content;
      if result.stop_reason <> "end_turn" then
        Printf.printf "\n[%s]" result.stop_reason;
      print_string "\n\n";
      flush stdout;
      Ok ()

let run_interactive endpoint =
  match connect_and_initialize endpoint with
  | Error err -> Error err
  | Ok client ->
      Fun.protect
        ~finally:(fun () -> close client)
        (fun () ->
          Printf.printf "OClaw ACP client connected to %s\n" (Transport.endpoint_to_string endpoint);
          Printf.printf "Commands: /quit, /clear\n\n";
          flush stdout;
          let rec loop () =
            Printf.printf "> ";
            flush stdout;
            match read_line () with
            | exception End_of_file ->
                print_endline "\nGoodbye!";
                Ok ()
            | "/quit" | "/exit" ->
                print_endline "Goodbye!";
                Ok ()
            | "/clear" ->
                begin
                  match new_session client ~cwd:(Sys.getcwd ()) with
                  | Ok _ ->
                      print_endline "Started a fresh ACP session.";
                      loop ()
                  | Error err -> Error err
                end
            | "" -> loop ()
            | content ->
                match run_prompt client content with
                | Ok () -> loop ()
                | Error err -> Error err
          in
          loop ())

let run_single_shot endpoint input =
  match connect_and_initialize endpoint with
  | Error err -> Error err
  | Ok client ->
      Fun.protect
        ~finally:(fun () -> close client)
        (fun () -> run_prompt client input)
