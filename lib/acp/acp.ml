(** Agent Client Protocol implementation using jsonrpc library. *)


module Message = struct
  module Method = struct
    let initialize = "initialize"
    let initialized = "initialized"
    let session_update = "session/update"
    let request_permission = "session/request_permission"
    let agent_plan = "agent/plan"
    let agent_message = "agent/message"
    let agent_delta = "agent/delta"
    let status = "status"
  end

  type initialize_params = {
    capabilities : Yojson.Safe.t;
  }
  [@@deriving yojson]

  type agent_message_params = {
    content : string;
    chat_id : int option;
  }
  [@@deriving yojson]

  type agent_plan_params = {
    steps : string list;
  }
  [@@deriving yojson]

  type agent_delta_params = {
    content : string;
  }
  [@@deriving yojson]

  type status_params = {
    status : string;
    message : string option;
  }
  [@@deriving yojson]

  type tool_kind =
    | Read
    | Edit
    | Delete
    | Move
    | Search
    | Execute
    | Think
    | Fetch
    | Other

  type tool_status =
    | Pending
    | In_progress
    | Completed
    | Failed

  type content_block =
    | Text of string

  type tool_call_content =
    | Content of content_block

  type tool_call = {
    tool_call_id : string;
    title : string option;
    kind : tool_kind option;
    status : tool_status option;
    content : tool_call_content list option;
    raw_input : Yojson.Safe.t option;
    raw_output : Yojson.Safe.t option;
  }

  type session_update =
    | Tool_call of tool_call
    | Tool_call_update of tool_call

  type permission_option_kind =
    | Allow_once
    | Allow_always
    | Reject_once
    | Reject_always

  type permission_option = {
    option_id : string;
    name : string;
    kind : permission_option_kind;
  }

  type permission_outcome =
    | Selected of string
    | Cancelled

  let tool_kind_to_string = function
    | Read -> "read"
    | Edit -> "edit"
    | Delete -> "delete"
    | Move -> "move"
    | Search -> "search"
    | Execute -> "execute"
    | Think -> "think"
    | Fetch -> "fetch"
    | Other -> "other"

  let tool_kind_of_string = function
    | "read" -> Some Read
    | "edit" -> Some Edit
    | "delete" -> Some Delete
    | "move" -> Some Move
    | "search" -> Some Search
    | "execute" -> Some Execute
    | "think" -> Some Think
    | "fetch" -> Some Fetch
    | "other" -> Some Other
    | _ -> None

  let tool_status_to_string = function
    | Pending -> "pending"
    | In_progress -> "in_progress"
    | Completed -> "completed"
    | Failed -> "failed"

  let tool_status_of_string = function
    | "pending" -> Some Pending
    | "in_progress" -> Some In_progress
    | "completed" -> Some Completed
    | "failed" -> Some Failed
    | _ -> None

  let permission_option_kind_to_string = function
    | Allow_once -> "allow_once"
    | Allow_always -> "allow_always"
    | Reject_once -> "reject_once"
    | Reject_always -> "reject_always"

  let permission_option_kind_of_string = function
    | "allow_once" -> Some Allow_once
    | "allow_always" -> Some Allow_always
    | "reject_once" -> Some Reject_once
    | "reject_always" -> Some Reject_always
    | _ -> None

  let content_block_to_yojson = function
    | Text text -> `Assoc [ ("type", `String "text"); ("text", `String text) ]

  let content_block_of_yojson = function
    | `Assoc fields ->
        begin
          match List.assoc_opt "type" fields, List.assoc_opt "text" fields with
          | Some (`String "text"), Some (`String text) -> Some (Text text)
          | _ -> None
        end
    | _ -> None

  let tool_call_content_to_yojson = function
    | Content block -> `Assoc [ ("type", `String "content"); ("content", content_block_to_yojson block) ]

  let tool_call_content_of_yojson = function
    | `Assoc fields ->
        begin
          match List.assoc_opt "type" fields, List.assoc_opt "content" fields with
          | Some (`String "content"), Some content_json ->
              Option.map (fun block -> Content block) (content_block_of_yojson content_json)
          | _ -> None
        end
    | _ -> None

  let option_field name to_json = function
    | None -> []
    | Some value -> [ (name, to_json value) ]

  let tool_call_to_yojson tool_call =
    `Assoc
      ([ ("toolCallId", `String tool_call.tool_call_id) ]
       @ option_field "title" (fun v -> `String v) tool_call.title
       @ option_field "kind" (fun v -> `String (tool_kind_to_string v)) tool_call.kind
       @ option_field "status" (fun v -> `String (tool_status_to_string v)) tool_call.status
       @ option_field "content" (fun blocks -> `List (List.map tool_call_content_to_yojson blocks)) tool_call.content
       @ option_field "rawInput" (fun v -> v) tool_call.raw_input
       @ option_field "rawOutput" (fun v -> v) tool_call.raw_output)

  let tool_call_of_yojson = function
    | `Assoc fields as json ->
        let find name = List.assoc_opt name fields in
        begin
          match find "toolCallId" with
          | Some (`String tool_call_id) ->
              let title =
                match find "title" with
                | Some (`String value) -> Some value
                | _ -> None
              in
              let kind =
                match find "kind" with
                | Some (`String value) -> tool_kind_of_string value
                | _ -> None
              in
              let status =
                match find "status" with
                | Some (`String value) -> tool_status_of_string value
                | _ -> None
              in
              let content =
                match find "content" with
                | Some (`List values) ->
                    let parsed = List.filter_map tool_call_content_of_yojson values in
                    Some parsed
                | _ -> None
              in
              let raw_input = find "rawInput" in
              let raw_output = find "rawOutput" in
              Some { tool_call_id; title; kind; status; content; raw_input; raw_output }
          | _ ->
              let _ = json in
              None
        end
    | _ -> None

  let permission_option_to_yojson option =
    `Assoc
      [
        ("optionId", `String option.option_id);
        ("name", `String option.name);
        ("kind", `String (permission_option_kind_to_string option.kind));
      ]

  let permission_option_of_yojson = function
    | `Assoc fields ->
        begin
          match List.assoc_opt "optionId" fields, List.assoc_opt "name" fields, List.assoc_opt "kind" fields with
          | Some (`String option_id), Some (`String name), Some (`String kind) ->
              Option.map (fun kind -> { option_id; name; kind }) (permission_option_kind_of_string kind)
          | _ -> None
        end
    | _ -> None

  type t =
    | Initialize of { capabilities : Yojson.Safe.t }
    | Initialized
    | Session_update of { session_id : string; update : session_update }
    | Request_permission of { session_id : string; tool_call : tool_call; options : permission_option list }
    | Agent_message of { content : string; chat_id : int option }
    | Agent_plan of { steps : string list }
    | Agent_delta of { content : string }
    | Status of { status : string; message : string option }
    | Error of { message : string; code : int }
    | Done

  let structured_of_yojson json =
    Jsonrpc.Structured.t_of_yojson json

  let session_update_to_yojson = function
    | Tool_call tool_call ->
        `Assoc
          [
            ("sessionUpdate", `String "tool_call");
            ("toolCallId", `String tool_call.tool_call_id);
          ]
        |> fun (`Assoc fields) ->
          `Assoc
            (fields
             @ option_field "title" (fun value -> `String value) tool_call.title
             @ option_field "kind" (fun value -> `String (tool_kind_to_string value)) tool_call.kind
             @ option_field "status" (fun value -> `String (tool_status_to_string value)) tool_call.status
             @ option_field "content" (fun values -> `List (List.map tool_call_content_to_yojson values)) tool_call.content
             @ option_field "rawInput" (fun value -> value) tool_call.raw_input
             @ option_field "rawOutput" (fun value -> value) tool_call.raw_output)
    | Tool_call_update tool_call ->
        `Assoc
          [
            ("sessionUpdate", `String "tool_call_update");
            ("toolCallId", `String tool_call.tool_call_id);
          ]
        |> fun (`Assoc fields) ->
          `Assoc
            (fields
             @ option_field "title" (fun value -> `String value) tool_call.title
             @ option_field "kind" (fun value -> `String (tool_kind_to_string value)) tool_call.kind
             @ option_field "status" (fun value -> `String (tool_status_to_string value)) tool_call.status
             @ option_field "content" (fun values -> `List (List.map tool_call_content_to_yojson values)) tool_call.content
             @ option_field "rawInput" (fun value -> value) tool_call.raw_input
             @ option_field "rawOutput" (fun value -> value) tool_call.raw_output)

  let session_update_of_yojson = function
    | `Assoc fields ->
        begin
          match List.assoc_opt "sessionUpdate" fields, tool_call_of_yojson (`Assoc fields) with
          | Some (`String "tool_call"), Some tool_call -> Some (Tool_call tool_call)
          | Some (`String "tool_call_update"), Some tool_call -> Some (Tool_call_update tool_call)
          | _ -> None
        end
    | _ -> None

  let to_structured ~id:_ t =
    match t with
    | Initialize { capabilities } ->
        initialize_params_to_yojson { capabilities }
    | Initialized ->
        `Assoc []
    | Session_update { session_id; update } ->
        `Assoc [ ("sessionId", `String session_id); ("update", session_update_to_yojson update) ]
    | Request_permission { session_id; tool_call; options } ->
        `Assoc
          [
            ("sessionId", `String session_id);
            ("toolCall", tool_call_to_yojson tool_call);
            ("options", `List (List.map permission_option_to_yojson options));
          ]
    | Agent_message { content; chat_id } ->
        agent_message_params_to_yojson { content; chat_id }
    | Agent_plan { steps } ->
        agent_plan_params_to_yojson { steps }
    | Agent_delta { content } ->
        agent_delta_params_to_yojson { content }
    | Status { status; message } ->
        status_params_to_yojson { status; message }
    | Error { message; code } ->
        `Assoc [("message", `String message); ("code", `Int code)]
    | Done ->
        `Assoc []

  let to_jsonrpc ?(id = `Int 0) t =
    match t with
    | Initialize { capabilities } ->
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.initialize ~params:(`Assoc [("capabilities", capabilities)]) ())
    | Initialized ->
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.initialized ())
    | Session_update _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.session_update ~params ())
    | Request_permission _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.request_permission ~params ())
    | Agent_message _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.agent_message ~params ())
    | Agent_plan _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_plan ~params ())
    | Agent_delta _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_delta ~params ())
    | Status _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.status ~params ())
    | Error { message; code } ->
        let error = Jsonrpc.Response.Error.make ~code:(Jsonrpc.Response.Error.Code.Other code) ~message () in
        Jsonrpc.Packet.Response (Jsonrpc.Response.error id error)
    | Done ->
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:"done" ())

  let of_structured method_name params =
    match method_name, params with
    | "initialize", _ ->
        begin match initialize_params_of_yojson params with
        | Ok { capabilities } -> Some (Initialize { capabilities })
        | Error _ -> None
        end
    | "session/update", `Assoc fields ->
        begin
          match List.assoc_opt "sessionId" fields, List.assoc_opt "update" fields with
          | Some (`String session_id), Some update_json ->
              Option.map (fun update -> Session_update { session_id; update }) (session_update_of_yojson update_json)
          | _ -> None
        end
    | "session/request_permission", `Assoc fields ->
        begin
          match List.assoc_opt "sessionId" fields, List.assoc_opt "toolCall" fields, List.assoc_opt "options" fields with
          | Some (`String session_id), Some tool_call_json, Some (`List option_jsons) ->
              begin
                match tool_call_of_yojson tool_call_json with
                | None -> None
                | Some tool_call ->
                    let options = List.filter_map permission_option_of_yojson option_jsons in
                    Some (Request_permission { session_id; tool_call; options })
              end
          | _ -> None
        end
    | "agent/message", _ ->
        begin match agent_message_params_of_yojson params with
        | Ok { content; chat_id } -> Some (Agent_message { content; chat_id })
        | Error _ -> None
        end
    | "agent/plan", _ ->
        begin match agent_plan_params_of_yojson params with
        | Ok { steps } -> Some (Agent_plan { steps })
        | Error _ -> None
        end
    | "agent/delta", _ ->
        begin match agent_delta_params_of_yojson params with
        | Ok { content } -> Some (Agent_delta { content })
        | Error _ -> None
        end
    | "status", _ ->
        begin match status_params_of_yojson params with
        | Ok { status; message } -> Some (Status { status; message })
        | Error _ -> None
        end
    | "done", _ ->
        Some Done
    | "initialized", _ ->
        Some Initialized
    | _ -> None

  let of_jsonrpc_packet packet =
    match packet with
    | Jsonrpc.Packet.Request req ->
        of_structured req.Jsonrpc.Request.method_
          ((Option.value ~default:(`Assoc []) req.Jsonrpc.Request.params : Jsonrpc.Structured.t) :> Yojson.Safe.t)
    | Jsonrpc.Packet.Notification notif ->
        of_structured notif.Jsonrpc.Notification.method_
          ((Option.value ~default:(`Assoc []) notif.Jsonrpc.Notification.params : Jsonrpc.Structured.t) :> Yojson.Safe.t)
    | Jsonrpc.Packet.Response resp ->
        (match resp.Jsonrpc.Response.result with
         | Ok _ -> None
         | Error e ->
             Some (Error { message = e.Jsonrpc.Response.Error.message; code =
               match e.Jsonrpc.Response.Error.code with
               | Other c -> c
               | _ -> -1
             })
        )
    | Jsonrpc.Packet.Batch_response _ | Jsonrpc.Packet.Batch_call _ ->
        None

  let permission_outcome_of_packet packet =
    match packet with
    | Jsonrpc.Packet.Response resp ->
        begin
          match resp.Jsonrpc.Response.result with
          | Error _ -> None
          | Ok json ->
              begin
                match (json : Yojson.Safe.t) with
                | `Assoc fields ->
                    begin
                      match List.assoc_opt "outcome" fields with
                      | Some (`Assoc outcome_fields) ->
                          begin
                            match List.assoc_opt "outcome" outcome_fields with
                            | Some (`String "cancelled") -> Some (resp.Jsonrpc.Response.id, Cancelled)
                            | Some (`String "selected") ->
                                begin
                                  match List.assoc_opt "optionId" outcome_fields with
                                  | Some (`String option_id) -> Some (resp.Jsonrpc.Response.id, Selected option_id)
                                  | _ -> None
                                end
                            | _ -> None
                          end
                      | _ -> None
                    end
                | _ -> None
              end
        end
    | _ -> None
end

module Channel = struct
  type 'a t = {
    mutex : Mutex.t;
    condition : Condition.t;
    queue : 'a Queue.t;
  }

  let create () = {
    mutex = Mutex.create ();
    condition = Condition.create ();
    queue = Queue.create ();
  }

  let send chan msg =
    Mutex.lock chan.mutex;
    Queue.add msg chan.queue;
    Condition.signal chan.condition;
    Mutex.unlock chan.mutex

  let recv chan =
    Mutex.lock chan.mutex;
    while Queue.is_empty chan.queue do
      Condition.wait chan.condition chan.mutex
    done;
    let msg = Queue.take chan.queue in
    Mutex.unlock chan.mutex;
    msg

  let try_recv chan =
    Mutex.lock chan.mutex;
    let result =
      if Queue.is_empty chan.queue then None
      else Some (Queue.take chan.queue)
    in
    Mutex.unlock chan.mutex;
    result

  let recv_timeout chan timeout =
    Mutex.lock chan.mutex;
    let deadline = Unix.gettimeofday () +. timeout in
    let rec wait () =
      if not (Queue.is_empty chan.queue) then
        Some (Queue.take chan.queue)
      else
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then None
        else begin
          Condition.wait chan.condition chan.mutex;
          let now = Unix.gettimeofday () in
          if now >= deadline then None
          else wait ()
        end
    in
    let result = wait () in
    Mutex.unlock chan.mutex;
    result
end

module Stdio_frontend = struct
  type config = {
    input : in_channel;
    output : out_channel;
  }

  type t = {
    input : in_channel;
    output : out_channel;
    output_mutex : Mutex.t;
  }

  let create ?(config = { input = stdin; output = stdout }) () = {
    input = config.input;
    output = config.output;
    output_mutex = Mutex.create ();
  }

  let send t msg =
    let json = Jsonrpc.Packet.yojson_of_t msg in
    Mutex.lock t.output_mutex;
    output_string t.output (Yojson.Safe.to_string json);
    output_char t.output '\n';
    flush t.output;
    Mutex.unlock t.output_mutex

  let recv t =
    try
      let line = input_line t.input in
      if String.trim line = "" then None
      else
        let json = Yojson.Safe.from_string line in
        Some (Jsonrpc.Packet.t_of_yojson json)
    with exn ->
      Logs.err (fun m -> m "Failed to parse JSON-RPC: %s" (Printexc.to_string exn));
      None

  let recv_timeout t timeout =
    let fd = Unix.descr_of_in_channel t.input in
    let readable, _, _ = Unix.select [fd] [] [] timeout in
    if readable = [] then None
    else recv t

  let close t =
    Mutex.lock t.output_mutex;
    flush t.output;
    Mutex.unlock t.output_mutex
end
