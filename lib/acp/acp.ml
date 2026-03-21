(** Agent Client Protocol implementation using jsonrpc library. *)


module Message = struct
  module Method = struct
    let initialize = "initialize"
    let initialized = "initialized"
    let tools_list = "tools/list"
    let tools_call = "tools/call"
    let tools_result = "tools/result"
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

  type tool_call_params = {
    name : string;
    arguments : Yojson.Safe.t;
  }
  [@@deriving yojson]

  type tool_result_payload = {
    name : string;
    content : string;
    is_error : bool;
  }
  [@@deriving yojson]

  type status_params = {
    status : string;
    message : string option;
  }
  [@@deriving yojson]

  type t =
    | Initialize of { capabilities : Yojson.Safe.t }
    | Initialized
    | Agent_message of { content : string; chat_id : int option }
    | Agent_plan of { steps : string list }
    | Agent_delta of { content : string }
    | Tool_call of { name : string; arguments : Yojson.Safe.t }
    | Tool_result of { name : string; content : string; is_error : bool }
    | Status of { status : string; message : string option }
    | Error of { message : string; code : int }
    | Done

  let structured_of_yojson json =
    Jsonrpc.Structured.t_of_yojson json

  let to_structured ~id:_ t =
    match t with
    | Initialize { capabilities } ->
        initialize_params_to_yojson { capabilities }
    | Initialized ->
        `Assoc []
    | Agent_message { content; chat_id } ->
        agent_message_params_to_yojson { content; chat_id }
    | Agent_plan { steps } ->
        agent_plan_params_to_yojson { steps }
    | Agent_delta { content } ->
        agent_delta_params_to_yojson { content }
    | Tool_call { name; arguments } ->
        tool_call_params_to_yojson { name; arguments }
    | Tool_result { name; content; is_error } ->
        tool_result_payload_to_yojson { name; content; is_error }
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
    | Agent_message _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.agent_message ~params ())
    | Agent_plan _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_plan ~params ())
    | Agent_delta _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_delta ~params ())
    | Tool_call _ ->
        let params = structured_of_yojson (to_structured ~id t) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.tools_call ~params ())
    | Tool_result { name; content; is_error } ->
        let params = structured_of_yojson (tool_result_payload_to_yojson { name; content; is_error }) in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.tools_result ~params ())
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
    | "agent/message", _ ->
        begin match agent_message_params_of_yojson params with
        | Ok { content; chat_id } -> Some (Agent_message { content; chat_id })
        | Error _ -> None
        end
    | "tools/call", _ ->
        begin match tool_call_params_of_yojson params with
        | Ok { name; arguments } -> Some (Tool_call { name; arguments })
        | Error _ -> None
        end
    | "tools/result", _ ->
        begin match tool_result_payload_of_yojson params with
        | Ok { name; content; is_error } -> Some (Tool_result { name; content; is_error })
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
