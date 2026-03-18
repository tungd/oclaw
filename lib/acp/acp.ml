(** Agent Client Protocol implementation using jsonrpc library. *)

open Yojson.Safe

module Message = struct
  module Method = struct
    let initialize = "initialize"
    let initialized = "initialized"
    let tools_list = "tools/list"
    let tools_call = "tools/call"
    let agent_plan = "agent/plan"
    let agent_message = "agent/message"
    let agent_delta = "agent/delta"
    let status = "status"
  end

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

  let to_structured ~id t =
    match t with
    | Initialize { capabilities } ->
        `Assoc [("capabilities", capabilities)]
    | Initialized ->
        `Assoc []
    | Agent_message { content; chat_id } ->
        let fields = [("content", `String content)] in
        let fields = match chat_id with
          | Some cid -> ("chat_id", `Int cid) :: fields
          | None -> fields
        in
        `Assoc fields
    | Agent_plan { steps } ->
        `Assoc [("steps", `List (List.map (fun s -> `String s) steps))]
    | Agent_delta { content } ->
        `Assoc [("content", `String content)]
    | Tool_call { name; arguments } ->
        `Assoc [("name", `String name); ("arguments", arguments)]
    | Tool_result { name; content; is_error } ->
        `Assoc [("name", `String name); ("content", `String content); ("is_error", `Bool is_error)]
    | Status { status; message } ->
        let fields = [("status", `String status)] in
        let fields = match message with
          | Some m -> ("message", `String m) :: fields
          | None -> fields
        in
        `Assoc fields
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
    | Agent_message { content; chat_id } ->
        let params = to_structured ~id t in
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.agent_message ~params ())
    | Agent_plan { steps } ->
        let params = to_structured ~id t in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_plan ~params ())
    | Agent_delta { content } ->
        let params = to_structured ~id t in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.agent_delta ~params ())
    | Tool_call { name; arguments } ->
        let params = to_structured ~id t in
        Jsonrpc.Packet.Request (Jsonrpc.Request.create ~id ~method_:Method.tools_call ~params ())
    | Tool_result { name; content; is_error } ->
        (* Tool results are sent as responses *)
        let result = `Assoc [("name", `String name); ("content", `String content); ("is_error", `Bool is_error)] in
        Jsonrpc.Packet.Response (Jsonrpc.Response.ok id result)
    | Status { status; message } ->
        let params = to_structured ~id t in
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:Method.status ~params ())
    | Error { message; code } ->
        let error = Jsonrpc.Response.Error.make ~code:(Jsonrpc.Response.Error.Code.Other code) ~message () in
        Jsonrpc.Packet.Response (Jsonrpc.Response.error id error)
    | Done ->
        Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:"done" ())

  let of_structured method_name params =
    match method_name, params with
    | "initialize", `Assoc fields ->
        let capabilities = List.assoc_opt "capabilities" fields |> Option.value ~default:`Null in
        Some (Initialize { capabilities })
    | "agent/message", `Assoc fields ->
        let content = match List.assoc_opt "content" fields with
          | Some (`String s) -> s
          | _ -> ""
        in
        let chat_id = match List.assoc_opt "chat_id" fields with
          | Some (`Int i) -> Some i
          | _ -> None
        in
        Some (Agent_message { content; chat_id })
    | "tools/call", `Assoc fields ->
        let name = match List.assoc_opt "name" fields with
          | Some (`String s) -> s
          | _ -> ""
        in
        let arguments = List.assoc_opt "arguments" fields |> Option.value ~default:`Null in
        Some (Tool_call { name; arguments })
    | "agent/plan", `Assoc fields ->
        let steps = match List.assoc_opt "steps" fields with
          | Some (`List l) -> List.filter_map (function `String s -> Some s | _ -> None) l
          | _ -> []
        in
        Some (Agent_plan { steps })
    | "agent/delta", `Assoc fields ->
        let content = match List.assoc_opt "content" fields with
          | Some (`String s) -> s
          | _ -> ""
        in
        Some (Agent_delta { content })
    | "status", `Assoc fields ->
        let status = match List.assoc_opt "status" fields with
          | Some (`String s) -> s
          | _ -> ""
        in
        let message = match List.assoc_opt "message" fields with
          | Some (`String s) -> Some s
          | _ -> None
        in
        Some (Status { status; message })
    | "done", _ ->
        Some Done
    | "initialized", _ ->
        Some Initialized
    | _ -> None

  let of_jsonrpc_packet packet =
    match packet with
    | Jsonrpc.Packet.Request req ->
        of_structured req.Jsonrpc.Request.method_ (Option.value ~default:(`Assoc []) req.Jsonrpc.Request.params)
    | Jsonrpc.Packet.Notification notif ->
        of_structured notif.Jsonrpc.Notification.method_ (Option.value ~default:(`Assoc []) notif.Jsonrpc.Notification.params)
    | Jsonrpc.Packet.Response resp ->
        (match resp.Jsonrpc.Response.result with
         | Ok (`Assoc fields) ->
             let name = match List.assoc_opt "name" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let content = match List.assoc_opt "content" fields with
               | Some (`String s) -> s
               | _ -> ""
             in
             let is_error = match List.assoc_opt "is_error" fields with
               | Some (`Bool b) -> b
               | _ -> false
             in
             Some (Tool_result { name; content; is_error })
         | Error e ->
             Some (Error { message = e.Jsonrpc.Response.Error.message; code = 
               match e.Jsonrpc.Response.Error.code with
               | Other c -> c
               | _ -> -1
             })
         | _ -> None)
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
