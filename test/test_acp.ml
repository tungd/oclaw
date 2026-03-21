let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expect_some = function
  | Some value -> value
  | None -> fail "expected Some value"

let () =
  let open Acp.Message in

  let packet =
    to_jsonrpc ~id:(`Int 7) (Agent_message { content = "hello"; chat_id = Some 3 })
  in
  begin
    match packet with
    | Jsonrpc.Packet.Request req ->
        expect (req.Jsonrpc.Request.method_ = Method.agent_message) "agent_message method mismatch";
        let params = expect_some req.Jsonrpc.Request.params in
        expect
          ((params : Jsonrpc.Structured.t :> Yojson.Safe.t)
           = `Assoc [ ("content", `String "hello"); ("chat_id", `Int 3) ])
          "agent_message params mismatch"
    | _ -> fail "expected request packet"
  end;

  let tool_call =
    {
      tool_call_id = "call_001";
      title = Some "Reading configuration file";
      kind = Some Read;
      status = Some Pending;
      content = None;
      raw_input = Some (`Assoc [ ("path", `String "/tmp/config") ]);
      raw_output = None;
    }
  in
  let tool_packet =
    to_jsonrpc
      (Session_update { session_id = "sess_1"; update = Tool_call tool_call })
  in
  begin
    match tool_packet with
    | Jsonrpc.Packet.Notification notif ->
        expect (notif.Jsonrpc.Notification.method_ = Method.session_update) "session_update method mismatch";
        begin
          match expect_some (of_jsonrpc_packet tool_packet) with
          | Session_update { session_id; update = Tool_call call } ->
              expect (session_id = "sess_1") "session id mismatch";
              expect (call.tool_call_id = "call_001") "toolCallId mismatch";
              expect (call.kind = Some Read) "tool kind mismatch"
          | _ -> fail "expected tool_call round-trip"
        end
    | _ -> fail "expected tool_call notification"
  end;

  let permission_packet =
    to_jsonrpc
      ~id:(`Int 5)
      (Request_permission
         {
           session_id = "sess_1";
           tool_call;
           options =
             [
               { option_id = "allow-once"; name = "Allow once"; kind = Allow_once };
               { option_id = "reject-once"; name = "Reject"; kind = Reject_once };
             ];
         })
  in
  begin
    match permission_packet with
    | Jsonrpc.Packet.Request req ->
        expect (req.Jsonrpc.Request.method_ = Method.request_permission) "request_permission method mismatch";
        begin
          match expect_some (of_jsonrpc_packet permission_packet) with
          | Request_permission { tool_call = call; options; _ } ->
              expect (call.tool_call_id = "call_001") "permission toolCallId mismatch";
              expect (List.length options = 2) "permission option count mismatch"
          | _ -> fail "expected request_permission round-trip"
        end
    | _ -> fail "expected request_permission request"
  end;

  let permission_response =
    Jsonrpc.Packet.Response
      (Jsonrpc.Response.ok
         (`Int 5)
         (`Assoc [ ("outcome", `Assoc [ ("outcome", `String "selected"); ("optionId", `String "allow-once") ]) ]))
  in
  begin
    match permission_outcome_of_packet permission_response with
    | Some (`Int 5, Selected "allow-once") -> ()
    | _ -> fail "expected permission outcome round-trip"
  end;

  Printf.printf "[PASS] acp tests\n"
