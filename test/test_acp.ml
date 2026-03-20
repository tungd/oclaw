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
    to_jsonrpc ~id:(`Int 7)
      (Agent_message { content = "hello"; chat_id = Some 3 })
  in
  begin
    match packet with
    | Jsonrpc.Packet.Request req ->
        expect (req.Jsonrpc.Request.method_ = Method.agent_message)
          "agent_message method mismatch";
        let params = expect_some req.Jsonrpc.Request.params in
        expect
          ((params : Jsonrpc.Structured.t :> Yojson.Safe.t)
           = `Assoc [ ("content", `String "hello"); ("chat_id", `Int 3) ])
          "agent_message params mismatch"
    | _ -> fail "expected request packet"
  end;

  let tool_packet =
    to_jsonrpc ~id:(`Int 9)
      (Tool_call { name = "bash"; arguments = `Assoc [ ("command", `String "pwd") ] })
  in
  begin
    match expect_some (of_jsonrpc_packet tool_packet) with
    | Tool_call { name; arguments } ->
        expect (name = "bash") "tool_call name mismatch";
        expect (arguments = `Assoc [ ("command", `String "pwd") ])
          "tool_call arguments mismatch"
    | _ -> fail "expected tool_call round-trip"
  end;

  let response_packet =
    to_jsonrpc ~id:(`Int 5)
      (Tool_result { name = "read_file"; content = "ok"; is_error = false })
  in
  begin
    match expect_some (of_jsonrpc_packet response_packet) with
    | Tool_result { name; content; is_error } ->
        expect (name = "read_file") "tool_result name mismatch";
        expect (content = "ok") "tool_result content mismatch";
        expect (not is_error) "tool_result error flag mismatch"
    | _ -> fail "expected tool_result round-trip"
  end;

  Printf.printf "[PASS] acp tests\n"
