let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let provider =
  {
    Llm_provider.api_base = "http://example.invalid";
    api_key = "test";
    model = {
      Llm_provider.id = "test-model";
      name = "test-model";
      reasoning = false;
      input_types = [ "text" ];
      cost = (0.0, 0.0, 0.0, 0.0);
      context_window = 16384;
      max_tokens = 256;
    };
    max_tokens = 256;
    timeout = 1;
  }

let () =
  let messages = [
    {
      Llm_types.role = "assistant";
      content =
        Llm_types.Blocks [
          Llm_types.Text { text = "thinking" };
          Llm_types.Tool_use { id = "call-1"; name = "bash"; input = `Assoc [ ("command", `String "pwd") ] };
        ];
    };
    {
      Llm_types.role = "user";
      content =
        Llm_types.Blocks [
          Llm_types.Tool_result { tool_use_id = "call-1"; content = "/tmp"; is_error = None };
        ];
    };
  ] in
  let tools = [
    {
      Llm_types.name = "bash";
      description = "Run shell";
      input_schema = `Assoc [ ("type", `String "object") ];
    }
  ] in
  let json = Llm_provider.build_request_json provider ~system_prompt:"sys" messages ~tools ~stream:false in
  let request_messages = json |> Yojson.Safe.Util.member "messages" |> Yojson.Safe.Util.to_list in
  expect (List.length request_messages = 3) "expected system + assistant + tool messages";
  let assistant = List.nth request_messages 1 in
  let tool = List.nth request_messages 2 in
  expect ((assistant |> Yojson.Safe.Util.member "tool_calls") <> `Null) "assistant tool calls missing";
  expect (Yojson.Safe.Util.to_string (tool |> Yojson.Safe.Util.member "tool_call_id") = "call-1")
    "tool_call_id missing from tool result message";
  
  (* Test streaming parser with text *)
  let streamed_text = ref [] in
  let parser = Llm_provider.create_stream_parser ~on_text_delta:(fun delta -> streamed_text := delta :: !streamed_text) () in
  let chunks = [
    "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"},\"finish_reason\":null}]}\n\n";
    "data: {\"choices\":[{\"delta\":{\"content\":\"lo\"},\"finish_reason\":\"stop\"}]}\n\n";
    "data: [DONE]\n\n";
  ] in
  List.iter (fun chunk -> ignore (Llm_provider.feed_parser parser chunk)) chunks;
  begin
    match Llm_provider.finalize_parser parser with
    | Ok response ->
        expect (String.concat "" (List.rev !streamed_text) = "Hello") "text deltas should be emitted in order";
        begin
          match response.Llm_types.content with
          | [ Llm_types.Response_text { text } ] ->
              expect (text = "Hello") "streamed text should be reassembled"
          | _ -> fail "unexpected streamed text response shape"
        end
    | Error err -> fail err
  end;

  (* Test streaming parser with tool calls *)
  let parser = Llm_provider.create_stream_parser () in
  let chunks = [
    "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-1\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\"}}]},\"finish_reason\":null}]}\n\n";
    "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"note.txt\\\"}\"}}]},\"finish_reason\":\"tool_calls\"}]}\n\n";
    "data: [DONE]\n\n";
  ] in
  List.iter (fun chunk -> ignore (Llm_provider.feed_parser parser chunk)) chunks;
  begin
    match Llm_provider.finalize_parser parser with
    | Ok response ->
        begin
          match response.Llm_types.content with
          | [ Llm_types.Response_tool_use { id; name; input } ] ->
              expect (id = "call-1") "streamed tool id should be preserved";
              expect (name = "read_file") "streamed tool name should be reassembled";
              expect ((input |> Yojson.Safe.Util.member "path") = `String "note.txt")
                "streamed tool arguments should be reassembled"
          | _ -> fail "unexpected streamed tool response shape"
        end
    | Error err -> fail err
  end;
  Printf.printf "[PASS] llm provider request serialization\n"
