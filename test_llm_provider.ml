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
    temperature = 0.0;
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
  Printf.printf "[PASS] llm provider request serialization\n"
