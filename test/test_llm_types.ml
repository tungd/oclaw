let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expect_ok = function
  | Ok value -> value
  | Error err -> fail err

let () =
  let open Llm_types in

  let tool_definition_json =
    tool_definition_to_yojson {
      name = "bash";
      description = "Run shell";
      input_schema = `Assoc [ ("type", `String "object") ];
    }
  in
  expect
    (tool_definition_json
     = `Assoc [
         ("name", `String "bash");
         ("description", `String "Run shell");
         ("parameters", `Assoc [ ("type", `String "object") ]);
       ])
    "tool_definition should serialize input_schema as parameters";

  let text_json =
    content_block_to_yojson (Text { text = "hello" })
  in
  expect
    (text_json = `Assoc [ ("type", `String "text"); ("text", `String "hello") ])
    "text block should use OpenAI object shape";

  let tool_use_json =
    content_block_to_yojson
      (Tool_use {
         id = "call-1";
         name = "bash";
         input = `Assoc [ ("command", `String "pwd") ];
       })
  in
  expect
    (tool_use_json
     = `Assoc [
         ("type", `String "tool_use");
         ("id", `String "call-1");
         ("name", `String "bash");
         ("input", `Assoc [ ("command", `String "pwd") ]);
       ])
    "tool_use block should preserve raw input JSON";

  let tool_result_json =
    content_block_to_yojson
      (Tool_result {
         tool_use_id = "call-1";
         content = "ok";
         is_error = Some true;
       })
  in
  expect
    (tool_result_json
     = `Assoc [
         ("type", `String "tool_result");
         ("tool_use_id", `String "call-1");
         ("content", `String "ok");
         ("is_error", `Bool true);
       ])
    "tool_result block should emit optional bool directly";

  let text_content_json =
    message_content_to_yojson (Text_content "hello")
  in
  expect (text_content_json = `String "hello")
    "message_content text should serialize as a bare string";

  let blocks_content_json =
    message_content_to_yojson
      (Blocks [ Text { text = "hello" }; Text { text = "world" } ])
  in
  expect
    (blocks_content_json
     = `List [
         `Assoc [ ("type", `String "text"); ("text", `String "hello") ];
         `Assoc [ ("type", `String "text"); ("text", `String "world") ];
       ])
    "message_content blocks should serialize as a block array";

  let parsed_null = expect_ok (message_content_of_yojson `Null) in
  expect (parsed_null = Text_content "")
    "message_content parser should keep null compatibility";

  let response_json =
    messages_response_to_yojson {
      content = [
        Response_tool_use {
          id = "call-2";
          name = "read_file";
          input = `Assoc [ ("path", `String "note.txt") ];
        }
      ];
      stop_reason = Some "tool_use";
      usage = Some { input_tokens = 1; output_tokens = 2 };
    }
  in
  expect
    (response_json
     = `Assoc [
         ("content", `List [
            `Assoc [
              ("type", `String "tool_use");
              ("id", `String "call-2");
              ("name", `String "read_file");
              ("input", `Assoc [ ("path", `String "note.txt") ]);
            ];
          ]);
         ("stop_reason", `String "tool_use");
         ("usage", `Assoc [ ("input_tokens", `Int 1); ("output_tokens", `Int 2) ]);
       ])
    "messages_response should keep exact response block shape";

  let round_trip =
    expect_ok
      (message_of_yojson
         (`Assoc [
            ("role", `String "assistant");
            ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String "hello") ] ]);
          ]))
  in
  expect
    (round_trip = { role = "assistant"; content = Blocks [ Text { text = "hello" } ] })
    "message should round-trip block content";

  Printf.printf "[PASS] llm types tests\n"
