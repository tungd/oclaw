let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expected_tools = [
  "bash";
  "read_file";
  "write_file";
  "edit_file";
]

let () =
  let registry = Agent_tools.Tools.create_default_registry () in
  let actual_tools = Agent_tools.Tools.definitions registry |> List.map (fun tool -> tool.Llm_types.name) in
  expect (actual_tools = expected_tools) "default tool registry does not match expected surface";
  Printf.printf "[PASS] tool registry smoke test (%d tools)\n" (List.length actual_tools)
