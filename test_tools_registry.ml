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
  "web_search";
  "web_fetch";
  "todo_read";
  "todo_write";
  "activate_skill";
  "sync_skills";
  "schedule_task";
  "list_scheduled_tasks";
  "pause_scheduled_task";
  "resume_scheduled_task";
  "cancel_scheduled_task";
  "get_task_history";
  "export_chat";
]

let temp_dir () =
  let path = Filename.temp_file "oclaw-tools-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let () =
  let data_dir = temp_dir () in
  let db =
    match Db.create (Filename.concat (Filename.concat data_dir "runtime") "test.db") with
    | Ok db -> db
    | Error err -> fail err
  in
  let registry =
    Tools.create_default_registry
      ~data_dir
      ~skills_dir:(Filename.concat data_dir "skills")
      ~db
      ()
  in
  let actual_tools = Tools.definitions registry |> List.map (fun tool -> tool.Llm_types.name) in
  expect (actual_tools = expected_tools) "default tool registry does not match expected surface";
  Printf.printf "[PASS] tool registry smoke test (%d tools)\n" (List.length actual_tools)
