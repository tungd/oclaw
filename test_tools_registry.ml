let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let expected_tools = [
  "read_file";
  "write_file";
  "edit_file";
  "append_file";
  "execute_command";
  "exec";
  "list_directory";
  "list_dir";
]

let removed_tools = [
  "task_create";
  "task_list";
  "task_show";
  "task_update";
  "task_ready";
  "task_claim";
  "task_close";
  "task_cancel";
  "task_dep_add";
  "task_dep_list";
  "task_events";
  "spawn";
  "subagent_list";
  "subagent_manage";
  "web_search";
  "web_fetch";
  "find_skills";
  "install_skill";
  "cron";
]

let () =
  Tools.init_default_tools ();
  let actual_tools = Tools.get_all_tools () |> List.map fst in
  expect (actual_tools = expected_tools) "default tool registry does not match primitive surface";
  List.iter
    (fun name -> expect (not (List.mem name actual_tools)) ("unexpected legacy tool present: " ^ name))
    removed_tools;
  Printf.printf "[PASS] tool registry smoke test (%d tools)\n" (List.length actual_tools)
