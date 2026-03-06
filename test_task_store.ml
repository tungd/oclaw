(** Integration test for SQLite-backed task store/service *)

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let temp_db_path () =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "oclaw-task-tests" in
  if not (Sys.file_exists base) then Unix.mkdir base 0o755;
  Filename.concat base (Printf.sprintf "tasks-%d-%f.db" (Unix.getpid ()) (Unix.gettimeofday ()))

let create_service db_path =
  let cfg : Task_store.config = {
    db_path;
    busy_timeout_ms = 5000;
    default_limit = 50;
    max_limit = 200;
    event_retention_days = 30;
  } in
  let service = Task_service.create cfg in
  (match Task_service.initialize service with
   | Ok () -> ()
   | Error err -> fail ("initialize failed: " ^ err));
  service

let create_task service title =
  let req : Task_types.task_create_request = {
    kind = Some "general";
    title;
    description = Some ("Task " ^ title);
    status = Some "open";
    priority = Some 2;
    assignee = None;
    created_by = Some "test";
    parent_id = None;
    session_id = Some "test-session";
  } in
  match Task_service.create_task service ~actor:"test" req with
  | Ok task -> task
  | Error err -> fail ("create_task failed: " ^ err)

let () =
  Random.self_init ();
  let db_path = temp_db_path () in
  let service = create_service db_path in

  let blocker = create_task service "blocker" in
  let blocked = create_task service "blocked" in

  (match Task_service.add_dependency service ~from_task_id:blocker.id ~to_task_id:blocked.id ~dep_type:"blocks" ~actor:"test" with
   | Ok _ -> ()
   | Error err -> fail ("add_dependency failed: " ^ err));

  let ready_before =
    match Task_service.ready_tasks service () with
    | Ok tasks -> tasks
    | Error err -> fail ("ready_tasks(before) failed: " ^ err)
  in
  expect (List.exists (fun t -> String.equal t.Task_types.id blocker.id) ready_before) "blocker should be ready";
  expect (not (List.exists (fun t -> String.equal t.Task_types.id blocked.id) ready_before)) "blocked task should not be ready";

  (match Task_service.close_task service ~id:blocker.id ~reason:(Some "done") ~actor:"test" with
   | Ok _ -> ()
   | Error err -> fail ("close_task failed: " ^ err));

  let ready_after =
    match Task_service.ready_tasks service () with
    | Ok tasks -> tasks
    | Error err -> fail ("ready_tasks(after) failed: " ^ err)
  in
  expect (List.exists (fun t -> String.equal t.Task_types.id blocked.id) ready_after) "blocked task should become ready after blocker closes";

  let claimed =
    match Task_service.claim_task service ~id:blocked.id ~assignee:(Some "agent-a") ~actor:"test" with
    | Ok task -> task
    | Error err -> fail ("claim_task failed: " ^ err)
  in
  expect (String.equal claimed.status "in_progress") "claimed task should be in_progress";

  let canceled =
    match Task_service.cancel_task service ~id:blocked.id ~actor:"test" with
    | Ok task -> task
    | Error err -> fail ("cancel_task failed: " ^ err)
  in
  expect (String.equal canceled.status "canceled") "canceled task should have status canceled";

  let service_after_restart = create_service db_path in
  let loaded =
    match Task_service.get_task service_after_restart blocked.id with
    | Ok (Some task) -> task
    | Ok None -> fail "task missing after restart"
    | Error err -> fail ("get_task(after restart) failed: " ^ err)
  in
  expect (String.equal loaded.status "canceled") "status should persist across restart";

  let events =
    match Task_service.list_events service_after_restart blocked.id () with
    | Ok xs -> xs
    | Error err -> fail ("list_events failed: " ^ err)
  in
  expect (List.length events >= 3) "expected at least create/claim/cancel events";

  Printf.printf "[PASS] task store integration test (%s)\n" db_path
