let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let test_parse_once () =
  match Schedule_spec.parse_once "2099-01-01T09:30:00" with
  | Ok timestamp -> expect (timestamp > Unix.gettimeofday ()) "expected future parsed timestamp"
  | Error err -> fail ("parse_once failed: " ^ err)

let test_validate_cron () =
  begin
    match Schedule_spec.validate_cron "15 9 * * 1,3,5" with
    | Ok () -> ()
    | Error err -> fail ("valid cron rejected: " ^ err)
  end;
  begin
    match Schedule_spec.validate_cron "99 9 * * *" with
    | Ok () -> fail "invalid cron should fail"
    | Error _ -> ()
  end

let test_next_cron_after () =
  match Schedule_spec.next_cron_after "0 10 * * *" ~after:(Unix.gettimeofday ()) with
  | Ok timestamp -> expect (timestamp > Unix.gettimeofday ()) "expected next cron timestamp in the future"
  | Error err -> fail ("next_cron_after failed: " ^ err)

let () =
  test_parse_once ();
  test_validate_cron ();
  test_next_cron_after ();
  Printf.printf "[PASS] schedule spec tests\n"
