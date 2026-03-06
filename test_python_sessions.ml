open Yojson.Safe
open Yojson.Safe.Util

let fail msg =
  prerr_endline ("FAIL: " ^ msg);
  exit 1

let expect cond msg =
  if not cond then fail msg

let parse_json s =
  try Yojson.Safe.from_string s
  with exn -> fail ("expected JSON, got: " ^ s ^ " (" ^ Printexc.to_string exn ^ ")")

let run_tool name args =
  Tools.execute_tool name (`Assoc args)

let get_required_string json key =
  match json |> member key with
  | `String s -> s
  | _ -> fail ("missing string field: " ^ key)

let get_required_bool json key =
  match json |> member key with
  | `Bool b -> b
  | _ -> fail ("missing bool field: " ^ key)

let get_required_list json key =
  match json |> member key with
  | `List xs -> xs
  | _ -> fail ("missing list field: " ^ key)

let string_contains ~haystack ~needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then true
  else if n_len > h_len then false
  else
    let rec loop i =
      if i > h_len - n_len then false
      else if String.sub haystack i n_len = needle then true
      else loop (i + 1)
    in
    loop 0

let () =
  let sandbox = {
    Tools.default_sandbox_config with
    python_sessions_enabled = true;
    python_session_idle_ttl_seconds = 1200;
    python_session_max_count = 4;
    python_timeout_seconds = 10;
    python_max_output_chars = 20000;
    python_max_code_chars = 20000;
  } in
  Tools.init_default_tools ~sandbox_config:sandbox ();

  let start_1 = run_tool "python_session_start" [("label", `String "s1")] |> parse_json in
  let s1 = get_required_string start_1 "session_id" in
  expect (s1 <> "") "session_id should not be empty";

  let list_1 = run_tool "python_session_list" [] |> parse_json in
  let sessions = get_required_list list_1 "sessions" in
  expect (List.length sessions >= 1) "python_session_list should include at least one session";

  let run_a =
    run_tool "python_session_run"
      [ ("session_id", `String s1); ("code", `String "x = 41") ]
    |> parse_json
  in
  expect (get_required_bool run_a "ok") "setting variable in session should succeed";

  let run_b =
    run_tool "python_session_run"
      [ ("session_id", `String s1); ("code", `String "print(x + 1)") ]
    |> parse_json
  in
  expect (get_required_bool run_b "ok") "reading persisted variable in same session should succeed";
  let stdout_b = get_required_string run_b "stdout" in
  expect (string_contains ~haystack:stdout_b ~needle:"42") "expected stdout to contain 42";

  let start_2 = run_tool "python_session_start" [("label", `String "s2")] |> parse_json in
  let s2 = get_required_string start_2 "session_id" in
  let run_c =
    run_tool "python_session_run"
      [ ("session_id", `String s2); ("code", `String "print(globals().get('x'))") ]
    |> parse_json
  in
  expect (get_required_bool run_c "ok") "isolated session command should succeed";
  let stdout_c = get_required_string run_c "stdout" in
  expect (string_contains ~haystack:stdout_c ~needle:"None") "x should not exist in a separate session";

  let run_blocked =
    run_tool "python_session_run"
      [ ("session_id", `String s1); ("code", `String "import ctypes") ]
    |> parse_json
  in
  expect (not (get_required_bool run_blocked "ok")) "blocked import should fail";

  ignore (run_tool "python_session_end" [("session_id", `String s1)] |> parse_json);
  ignore (run_tool "python_session_end" [("session_id", `String s2)] |> parse_json);

  print_endline "PASS: python session tools smoke test"
