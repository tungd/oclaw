(** Tests for HTTP client module. *)

module Client = Httpkit.Client

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let test_execute_request_missing_host () =
  let request = H1.Request.create `GET "http://127.0.0.1:9" in
  match Client.execute_request ~timeout:1 request with
  | Ok _ -> fail "expected transport failure"
  | Error _ -> print_endline "  ✓ execute_request_missing_host"

let test_h1_request_construction () =
  let headers = H1.Headers.of_list [ ("Content-Type", "application/json") ] in
  let request = H1.Request.create ~headers `POST "http://example.com" in
  expect (H1.Method.to_string request.H1.Request.meth = "POST") "method should be POST";
  expect (request.H1.Request.target = "http://example.com") "target should be preserved";
  expect (H1.Headers.get request.H1.Request.headers "Content-Type" = Some "application/json") "header should be preserved";
  print_endline "  ✓ h1_request_construction"

let () =
  print_endline "Running HTTP client tests...";
  test_h1_request_construction ();
  test_execute_request_missing_host ();
  print_endline "[PASS] all http_client tests ✓"
