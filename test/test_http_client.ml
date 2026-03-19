(** Tests for HTTP client module. *)

module Client = Httpkit.Http_client

let test_http_method_to_string () =
  let open Client.HttpMethod in
  assert (to_string GET = "GET");
  assert (to_string POST = "POST");
  assert (to_string PUT = "PUT");
  assert (to_string DELETE = "DELETE");
  print_endline "  ✓ http_method_to_string"

let test_http_method_from_string () =
  let open Client.HttpMethod in
  assert (of_string "GET" = Some GET);
  assert (of_string "POST" = Some POST);
  assert (of_string "INVALID" = None);
  assert (of_string "get" = None);  (* case sensitive *)
  print_endline "  ✓ http_method_from_string"

let test_request_creation () =
  let req = Client.HttpRequest.create ~method_:Client.HttpMethod.GET ~url:"http://example.com" () in
  assert (req.Client.HttpRequest.method_ = Client.HttpMethod.GET);
  assert (req.Client.HttpRequest.url = "http://example.com");
  assert (req.Client.HttpRequest.timeout = 30);  (* default *)
  print_endline "  ✓ request_creation"

let test_request_with_options () =
  let headers = [ "Authorization", "Bearer token" ] in
  let req = Client.HttpRequest.create 
    ~method_:Client.HttpMethod.POST 
    ~url:"http://api.example.com/data"
    ~headers
    ~body:"{\"key\": \"value\"}"
    ~timeout:60
    () in
  assert (req.Client.HttpRequest.method_ = Client.HttpMethod.POST);
  assert (List.length req.Client.HttpRequest.headers = 1);
  assert (req.Client.HttpRequest.body = Some "{\"key\": \"value\"}");
  assert (req.Client.HttpRequest.timeout = 60);
  print_endline "  ✓ request_with_options"

let test_response_creation () =
  let resp = Client.HttpResponse.create ~status:200 ~headers:[ "Content-Type", "application/json" ] ~body:"{}" () in
  assert (resp.Client.HttpResponse.status = 200);
  assert (Client.HttpResponse.is_success resp);
  assert (Client.HttpResponse.get_header resp "Content-Type" = Some "application/json");
  print_endline "  ✓ response_creation"

let test_response_is_success () =
  let success_resp = Client.HttpResponse.create ~status:200 ~headers:[] ~body:"" () in
  let not_found = Client.HttpResponse.create ~status:404 ~headers:[] ~body:"" () in
  let server_error = Client.HttpResponse.create ~status:500 ~headers:[] ~body:"" () in
  assert (Client.HttpResponse.is_success success_resp);
  assert (not (Client.HttpResponse.is_success not_found));
  assert (not (Client.HttpResponse.is_success server_error));
  print_endline "  ✓ response_is_success"

let () =
  print_endline "Running HTTP client tests...";
  test_http_method_to_string ();
  test_http_method_from_string ();
  test_request_creation ();
  test_request_with_options ();
  test_response_creation ();
  test_response_is_success ();
  print_endline "[PASS] all http_client tests ✓"
