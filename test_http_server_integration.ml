(* Integration smoke tests for h1 + iomux HTTP server *)

let fail msg =
  Printf.eprintf "FAIL: %s\n" msg;
  exit 1

let assert_true label condition =
  if not condition then fail label

let assert_status label expected response =
  match response.Http_client.HttpResponse.error with
  | Some err -> fail (Printf.sprintf "%s: HTTP error: %s" label err)
  | None ->
      if response.Http_client.HttpResponse.status <> expected then
        fail (Printf.sprintf "%s: expected status %d but got %d"
          label expected response.Http_client.HttpResponse.status)

let wait_until_ready ~url ~attempts =
  let rec loop n =
    if n <= 0 then false
    else
      let response = Http_client.get url [] 2 in
      match response.Http_client.HttpResponse.error with
      | None when response.Http_client.HttpResponse.status = 200 -> true
      | _ ->
          Unix.sleepf 0.1;
          loop (n - 1)
  in
  loop attempts

let read_raw_response fd =
  let buf = Bytes.create 4096 in
  let n = Unix.read fd buf 0 (Bytes.length buf) in
  if n <= 0 then "" else Bytes.sub_string buf 0 n

let test_malformed_request () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with _ -> ())
    (fun () ->
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 18081));
      let payload = "GET / HTP/1.1\r\nHost: localhost\r\n\r\n" in
      ignore (Unix.write_substring fd payload 0 (String.length payload));
      Unix.shutdown fd Unix.SHUTDOWN_SEND;
      let response = read_raw_response fd in
      assert_true "Malformed request should return 400"
        (String.starts_with ~prefix:"HTTP/1.1 400" response))

let () =
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ());

  let server = Http_server.create ~host:"127.0.0.1" ~port:18081 ~max_connections:32 () in

  Http_server.add_route server
    ~method_:(Some `GET)
    ~match_type:Http_server.Exact
    "/api/health"
    (fun reqd ->
      Http_server.respond_json reqd (`Assoc [ ("status", `String "ok") ]));

  Http_server.add_route server
    ~method_:(Some `POST)
    ~match_type:Http_server.Exact
    "/echo"
    (fun reqd ->
      let body_reader = Http_server.Reqd.request_body reqd in
      Http_server.read_body body_reader (function
        | Ok body -> Http_server.respond_text reqd body
        | Error (`Too_large _) -> Http_server.respond_text ~status:`Payload_too_large reqd "too large"
        | Error (`Exception _) -> Http_server.respond_text ~status:`Bad_request reqd "bad body"));

  ignore (Http_server.start server);

  Fun.protect
    ~finally:(fun () -> Http_server.stop server)
    (fun () ->
      assert_true "Server did not become ready"
        (wait_until_ready ~url:"http://127.0.0.1:18081/api/health" ~attempts:50);

      let health = Http_client.get "http://127.0.0.1:18081/api/health" [] 5 in
      assert_status "GET /api/health" 200 health;

      let missing = Http_client.get "http://127.0.0.1:18081/missing" [] 5 in
      assert_status "GET /missing" 404 missing;

      let echo = Http_client.post
        "http://127.0.0.1:18081/echo"
        [ ("Content-Type", "application/json") ]
        "{\"message\":\"hello\"}"
        5
      in
      assert_status "POST /echo" 200 echo;
      assert_true "POST /echo should include request body"
        (String.equal echo.Http_client.HttpResponse.body "{\"message\":\"hello\"}");

      test_malformed_request ();

      Printf.printf "PASS: http_server integration smoke tests\n")
