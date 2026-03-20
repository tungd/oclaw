(** Tests for HTTP client module. *)

module Client = Httpkit.Client

let fail msg =
  Printf.eprintf "[FAIL] %s\n" msg;
  exit 1

let expect cond msg =
  if not cond then fail msg

let read_http_request client_sock =
  let ic = Unix.in_channel_of_descr client_sock in
  let request_line = input_line ic |> String.trim in
  let rec read_headers acc =
    let line = input_line ic |> String.trim in
    if line = "" then List.rev acc else read_headers (line :: acc)
  in
  let headers = read_headers [] in
  (request_line, headers)

let write_http_response client_sock ~status ~headers ~chunks =
  let oc = Unix.out_channel_of_descr client_sock in
  output_string oc (Printf.sprintf "HTTP/1.1 %d Test\r\n" status);
  List.iter
    (fun (name, value) -> output_string oc (Printf.sprintf "%s: %s\r\n" name value))
    headers;
  output_string oc "\r\n";
  List.iter (output_string oc) chunks;
  flush oc;
  close_out_noerr oc

let with_test_server handler f =
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close server_sock)
    (fun () ->
      Unix.setsockopt server_sock Unix.SO_REUSEADDR true;
      Unix.bind server_sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen server_sock 1;
      let port =
        match Unix.getsockname server_sock with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> fail "expected inet socket"
      in
      match Unix.fork () with
      | 0 ->
          begin
            try
              let client_sock, _ = Unix.accept server_sock in
              Unix.close server_sock;
              handler client_sock;
              exit 0
            with exn ->
              prerr_endline (Printexc.to_string exn);
              exit 1
          end
      | pid ->
          Fun.protect
            ~finally:(fun () ->
              match Unix.waitpid [] pid with
              | _, Unix.WEXITED 0 -> ()
              | _, Unix.WEXITED code ->
                  fail (Printf.sprintf "test server exited with code %d" code)
              | _, Unix.WSIGNALED signal ->
                  fail (Printf.sprintf "test server killed by signal %d" signal)
              | _, Unix.WSTOPPED signal ->
                  fail (Printf.sprintf "test server stopped by signal %d" signal))
            (fun () -> f port))

let test_execute_request_round_trip () =
  with_test_server
    (fun client_sock ->
      let request_line, headers = read_http_request client_sock in
      expect (request_line = "POST /echo HTTP/1.1") "request line should include method and path";
      expect
        (List.exists
           (fun line -> String.lowercase_ascii line = "content-type: application/json")
           headers)
        "request should preserve content-type";
      expect
        (List.exists
           (fun line -> String.lowercase_ascii line = "user-agent: openai/go 3.22.0")
           headers)
        "request should add default user-agent";
      expect
        (List.exists
           (fun line -> String.lowercase_ascii line = "content-length: 13")
           headers)
        "request should set content-length for the body";
      write_http_response
        client_sock
        ~status:201
        ~headers:
          [
            ("Content-Type", "application/json");
            ("X-Test", "ok");
            ("Content-Length", "16");
          ]
        ~chunks:[ "{\"ok\":true}\n    " ])
    (fun port ->
      let headers = H1.Headers.of_list [ ("Content-Type", "application/json") ] in
      let request =
        H1.Request.create
          ~headers
          `POST
          (Printf.sprintf "http://127.0.0.1:%d/echo" port)
      in
      let chunks = ref [] in
      match
        Client.execute_request
          ~body:"{\"ping\":true}"
          ~timeout:2
          ~on_write:(fun chunk -> chunks := chunk :: !chunks)
          request
      with
      | Error err -> fail ("expected success but got error: " ^ err)
      | Ok (response, body) ->
          expect (H1.Status.to_code response.H1.Response.status = 201) "status should be preserved";
          expect (body = "{\"ok\":true}\n    ") "response body should be preserved exactly";
          expect
            (H1.Headers.get response.H1.Response.headers "X-Test" = Some "ok")
            "response headers should be preserved";
          expect (String.concat "" (List.rev !chunks) = body) "on_write should receive streamed chunks";
          print_endline "  ✓ execute_request_round_trip")

let test_execute_request_missing_host () =
  let request = H1.Request.create `GET "http://127.0.0.1:9" in
  match Client.execute_request ~timeout:1 request with
  | Ok _ -> fail "expected transport failure"
  | Error _ -> print_endline "  ✓ execute_request_missing_host"

let () =
  print_endline "Running HTTP client tests...";
  test_execute_request_round_trip ();
  test_execute_request_missing_host ();
  print_endline "[PASS] all http_client tests ✓"
