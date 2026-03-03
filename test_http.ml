(* Test file for HTTP client functionality *)

let test_single_request () =
  Printf.printf "Testing single HTTP request...\n";
  let response = Http_client.get "https://api.github.com" [] 30 in
  match response.Http_client.HttpResponse.error with
  | Some error -> Printf.printf "Error: %s\n" error
  | None -> Printf.printf "Success: Status %d\n" response.Http_client.HttpResponse.status

let test_multi_requests () =
  Printf.printf "Testing multi HTTP requests...\n";
  let requests = [
    Http_client.HttpRequest.create ~method_:Http_client.HttpMethod.GET ~url:"https://api.github.com" ~timeout:30 ();
    Http_client.HttpRequest.create ~method_:Http_client.HttpMethod.GET ~url:"https://api.github.com/users/octocat" ~timeout:30 ()
  ] in
  
  let responses = Http_client.perform_multi_requests requests in
  List.iteri (fun i response ->
    match response.Http_client.HttpResponse.error with
    | Some error -> Printf.printf "Request %d Error: %s\n" i error
    | None -> Printf.printf "Request %d Success: Status %d\n" i response.Http_client.HttpResponse.status
  ) responses

let test_post_request () =
  Printf.printf "Testing POST HTTP request...\n";
  let response = Http_client.post "https://httpbin.org/post" ["Content-Type", "application/json"] "{\"test\": \"data\"}" 30 in
  match response.Http_client.HttpResponse.error with
  | Some error -> Printf.printf "Error: %s\n" error
  | None -> Printf.printf "Success: Status %d\n" response.Http_client.HttpResponse.status

let () =
  test_single_request ();
  test_multi_requests ();
  test_post_request ();
  Printf.printf "All tests completed.\n"