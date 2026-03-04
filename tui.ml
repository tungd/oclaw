(** TUI client for OClaw *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "tui") : Logs.LOG)

module Tui = struct
  let send_message ~server_address ~session_id ~content =
    let base_url = "http://" ^ server_address in
    let url = base_url ^ "/api/chat" in

    let request_body = Yojson.Basic.to_string (`Assoc [
      ("session_id", `String session_id);
      ("content", `String content);
    ]) in

    match Http_client.post url [] request_body 60 with
    | response ->
        (match response.Http_client.HttpResponse.error with
        | Some error -> Error ("HTTP error: " ^ error)
        | None ->
            try
              let json = Yojson.Basic.from_string response.Http_client.HttpResponse.body in
              let response_text = json |> member "response" |> to_string in
              Ok response_text
            with exn ->
              Error ("Parse error: " ^ Printexc.to_string exn))

  let run ~server_address ~session_id =
    Printf.printf "OClaw TUI - Connected to %s\n" server_address;
    Printf.printf "Session: %s\n" session_id;
    Printf.printf "Commands: /quit, /clear\n\n";
    flush stdout;

    let rec loop () =
      Printf.printf "> ";
      flush stdout;

      try
        let content = read_line () in

        (* Handle commands *)
        if content = "/quit" || content = "/exit" then begin
          Printf.printf "Goodbye!\n";
          exit 0
        end else if content = "/clear" then begin
          Printf.printf "Local history cleared.\n";
          loop ()
        end else if content = "" then begin
          loop ()
        end else begin
          (* Send chat request *)
          match send_message ~server_address ~session_id ~content with
          | Ok response ->
              Printf.printf "\n%s\n\n" response
          | Error err ->
              Printf.printf "Error: %s\n\n" err;
          ;

          loop ()
        end
      with End_of_file ->
        Printf.printf "\nGoodbye!\n";
        exit 0
    in

    loop ()
end
