(** Telegram Bot API client for OClaw *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "telegram_api") : Logs.LOG)

(* Helper function to get optional field *)
let get_option json key f =
  try Some (f (member key json))
  with Not_found -> None

(* Helper function to get optional string field *)
let get_option_string json key =
  try Some (to_string (member key json))
  with Not_found -> None

(* User type *)
module User = struct
  type t = {
    id : int64;
    is_bot : bool;
    first_name : string;
    last_name : string;
    username : string option;
    language_code : string option;
  }

  let of_json json =
    let is_bot = try json |> member "is_bot" |> to_bool with Not_found -> false in
    let last_name = get_option_string json "last_name" |> Option.value ~default:"" in
    let username = get_option_string json "username" in
    let language_code = get_option_string json "language_code" in
    {
      id = Int64.of_float (json |> member "id" |> to_float);
      is_bot;
      first_name = json |> member "first_name" |> to_string;
      last_name;
      username;
      language_code;
    }

  let to_json user =
    `Assoc [
      ("id", `Int (Int64.to_int user.id));
      ("is_bot", `Bool user.is_bot);
      ("first_name", `String user.first_name);
      ("last_name", `String user.last_name);
      ("username", (match user.username with Some s -> `String s | None -> `Null));
      ("language_code", (match user.language_code with Some s -> `String s | None -> `Null));
    ]
end

(* Chat type *)
module Chat = struct
  type t = {
    id : int64;
    type_ : string;
    title : string option;
    username : string option;
    first_name : string option;
    last_name : string option;
  }

  let of_json json =
    {
      id = Int64.of_float (json |> member "id" |> to_float);
      type_ = json |> member "type" |> to_string;
      title = get_option_string json "title";
      username = get_option_string json "username";
      first_name = get_option_string json "first_name";
      last_name = get_option_string json "last_name";
    }

  let to_json chat =
    `Assoc [
      ("id", `Int (Int64.to_int chat.id));
      ("type", `String chat.type_);
      ("title", (match chat.title with Some s -> `String s | None -> `Null));
      ("username", (match chat.username with Some s -> `String s | None -> `Null));
      ("first_name", (match chat.first_name with Some s -> `String s | None -> `Null));
      ("last_name", (match chat.last_name with Some s -> `String s | None -> `Null));
    ]
end

(* Message type *)
module Message = struct
  type t = {
    message_id : int;
    from : User.t option;
    chat : Chat.t;
    date : int;
    text : string option;
  }

  let of_json json =
    let from =
      try Some (User.of_json (json |> member "from"))
      with Not_found -> None
    in
    {
      message_id = json |> member "message_id" |> to_int;
      from;
      chat = Chat.of_json (json |> member "chat");
      date = json |> member "date" |> to_int;
      text = get_option_string json "text";
    }

  let to_json msg =
    `Assoc [
      ("message_id", `Int msg.message_id);
      ("from", (match msg.from with Some u -> User.to_json u | None -> `Null));
      ("chat", Chat.to_json msg.chat);
      ("date", `Int msg.date);
      ("text", (match msg.text with Some s -> `String s | None -> `Null));
    ]
end

(* Update type *)
module Update = struct
  type t = {
    update_id : int;
    message : Message.t option;
  }

  let of_json json =
    {
      update_id = json |> member "update_id" |> to_int;
      message =
        try Some (Message.of_json (json |> member "message"))
        with Not_found -> None;
    }

  let to_json upd =
    `Assoc [
      ("update_id", `Int upd.update_id);
      ("message", (match upd.message with Some m -> Message.to_json m | None -> `Null));
    ]
end

(* Bot API client *)
module Bot = struct
  type t = {
    token : string;
    base_url : string;
  }

  let create ?(base_url="https://api.telegram.org") token =
    { token; base_url }

  let api_url bot method_ =
    bot.base_url ^ "/bot" ^ bot.token ^ "/" ^ method_

  let http_request ?body ?headers url =
    match body with
    | None ->
        Ok (Http_client.get url [] 30)
    | Some b ->
        Ok (Http_client.post url [] b 30)

  let get_updates bot ~offset ~timeout =
    let url = api_url bot "getUpdates" in
    let params = Printf.sprintf "?offset=%d&timeout=%d" offset timeout in
    let full_url = url ^ params in

    match http_request full_url with
    | Ok response ->
        (match response.Http_client.HttpResponse.error with
        | Some error -> Error ("HTTP error: " ^ error)
        | None ->
            let json = Yojson.Basic.from_string response.Http_client.HttpResponse.body in
            let ok = json |> member "ok" |> to_bool in
            if ok then
              try
                let result_json = json |> member "result" in
                let updates_json = result_json |> to_list in
                Ok (List.map Update.of_json updates_json)
              with exn ->
                Error ("Parse error: " ^ Printexc.to_string exn)
            else
              let description = try json |> member "description" |> to_string with _ -> "Unknown error" in
              Error description)
    | Error error -> Error ("Request failed: " ^ error)

  let send_message bot ~chat_id ~text =
    let url = api_url bot "sendMessage" in
    let body = Yojson.Basic.to_string (`Assoc [
      ("chat_id", `Int (Int64.to_int chat_id));
      ("text", `String text);
    ]) in

    match http_request ~body url with
    | Ok response ->
        (match response.Http_client.HttpResponse.error with
        | Some error -> Error ("HTTP error: " ^ error)
        | None ->
            let json = Yojson.Basic.from_string response.Http_client.HttpResponse.body in
            let ok = json |> member "ok" |> to_bool in
            if ok then Ok ()
            else
              let description = try json |> member "description" |> to_string with _ -> "Unknown error" in
              Error description)
    | Error error -> Error ("Request failed: " ^ error)

  let get_me bot =
    let url = api_url bot "getMe" in

    match http_request url with
    | Ok response ->
        (match response.Http_client.HttpResponse.error with
        | Some error -> Error ("HTTP error: " ^ error)
        | None ->
            let json = Yojson.Basic.from_string response.Http_client.HttpResponse.body in
            let ok = json |> member "ok" |> to_bool in
            if ok then
              try
                let result_json = json |> member "result" in
                Ok (User.of_json result_json)
              with exn ->
                Error ("Parse error: " ^ Printexc.to_string exn)
            else
              let description = try json |> member "description" |> to_string with _ -> "Unknown error" in
              Error description)
    | Error error -> Error ("Request failed: " ^ error)
end
