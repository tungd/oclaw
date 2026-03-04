(** Session management for OClaw server *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "session") : Logs.LOG)

(* Message type *)
module Message = struct
  type t = {
    role : string;
    content : string;
    timestamp : float;
  }

  let create ~role ~content =
    { role; content; timestamp = Unix.gettimeofday () }

  let to_json msg =
    `Assoc [
      ("role", `String msg.role);
      ("content", `String msg.content);
      ("timestamp", `Float msg.timestamp);
    ]

  let of_json json =
    {
      role = json |> member "role" |> to_string;
      content = json |> member "content" |> to_string;
      timestamp = json |> member "timestamp" |> to_float;
    }
end

(* Session type *)
module Session = struct
  type t = {
    id : string;
    directory : string;
    mutable messages : Message.t list;
    created_at : float;
    mutable last_active : float;
    mutex : Mutex.t;
  }

  let create ~id ~directory =
    {
      id;
      directory;
      messages = [];
      created_at = Unix.gettimeofday ();
      last_active = Unix.gettimeofday ();
      mutex = Mutex.create ();
    }

  let id session = session.id

  let directory session = session.directory

  let add_message session msg =
    Mutex.lock session.mutex;
    session.messages <- msg :: session.messages;
    Mutex.unlock session.mutex

  let get_history session ~limit =
    Mutex.lock session.mutex;
    let all = List.rev session.messages in
    let result =
      if List.length all > limit then
        let n = List.length all in
        let rec skip lst i =
          if i <= 0 then lst
          else match lst with
          | _ :: rest -> skip rest (i - 1)
          | [] -> []
        in
        skip all (n - limit)
      else all
    in
    Mutex.unlock session.mutex;
    result

  let touch session =
    Mutex.lock session.mutex;
    session.last_active <- Unix.gettimeofday ();
    Mutex.unlock session.mutex

  let created_at session =
    session.created_at

  let last_active session =
    Mutex.lock session.mutex;
    let result = session.last_active in
    Mutex.unlock session.mutex;
    result

  let message_count session =
    Mutex.lock session.mutex;
    let count = List.length session.messages in
    Mutex.unlock session.mutex;
    count
end

(* Session Manager *)
module Manager = struct
  type t = {
    sessions : (string, Session.t) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create () =
    { sessions = Hashtbl.create 16; mutex = Mutex.create () }

  let create_session mgr ~id ~directory =
    Mutex.lock mgr.mutex;
    let session = Session.create ~id ~directory in
    Hashtbl.add mgr.sessions id session;
    Mutex.unlock mgr.mutex;
    Log.debug (fun m -> m "Created session: %s" id);
    session

  let get_session mgr ~id =
    Mutex.lock mgr.mutex;
    let result = Hashtbl.find_opt mgr.sessions id in
    Mutex.unlock mgr.mutex;
    result

  let list_sessions mgr =
    Mutex.lock mgr.mutex;
    let result = Hashtbl.fold (fun _ v acc -> v :: acc) mgr.sessions [] in
    Mutex.unlock mgr.mutex;
    result

  let remove_session mgr ~id =
    Mutex.lock mgr.mutex;
    Hashtbl.remove mgr.sessions id;
    Mutex.unlock mgr.mutex;
    Log.debug (fun m -> m "Removed session: %s" id)

  let cleanup_old_sessions mgr ~max_age =
    let now = Unix.gettimeofday () in
    Mutex.lock mgr.mutex;
    let to_remove = ref [] in
    Hashtbl.iter (fun id session ->
      let last = Session.last_active session in
      if now -. last > max_age then
        to_remove := id :: !to_remove
    ) mgr.sessions;
    List.iter (fun id -> Hashtbl.remove mgr.sessions id) !to_remove;
    let count = List.length !to_remove in
    Mutex.unlock mgr.mutex;
    if count > 0 then
      Log.info (fun m -> m "Cleaned up %d old sessions" count)
end
