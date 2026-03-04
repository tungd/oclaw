(** Memory and conversation history system for OClaw *)

open Yojson.Safe
open Yojson.Safe.Util

(* Message type for conversation history *)
type conversation_message = {
  role : string; (* "user" or "assistant" *)
  content : string;
  timestamp : float;
  importance : float; (* Importance score 0.0-1.0 *)
  estimated_tokens : int; (* Approximate token count *)
  metadata : (string * string) list
}

(* Conversation session *)
type conversation_session = {
  session_id : string;
  created_at : float;
  messages : conversation_message list;
  current_context : string list
}

(* Memory statistics *)
type memory_stats = {
  total_sessions : int;
  total_messages : int;
  avg_messages_per_session : int;
  estimated_tokens : int
}

(* Memory cleanup policy *)
type cleanup_policy = {
  max_age_seconds : float; (* Messages older than this may be pruned *)
  max_tokens : int; (* Approximate token limit for context *)
  max_messages : int; (* Hard limit on number of messages *)
  importance_threshold : float (* Messages with lower importance may be pruned *)
}

(* Default cleanup policy *)
let default_cleanup_policy = {
  max_age_seconds = 3600.0; (* 1 hour *)
  max_tokens = 4000; (* Approximate token limit *)
  max_messages = 50; (* Don't keep more than 50 messages *)
  importance_threshold = 0.3 (* Keep messages with importance >= 0.3 *)
}

(* Estimate token count for a message *)
let estimate_tokens text =
  (* Simple approximation: average 4 characters per token for English text *)
  max 1 (String.length text / 4)

(* Memory store *)
let memory_store = ref []

(* Create a new conversation session *)
let create_session session_id =
  {
    session_id = session_id;
    created_at = Unix.time ();
    messages = [];
    current_context = []
  }

(* Add a message to conversation history with cleanup policy *)
let add_message session message role cleanup_policy =
  let timestamp = Unix.time () in
  let importance = if role = "system" then 1.0 else 0.5 in
  let estimated_tokens = estimate_tokens message in
  let conversation_msg = {
    role = role;
    content = message;
    timestamp = timestamp;
    importance = importance;
    estimated_tokens = estimated_tokens;
    metadata = [("source", "user_input")]
  } in

  (* Apply cleanup policy *)
  let filtered_messages : conversation_message list = List.filter (fun msg ->
    (* Age filter: keep messages within max_age_seconds *)
    (Unix.time () -. msg.timestamp <= cleanup_policy.max_age_seconds) &&
    (* Importance filter: keep messages above threshold *)
    msg.importance >= cleanup_policy.importance_threshold
  ) session.messages in

  (* Token limit enforcement *)
  let rec limit_by_tokens (msgs : conversation_message list) current_tokens acc =
    match msgs with
    | [] -> List.rev acc
    | msg::rest when current_tokens + msg.estimated_tokens <= cleanup_policy.max_tokens ->
        limit_by_tokens rest (current_tokens + msg.estimated_tokens) (msg::acc)
    | _ -> List.rev acc
  in

  let token_limited = limit_by_tokens filtered_messages 0 [] in
  let final_messages = List.take cleanup_policy.max_messages token_limited in

  let updated_session =
    { session with
      messages = conversation_msg :: final_messages;
      current_context = if List.length session.current_context >= 5 then
                          List.tl session.current_context @ [message]
                        else
                          session.current_context @ [message]
    }
  in
  (* Update memory store *)
  memory_store := List.filter (fun s -> s.session_id <> session.session_id) !memory_store;
  memory_store := updated_session :: !memory_store;
  updated_session

(* Get conversation history *)
let get_history session_id limit =
  match List.find_opt (fun s -> s.session_id = session_id) !memory_store with
  | Some session ->
      let limited_messages = List.take limit session.messages in
      Some (List.rev limited_messages)
  | None -> None

(* Get current context *)
let get_context session_id =
  match List.find_opt (fun s -> s.session_id = session_id) !memory_store with
  | Some session -> Some session.current_context
  | None -> None

(* Clear conversation history *)
let clear_history session_id =
  memory_store := List.filter (fun s -> s.session_id <> session_id) !memory_store

(* Build conversation context for LLM with token limit *)
let build_context session_id system_prompt max_tokens =
  match get_history session_id 50 with (* Start with last 50 messages *)
  | Some messages ->
      (* Sort by timestamp (oldest first) *)
      let sorted_messages = List.sort (fun a b -> compare a.timestamp b.timestamp) messages in

      (* Calculate system prompt tokens *)
      let system_tokens = estimate_tokens system_prompt in
      let remaining_tokens = max_tokens - system_tokens in

      if remaining_tokens <= 0 then None else

      (* Filter messages to fit within token limit *)
      let rec select_messages (msgs : conversation_message list) current_tokens acc =
        match msgs with
        | [] -> List.rev acc
        | msg::rest when current_tokens + msg.estimated_tokens <= remaining_tokens ->
            select_messages rest (current_tokens + msg.estimated_tokens) (msg::acc)
        | _ -> List.rev acc
      in

      let selected_messages = select_messages sorted_messages 0 [] in

      let message_json_list = List.map (fun msg ->
        `Assoc [
          ("role", `String msg.role);
          ("content", `String msg.content)
        ]
      ) selected_messages in

      let context =
        `List ([`Assoc [("role", `String "system"); ("content", `String system_prompt)]] @ message_json_list)
      in
      Some context
  | None -> None

(* Save memory to file *)
let save_memory filename =
  try
    let json_list = List.map (fun session ->
      `Assoc [
        ("session_id", `String session.session_id);
        ("created_at", `Float session.created_at);
        ("messages", `List (List.map (fun msg ->
          `Assoc [
            ("role", `String msg.role);
            ("content", `String msg.content);
            ("timestamp", `Float msg.timestamp);
            ("importance", `Float msg.importance);
            ("estimated_tokens", `Int msg.estimated_tokens);
            ("metadata", `Assoc (List.map (fun (k, v) -> (k, `String v)) msg.metadata))
          ]
        ) session.messages))
      ]
    ) !memory_store in
    let json = `List json_list in
    let json_string = Yojson.Safe.to_string json in
    let channel = open_out filename in
    output_string channel json_string;
    close_out channel;
    true
  with exn ->
    Printf.printf "Error saving memory: %s\n" (Printexc.to_string exn);
    false

(* Load memory from file *)
let load_memory filename =
  try
    let channel = open_in filename in
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    let json = Yojson.Safe.from_string content in
    let session_list = Yojson.Safe.Util.to_list json in
    let loaded_sessions = List.map (fun session_json ->
      let session_id = Yojson.Safe.Util.member "session_id" session_json |> Yojson.Safe.Util.to_string in
      let created_at = Yojson.Safe.Util.member "created_at" session_json |> Yojson.Safe.Util.to_float in
      let messages_json = Yojson.Safe.Util.member "messages" session_json |> Yojson.Safe.Util.to_list in
      let messages = List.map (fun msg_json ->
        let role = Yojson.Safe.Util.member "role" msg_json |> Yojson.Safe.Util.to_string in
        let content = Yojson.Safe.Util.member "content" msg_json |> Yojson.Safe.Util.to_string in
        let timestamp = Yojson.Safe.Util.member "timestamp" msg_json |> Yojson.Safe.Util.to_float in
        let importance =
          try Yojson.Safe.Util.member "importance" msg_json |> Yojson.Safe.Util.to_float
          with _ -> 0.5 in
        let estimated_tokens =
          try Yojson.Safe.Util.member "estimated_tokens" msg_json |> Yojson.Safe.Util.to_int
          with _ -> estimate_tokens content in
        let metadata_json = Yojson.Safe.Util.member "metadata" msg_json in
        let metadata =
          try
            Yojson.Safe.Util.to_assoc metadata_json |> List.map (fun (k, v) -> (k, Yojson.Safe.Util.to_string v))
          with _ -> []
        in
        { role; content; timestamp; importance; estimated_tokens; metadata }
      ) messages_json in
      { session_id; created_at; messages; current_context = [] }
    ) session_list in
    memory_store := loaded_sessions;
    true
  with exn ->
    Printf.printf "Error loading memory: %s\n" (Printexc.to_string exn);
    false

(* Get session summary *)
let get_session_summary session_id =
  match List.find_opt (fun s -> s.session_id = session_id) !memory_store with
  | Some session ->
      Some (Printf.sprintf "Session %s: %d messages, created %.0f seconds ago"
        session.session_id
        (List.length session.messages)
        (Unix.time () -. session.created_at))
  | None -> None

(* List all active sessions *)
let list_sessions () =
  List.map (fun session ->
    (session.session_id, List.length session.messages)
  ) !memory_store

(* Get memory statistics *)
let get_stats () =
  let total_messages = List.fold_left (fun acc session -> acc + List.length session.messages) 0 !memory_store in
  let total_sessions = List.length !memory_store in
  let total_tokens = List.fold_left (fun acc session ->
    acc + List.fold_left (fun msg_acc (msg : conversation_message) -> msg_acc + msg.estimated_tokens) 0 session.messages
  ) 0 !memory_store in
  {
    total_sessions = total_sessions;
    total_messages = total_messages;
    avg_messages_per_session = if total_sessions > 0 then total_messages / total_sessions else 0;
    estimated_tokens = total_tokens
  }
