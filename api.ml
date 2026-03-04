(** API endpoint handlers for OClaw *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "api") : Logs.LOG)

(* Configuration passed to API handlers *)
type handler_config = {
  llm_config : Llm_provider.provider_config;
  knowledge : Knowledge.Knowledge.t;
  session_manager : Session.Manager.t;
}

(* API handlers *)
module Api = struct
  (* Build LLM messages from session history *)
  let build_messages session_manager knowledge session_id user_content =
    let session = match Session.Manager.get_session session_manager ~id:session_id with
    | Some s -> s
    | None -> Session.Manager.create_session session_manager ~id:session_id ~directory:"."
    in

    (* Get session history *)
    let history = Session.Session.get_history session ~limit:50 in

    (* Convert history to LLM messages *)
    let history_messages = List.map (fun (msg : Session.Message.t) ->
      let role = match msg.Session.Message.role with
      | "user" -> Llm_provider.User
      | "assistant" -> Llm_provider.Assistant
      | "tool" -> Llm_provider.Tool
      | _ -> Llm_provider.User
      in
      {
        Llm_provider.role;
        content = msg.Session.Message.content;
        Llm_provider.tool_call_id = None;
        Llm_provider.tool_calls = [];
      }
    ) history in

    (* Build system message *)
    let system_prompt = Knowledge.Knowledge.get_system_prompt knowledge in
    let system_message = {
      Llm_provider.role = Llm_provider.System;
      content = system_prompt;
      Llm_provider.tool_call_id = None;
      Llm_provider.tool_calls = [];
    } in

    (* User message *)
    let user_message = {
      Llm_provider.role = Llm_provider.User;
      content = user_content;
      Llm_provider.tool_call_id = None;
      Llm_provider.tool_calls = [];
    } in

    system_message :: history_messages @ [user_message]

  (* POST /api/chat - Send a message and get response *)
  let chat_post config req =
    try
      let path = req.Http_server.Request.path in
      let session_id =
        try String.sub path 10 (String.length path - 10)
        with _ -> "default"
      in

      let json = Yojson.Basic.from_string req.Http_server.Request.body in
      let content = json |> member "content" |> to_string in

      Log.debug (fun m -> m "Chat request from session: %s" session_id);

      (* Build messages *)
      let messages = build_messages config.session_manager config.knowledge session_id content in

      (* Call LLM *)
      let result = Llm_provider.call_llm config.llm_config messages () in

      (match result with
      | Llm_provider.Error err ->
          Log.warn (fun m -> m "LLM error: %s" err);
          Http_server.Response.json ~status:500 (`Assoc [
            ("error", `String err);
          ])
      | Llm_provider.Success response ->
          (match response.Llm_provider.choices with
          | [] ->
              Http_server.Response.json ~status:500 (`Assoc [
                ("error", `String "No choices in LLM response");
              ])
          | choice :: _ ->
              let response_text = choice.Llm_provider.message.Llm_provider.content in

              (* Add messages to session *)
              let session = match Session.Manager.get_session config.session_manager ~id:session_id with
              | Some s -> s
              | None -> Session.Manager.create_session config.session_manager ~id:session_id ~directory:"."
              in

              (* Add user message *)
              Session.Session.add_message session (Session.Message.create ~role:"user" ~content:content);

              (* Add assistant response *)
              Session.Session.add_message session (Session.Message.create ~role:"assistant" ~content:response_text);

              Session.Session.touch session;

              Http_server.Response.json (`Assoc [
                ("response", `String response_text);
                ("session_id", `String session_id);
              ])))

    with exn ->
      Log.warn (fun m -> m "Chat handler error: %s" (Printexc.to_string exn));
      Http_server.Response.string ~status:400 "Bad Request"

  (* GET /api/sessions - List all sessions *)
  let sessions_get config _req =
    try
      let sessions = Session.Manager.list_sessions config.session_manager in

      let json_list = List.map (fun (session : Session.Session.t) ->
        `Assoc [
          ("id", `String (Session.Session.id session));
          ("directory", `String (Session.Session.directory session));
          ("created", `Float (Session.Session.created_at session));
          ("last_active", `Float (Session.Session.last_active session));
          ("message_count", `Int (Session.Session.message_count session));
        ]
      ) sessions in

      Http_server.Response.json (`Assoc [
        ("sessions", `List json_list);
        ("count", `Int (List.length sessions));
      ])
    with exn ->
      Log.warn (fun m -> m "Sessions handler error: %s" (Printexc.to_string exn));
      Http_server.Response.json ~status:500 (`Assoc [
        ("error", `String (Printexc.to_string exn));
      ])

  (* DELETE /api/sessions/:id - Delete a session *)
  let session_delete config req =
    try
      let path = req.Http_server.Request.path in
      let session_id = String.sub path 15 (String.length path - 15) in
      Session.Manager.remove_session config.session_manager ~id:session_id;
      Http_server.Response.json (`Assoc [
        ("success", `Bool true);
        ("message", `String "Session deleted");
      ])
    with exn ->
      Log.warn (fun m -> m "Session delete error: %s" (Printexc.to_string exn));
      Http_server.Response.json ~status:500 (`Assoc [
        ("error", `String (Printexc.to_string exn));
      ])

  (* GET /api/knowledge - Get knowledge store info *)
  let knowledge_get config _req =
    try
      let identity = Knowledge.Knowledge.get_identity config.knowledge in
      let skills = Knowledge.Knowledge.get_skills config.knowledge in
      let last_updated = Knowledge.Knowledge.last_updated config.knowledge in

      Http_server.Response.json (`Assoc [
        ("identity", identity);
        ("skills", `List skills);
        ("skill_count", `Int (List.length skills));
        ("last_updated", `Float last_updated);
      ])
    with exn ->
      Log.warn (fun m -> m "Knowledge handler error: %s" (Printexc.to_string exn));
      Http_server.Response.json ~status:500 (`Assoc [
        ("error", `String (Printexc.to_string exn));
      ])

  (* POST /api/knowledge/refresh - Refresh knowledge from workspace *)
  let knowledge_refresh_post config _req =
    try
      (* Get workspace directory from config or default *)
      let home_dir = try Sys.getenv "HOME" with Not_found -> "." in
      let workspace_dir = Filename.concat home_dir ".oclaw/workspace" in

      Knowledge.Knowledge.refresh config.knowledge ~workspace_dir;

      Http_server.Response.json (`Assoc [
        ("success", `Bool true);
        ("message", `String "Knowledge refreshed");
      ])
    with exn ->
      Log.warn (fun m -> m "Knowledge refresh error: %s" (Printexc.to_string exn));
      Http_server.Response.json ~status:500 (`Assoc [
        ("error", `String (Printexc.to_string exn));
      ])

  (* GET /api/health - Health check endpoint *)
  let health_get _config _req =
    Http_server.Response.json (`Assoc [
      ("status", `String "ok");
      ("timestamp", `Float (Unix.gettimeofday ()));
    ])
end

(* Register all API routes *)
let register_routes server config =
  (* Register GET /api/sessions *)
  Http_server.Server.add_route server ~path:"/api/sessions" (Api.sessions_get config);

  (* Register GET /api/knowledge *)
  Http_server.Server.add_route server ~path:"/api/knowledge" (Api.knowledge_get config);

  (* Register GET /api/health *)
  Http_server.Server.add_route server ~path:"/api/health" (Api.health_get config);

  (* Register root endpoint *)
  Http_server.Server.add_route server ~path:"/" (fun _req ->
    Http_server.Response.string {|
OClaw Server API

Endpoints:
  POST /api/chat        - Send a message
  GET  /api/sessions    - List all sessions
  DEL  /api/sessions/:id- Delete a session
  GET  /api/knowledge   - Get knowledge info
  POST /api/knowledge/refresh - Refresh knowledge
  GET  /api/health      - Health check
|}
  );

  (* Register POST /api/chat - use a wrapper that reads body from request *)
  Http_server.Server.add_route server ~path:"POST /api/chat" (fun req ->
    Api.chat_post config req
  );

  (* Register DELETE /api/sessions/:id *)
  Http_server.Server.add_route server ~path:"/api/sessions/" (fun req ->
    Api.session_delete config req
  );

  (* Register POST /api/knowledge/refresh *)
  Http_server.Server.add_route server ~path:"POST /api/knowledge/refresh" (fun req ->
    Api.knowledge_refresh_post config req
  );

  Log.info (fun m -> m "API routes registered")
