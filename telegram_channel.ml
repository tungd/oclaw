(** Telegram channel for OClaw server *)

open Yojson.Basic.Util

module Log = (val Logs.src_log (Logs.Src.create "telegram_channel") : Logs.LOG)

(* Telegram configuration *)
type config = {
  enabled : bool;
  token : string;
  allow_from : int64 list;
}

(* Telegram channel *)
module Channel = struct
  type t = {
    bot : Telegram_api.Bot.t;
    config : config;
    session_manager : Session.Manager.t;
    knowledge : Knowledge.Knowledge.t;
    llm_config : Llm_provider.provider_config;
    mutable running : bool;
    mutex : Mutex.t;
    mutable domain : unit Domain.t option;
  }

  let create bot config session_manager knowledge llm_config =
    {
      bot;
      config;
      session_manager;
      knowledge;
      llm_config;
      running = false;
      mutex = Mutex.create ();
      domain = None;
    }

  let is_allowed channel user_id =
    channel.config.allow_from = [] ||
    List.mem user_id channel.config.allow_from

  let build_messages channel session_id user_content =
    let session = match Session.Manager.get_session channel.session_manager ~id:session_id with
    | Some s -> s
    | None -> Session.Manager.create_session channel.session_manager ~id:session_id ~directory:"."
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
    let system_prompt = Knowledge.Knowledge.get_system_prompt channel.knowledge in
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

  let process_message channel msg =
    (* Get chat ID for session ID *)
    let chat_id = msg.Telegram_api.Message.chat.Telegram_api.Chat.id in
    let session_id = Printf.sprintf "telegram:%Ld" chat_id in

    (* Check if user is allowed *)
    (match msg.Telegram_api.Message.from with
    | Some user ->
        if not (is_allowed channel user.Telegram_api.User.id) then begin
          Log.warn (fun m -> m "User %Ld not allowed" user.Telegram_api.User.id);
          ignore (Telegram_api.Bot.send_message channel.bot
            ~chat_id
            ~text:"Sorry, you are not authorized to use this bot.");
          exit 0
        end
    | None ->
        Log.warn (fun m -> m "Message without user info");
        exit 0
    );

    (* Get text content *)
    let content = match msg.Telegram_api.Message.text with
    | Some t -> t
    | None ->
        Log.debug (fun m -> m "Ignoring non-text message");
        exit 0
    in

    (* Handle special commands *)
    if String.starts_with ~prefix:"/" content then begin
      match content with
      | "/start" ->
          ignore (Telegram_api.Bot.send_message channel.bot
            ~chat_id
            ~text:"Welcome to OClaw! Send me a message and I'll respond using AI.");
      | "/help" ->
          ignore (Telegram_api.Bot.send_message channel.bot
            ~chat_id
            ~text:"Commands:\n/start - Start the bot\n/help - Show this help\n\nJust send me any message to chat!");
      | "/clear" ->
          (* Remove and recreate session *)
          Session.Manager.remove_session channel.session_manager ~id:session_id;
          ignore (Session.Manager.create_session channel.session_manager ~id:session_id ~directory:".");
          ignore (Telegram_api.Bot.send_message channel.bot
            ~chat_id
            ~text:"Conversation history cleared.");
      | _ ->
          ignore (Telegram_api.Bot.send_message channel.bot
            ~chat_id
            ~text:(Printf.sprintf "Unknown command: %s" content));
      exit 0
    end;

    (* Build messages *)
    let messages = build_messages channel session_id content in

    (* Call LLM *)
    let result = Llm_provider.call_llm channel.llm_config messages () in

    (* Get session *)
    let session = match Session.Manager.get_session channel.session_manager ~id:session_id with
    | Some s -> s
    | None -> Session.Manager.create_session channel.session_manager ~id:session_id ~directory:"."
    in

    (* Add user message to session *)
    Session.Session.add_message session (Session.Message.create ~role:"user" ~content:content);

    (* Handle response *)
    (match result with
    | Llm_provider.Error err ->
        Log.warn (fun m -> m "LLM error: %s" err);
        ignore (Telegram_api.Bot.send_message channel.bot
          ~chat_id
          ~text:(Printf.sprintf "Error: %s" err));
    | Llm_provider.Success response ->
        (match response.Llm_provider.choices with
        | [] ->
            Log.warn (fun m -> m "No choices in LLM response");
        | choice :: _ ->
            let response_text = choice.Llm_provider.message.Llm_provider.content in

            (* Add assistant response to session *)
            Session.Session.add_message session (Session.Message.create ~role:"assistant" ~content:response_text);

            Session.Session.touch session;

            (* Send response to user *)
            (* Telegram has a 4096 character limit for messages *)
            let max_length = 4000 in
            if String.length response_text > max_length then begin
              (* Split message *)
              let parts = ref [] in
              let pos = ref 0 in
              while !pos < String.length response_text do
                let len = min max_length (String.length response_text - !pos) in
                parts := String.sub response_text !pos len :: !parts;
                pos := !pos + len;
              done;
              List.iter (fun part ->
                ignore (Telegram_api.Bot.send_message channel.bot ~chat_id ~text:part);
                Unix.sleepf 0.5
              ) (List.rev !parts)
            end else begin
              ignore (Telegram_api.Bot.send_message channel.bot ~chat_id ~text:response_text);
            end
        ));

    exit 0

  let poll_loop channel =
    let offset = ref 0 in
    let timeout = 30 in

    Log.info (fun m -> m "Telegram poll loop started");

    while channel.running do
      match Telegram_api.Bot.get_updates channel.bot ~offset:!offset ~timeout with
      | Ok updates ->
          List.iter (fun update ->
            offset := update.Telegram_api.Update.update_id + 1;

            (* Handle message *)
            (match update.Telegram_api.Update.message with
            | Some msg ->
                (* Process in a domain *)
                let _ = Domain.spawn (fun () ->
                  try process_message channel msg
                  with exn ->
                    Log.warn (fun m -> m "Message processing error: %s" (Printexc.to_string exn))
                ) in ()
            | None -> ())
          ) updates
      | Error err ->
          Log.warn (fun m -> m "Telegram poll error: %s" err);
          Unix.sleepf 5.0
    done;

    Log.info (fun m -> m "Telegram poll loop stopped")

  let start channel =
    Mutex.lock channel.mutex;
    if channel.running then begin
      Log.warn (fun m -> m "Telegram channel already running");
      Mutex.unlock channel.mutex;
    end else begin
      channel.running <- true;

      (* Verify bot token by getting bot info *)
      (match Telegram_api.Bot.get_me channel.bot with
      | Ok bot ->
          Log.info (fun m -> m "Connected to Telegram bot: @%s" (bot.Telegram_api.User.username |> Option.value ~default:"unknown"))
      | Error err ->
          Log.warn (fun m -> m "Failed to connect to Telegram: %s" err);
          channel.running <- false;
          Mutex.unlock channel.mutex;
          failwith "Telegram connection failed"
      );

      (* Start poll loop in a domain *)
      let domain = Domain.spawn (fun () ->
        try poll_loop channel
        with exn ->
          Log.warn (fun m -> m "Telegram poll loop error: %s" (Printexc.to_string exn))
      ) in

      channel.domain <- Some domain;
      Mutex.unlock channel.mutex;
      Log.info (fun m -> m "Telegram channel started")
    end

  let stop channel =
    Mutex.lock channel.mutex;
    if not channel.running then begin
      Log.warn (fun m -> m "Telegram channel not running");
      Mutex.unlock channel.mutex;
    end else begin
      Log.info (fun m -> m "Stopping Telegram channel");
      channel.running <- false;
      Mutex.unlock channel.mutex;

      (* Wait for domain to finish *)
      (match channel.domain with
      | Some domain -> Domain.join domain
      | None -> ());

      channel.domain <- None;
      Log.info (fun m -> m "Telegram channel stopped")
    end

  let is_running channel =
    Mutex.lock channel.mutex;
    let result = channel.running in
    Mutex.unlock channel.mutex;
    result
end
