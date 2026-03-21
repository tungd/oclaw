open Mosaic

let input_id = "chat-input"
let orange = Ansi.Color.of_rgb 255 149 0

type pending_approval = {
  tool_call : Acp.Message.tool_call;
  options : Acp.Message.permission_option list;
  selected_index : int;
}

type runtime_command =
  | Prompt of string
  | Permission of Acp.Message.permission_outcome

type model = {
  messages : (string * string) list;
  input_text : string;
  status_text : string;
  streaming_text : string;
  to_agent_chan : runtime_command Domainslib.Chan.t;
  spinner_index : int;
  is_thinking : bool;
  pending_approval : pending_approval option;
}

type msg =
  | Input_changed of string
  | Submit
  | Runtime_event of Acp.Message.t
  | Runtime_error of string
  | Runtime_done
  | Quit
  | Tick
  | Approval_next
  | Approval_prev
  | Approval_submit

let braille_spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠇" |]

let init state chat_id persistent () =
  let to_agent_chan = Domainslib.Chan.make_unbounded () in
  let model =
    {
      messages = [];
      input_text = "";
      status_text = "Ready";
      streaming_text = "";
      to_agent_chan;
      spinner_index = 0;
      is_thinking = false;
      pending_approval = None;
    }
  in
  let agent_cmd =
    Cmd.perform (fun dispatch ->
        let rec loop () =
          match Domainslib.Chan.recv to_agent_chan with
          | Prompt prompt ->
              let emit runtime_message = dispatch (Runtime_event runtime_message) in
              begin
                match Agent_runtime.Session.process ~emit state ~chat_id ~persistent prompt with
                | Ok () ->
                    dispatch Runtime_done;
                    loop ()
                | Error err ->
                    dispatch (Runtime_error err);
                    loop ()
              end
          | Permission outcome ->
              begin
                match Agent_runtime.Session.resolve_permission state ~chat_id outcome with
                | Ok () ->
                    dispatch Runtime_done;
                    loop ()
                | Error err ->
                    dispatch (Runtime_error err);
                    loop ()
              end
        in
        loop ())
  in
  (model, agent_cmd)

let approval_option_label option =
  match option.Acp.Message.kind with
  | Acp.Message.Allow_once -> "Allow once"
  | Acp.Message.Allow_always -> "Allow always"
  | Acp.Message.Reject_once -> "Reject"
  | Acp.Message.Reject_always -> "Reject always"

let rec update msg model =
  match msg with
  | Input_changed text -> ({ model with input_text = text }, Cmd.None)
  | Submit ->
      begin
        match model.pending_approval with
        | Some _ -> update Approval_submit model
        | None ->
            if String.trim model.input_text = "" then
              (model, Cmd.None)
            else
              let text = model.input_text in
              Domainslib.Chan.send model.to_agent_chan (Prompt text);
              ( {
                  model with
                  messages = model.messages @ [ ("User", text) ];
                  input_text = "";
                  is_thinking = true;
                  status_text = "Thinking...";
                },
                Cmd.focus input_id )
      end
  | Runtime_event runtime_message ->
      begin
        match runtime_message with
        | Acp.Message.Status { status; message } ->
            let status_text = match message with Some m -> status ^ ": " ^ m | None -> status in
            ({ model with status_text; is_thinking = status <> "cancelled" && model.pending_approval = None }, Cmd.None)
        | Acp.Message.Agent_delta { content } ->
            ({ model with streaming_text = model.streaming_text ^ content; is_thinking = true }, Cmd.None)
        | Acp.Message.Agent_message { content; _ } ->
            ( {
                model with
                messages = model.messages @ [ ("Assistant", content) ];
                streaming_text = "";
                status_text = "Ready";
                is_thinking = false;
              },
              Cmd.None )
        | Acp.Message.Session_update { update = Acp.Message.Tool_call tool_call; _ } ->
            let label = Option.value tool_call.Acp.Message.title ~default:tool_call.tool_call_id in
            ({ model with status_text = label; is_thinking = true }, Cmd.None)
        | Acp.Message.Session_update { update = Acp.Message.Tool_call_update tool_call; _ } ->
            let status =
              match tool_call.Acp.Message.status with
              | Some status -> Acp.Message.tool_status_to_string status
              | None -> "tool"
            in
            ({ model with status_text = status; is_thinking = status = "in_progress" }, Cmd.None)
        | Acp.Message.Request_permission { tool_call; options; _ } ->
            ( {
                model with
                pending_approval = Some { tool_call; options; selected_index = 0 };
                status_text = "waiting_for_approval";
                is_thinking = false;
              },
              Cmd.None )
        | Acp.Message.Error { message; _ } ->
            ({ model with messages = model.messages @ [ ("Error", message) ]; status_text = "Error"; is_thinking = false }, Cmd.None)
        | Acp.Message.Done ->
            let model =
              if model.streaming_text <> "" then
                { model with messages = model.messages @ [ ("Assistant", model.streaming_text) ]; streaming_text = "" }
              else
                model
            in
            let status_text = if model.pending_approval <> None then "waiting_for_approval" else "Ready" in
            ({ model with status_text; is_thinking = false }, Cmd.None)
        | Acp.Message.Initialize _
        | Acp.Message.Initialized
        | Acp.Message.Agent_plan _ ->
            (model, Cmd.None)
      end
  | Runtime_error err ->
      ({ model with messages = model.messages @ [ ("Error", err) ]; status_text = "Error"; is_thinking = false; pending_approval = None }, Cmd.None)
  | Runtime_done ->
      (model, Cmd.None)
  | Tick ->
      if model.is_thinking then
        ({ model with spinner_index = (model.spinner_index + 1) mod 8 }, Cmd.None)
      else
        (model, Cmd.None)
  | Approval_next ->
      begin
        match model.pending_approval with
        | None -> (model, Cmd.None)
        | Some pending ->
            let len = List.length pending.options in
            if len = 0 then
              (model, Cmd.None)
            else
              ({
                 model with
                 pending_approval =
                   Some { pending with selected_index = (pending.selected_index + 1) mod len };
               }, Cmd.None)
      end
  | Approval_prev ->
      begin
        match model.pending_approval with
        | None -> (model, Cmd.None)
        | Some pending ->
            let len = List.length pending.options in
            if len = 0 then
              (model, Cmd.None)
            else
              ({
                 model with
                 pending_approval =
                   Some { pending with selected_index = (pending.selected_index + len - 1) mod len };
               }, Cmd.None)
      end
  | Approval_submit ->
      begin
        match model.pending_approval with
        | None -> (model, Cmd.None)
        | Some pending ->
            begin
              match List.nth_opt pending.options pending.selected_index with
              | None -> (model, Cmd.None)
              | Some option ->
                  let outcome =
                    Acp.Message.Selected option.Acp.Message.option_id
                  in
                  Domainslib.Chan.send model.to_agent_chan (Permission outcome);
                  ({
                     model with
                     pending_approval = None;
                     status_text = "resuming_after_approval";
                     is_thinking = true;
                   }, Cmd.None)
            end
      end
  | Quit -> (model, Cmd.Quit)

let render_approval_panel pending =
  let target =
    Option.value pending.tool_call.Acp.Message.title ~default:pending.tool_call.tool_call_id
  in
  let target_line =
    match pending.tool_call.Acp.Message.content with
    | Some [ Acp.Message.Content (Acp.Message.Text text) ] -> text
    | _ ->
        match pending.tool_call.Acp.Message.raw_input with
        | Some (`Assoc fields) ->
            begin
              match List.assoc_opt "path" fields, List.assoc_opt "command" fields, List.assoc_opt "name" fields with
              | Some (`String value), _, _ -> value
              | _, Some (`String value), _ -> value
              | _, _, Some (`String value) -> value
              | _ -> ""
            end
        | _ -> ""
  in
  let options =
    pending.options
    |> List.mapi (fun idx option ->
           let selected = idx = pending.selected_index in
           let prefix = if selected then "> " else "  " in
           let style =
             if selected then
               Ansi.Style.(make ~fg:Ansi.Color.black ~bg:Ansi.Color.yellow ~bold:true ())
             else
               Ansi.Style.(make ~fg:Ansi.Color.white ())
           in
           text ~text_style:style (prefix ^ approval_option_label option))
  in
  box
    ~border:true
    ~border_color:Ansi.Color.yellow
    ~padding:(padding 1)
    ~flex_direction:Column
    [
      text ~text_style:Ansi.Style.(make ~fg:Ansi.Color.yellow ~bold:true ()) "Approval required";
      text ~wrap:`Word ("Tool: " ^ target);
      text ~wrap:`Word ("Target: " ^ target_line);
    ]
  :: options

let view model =
  let render_transcript_line ?(prefix = "") ~style content =
    text ~wrap:`Word ~text_style:style (prefix ^ content)
  in
  let render_user_msg content =
    box
      ~flex_direction:Row
      ~align_items:Align.Start
      ~gap:(gap 1)
      [
        text ~text_style:Ansi.Style.(make ~fg:orange ~bold:true ()) "│";
        box
          ~flex_grow:1.0
          [ render_transcript_line ~style:Ansi.Style.(make ~fg:Ansi.Color.white ()) content ];
      ]
  in
  let render_msg (role, content) =
    match role with
    | "User" -> render_user_msg content
    | "Assistant" ->
        render_transcript_line ~prefix:"Assistant: " ~style:Ansi.Style.(make ~fg:Ansi.Color.green ()) content
    | "Error" ->
        render_transcript_line ~prefix:"Error: " ~style:Ansi.Style.(make ~fg:Ansi.Color.red ()) content
    | _ ->
        render_transcript_line ~prefix:(role ^ ": ") ~style:Ansi.Style.(make ~fg:Ansi.Color.yellow ()) content
  in
  let msgs = List.map render_msg model.messages in
  let msgs =
    if model.streaming_text <> "" then
      msgs
      @ [
          render_transcript_line
            ~prefix:"Assistant: "
            ~style:Ansi.Style.(make ~fg:Ansi.Color.green ())
            model.streaming_text;
        ]
    else
      msgs
  in
  let msgs =
    match model.pending_approval with
    | None -> msgs
    | Some pending -> msgs @ render_approval_panel pending
  in
  let status_icon =
    if model.is_thinking then
      let icon = braille_spinner.(model.spinner_index) ^ " " in
      text ~text_style:Ansi.Style.(make ~fg:Ansi.Color.yellow ~bold:true ()) icon
    else
      text ""
  in
  box
    ~flex_direction:Column
    ~flex_grow:1.0
    [
      scroll_box ~flex_grow:1.0 ~sticky_scroll:true ~sticky_start:`Bottom msgs;
      box ~flex_direction:Row ~padding:(padding_xy 0 1) [ status_icon; text ("Status: " ^ model.status_text) ];
      box
        ~border:true
        ~border_color:orange
        ~padding:(padding 1)
        ~margin:(margin_xy 0 1)
        ~flex_direction:Column
        ~gap:(gap 1)
        [
          text ~text_style:Ansi.Style.(make ~fg:orange ~bold:true ()) "Prompt";
          input
            ~id:input_id
            ~autofocus:true
            ~value:model.input_text
            ~placeholder:"Type a message and press Enter. Press Escape to quit."
            ~text_color:Ansi.Color.white
            ~focused_text_color:Ansi.Color.white
            ~cursor_blinking:false
            ~on_change:(fun s -> Some (Input_changed s))
            ~on_submit:(fun _ -> Some Submit)
            ();
        ];
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.every 0.15 (fun () -> Tick);
      Sub.on_key_all (fun key ->
          let data = Event.Key.data key in
          match data.key with
          | Matrix.Input.Key.Escape -> Some Quit
          | Matrix.Input.Key.Tab when model.pending_approval <> None -> Some Approval_next
          | Matrix.Input.Key.Left when model.pending_approval <> None -> Some Approval_prev
          | Matrix.Input.Key.Right when model.pending_approval <> None -> Some Approval_next
          | Matrix.Input.Key.Enter when model.pending_approval <> None -> Some Approval_submit
          | _ -> None);
    ]

let run ~state ~chat_id ~persistent =
  let app = { init = init state chat_id persistent; update; view; subscriptions } in
  Mosaic.run app
