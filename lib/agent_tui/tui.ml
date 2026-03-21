open Mosaic

type model = {
  messages : (string * string) list;
  input_text : string;
  status_text : string;
  streaming_text : string;
  to_agent_chan : string Domainslib.Chan.t;
  spinner_index : int;
  is_thinking : bool;
}

type msg =
  | Input_changed of string
  | Submit
  | Runtime_event of Acp.Message.t
  | Runtime_error of string
  | Runtime_done
  | Quit
  | Tick

let braille_spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠇" |]

let init state chat_id persistent () =
  let to_agent_chan = Domainslib.Chan.make_unbounded () in

  let model = {
    messages = [];
    input_text = "";
    status_text = "Ready";
    streaming_text = "";
    to_agent_chan;
    spinner_index = 0;
    is_thinking = false;
  } in

  (* Single Cmd.perform spawns one thread for the agent processor.
     No manual domain management, no intermediate channel. *)
  let agent_cmd = Cmd.perform (fun dispatch ->
    let rec loop () =
      let prompt = Domainslib.Chan.recv to_agent_chan in
      let emit runtime_message =
        dispatch (Runtime_event runtime_message)
      in
      match Agent_runtime.Session.process ~emit state ~chat_id ~persistent prompt with
      | Ok () ->
          dispatch Runtime_done;
          loop ()
      | Error err ->
          dispatch (Runtime_error err);
          loop ()
    in
    loop ()
  ) in

  (model, agent_cmd)

let update msg model =
  match msg with
  | Input_changed text -> ({ model with input_text = text }, Cmd.None)
  | Submit ->
      if String.trim model.input_text = "" then (model, Cmd.None)
      else
        let text = model.input_text in
        Domainslib.Chan.send model.to_agent_chan text;
        ({ model with
           messages = model.messages @ [("User", text)];
           input_text = "";
           is_thinking = true;
           status_text = "Thinking..."
         }, Cmd.None)
  | Runtime_event runtime_message ->
      (match runtime_message with
       | Acp.Message.Status { status; message } ->
           let status_text = match message with Some m -> status ^ ": " ^ m | None -> status in
           ({ model with status_text; is_thinking = true }, Cmd.None)
       | Acp.Message.Agent_delta { content } ->
           ({ model with streaming_text = model.streaming_text ^ content; is_thinking = true }, Cmd.None)
       | Acp.Message.Agent_message { content; _ } ->
           ({ model with
              messages = model.messages @ [("Assistant", content)];
              streaming_text = "";
              status_text = "Ready";
              is_thinking = false;
            }, Cmd.None)
       | Acp.Message.Tool_call { name; _ } ->
           ({ model with status_text = "tool: " ^ name; is_thinking = true }, Cmd.None)
       | Acp.Message.Tool_result { name; is_error; _ } ->
           let status_text =
             if is_error then "tool error: " ^ name else "tool finished: " ^ name
           in
           ({ model with status_text; is_thinking = true }, Cmd.None)
       | Acp.Message.Error { message; _ } ->
           ({ model with messages = model.messages @ [("Error", message)]; status_text = "Error"; is_thinking = false }, Cmd.None)
       | Acp.Message.Done ->
           let model =
             if model.streaming_text <> "" then
               { model with
                 messages = model.messages @ [("Assistant", model.streaming_text)];
                 streaming_text = "" }
             else model
           in
           ({ model with status_text = "Ready"; is_thinking = false }, Cmd.None)
       | Acp.Message.Initialize _
       | Acp.Message.Initialized
       | Acp.Message.Agent_plan _ ->
           (model, Cmd.None))
  | Runtime_error err ->
      ({ model with messages = model.messages @ [("Error", err)]; status_text = "Error"; is_thinking = false }, Cmd.None)
  | Runtime_done ->
      (model, Cmd.None)
  | Tick ->
      if model.is_thinking then
        ({ model with spinner_index = (model.spinner_index + 1) mod 8 }, Cmd.None)
      else
        (model, Cmd.None)
  | Quit -> (model, Cmd.Quit)

let view model =
  let render_msg (role, content) =
    let style =
      match role with
      | "User" -> Ansi.Style.(make ~fg:Ansi.Color.blue ())
      | "Assistant" -> Ansi.Style.(make ~fg:Ansi.Color.green ())
      | "Error" -> Ansi.Style.(make ~fg:Ansi.Color.red ())
      | _ -> Ansi.Style.(make ~fg:Ansi.Color.yellow ())
    in
    text ~text_style:style (role ^ ": " ^ content)
  in
  let msgs = List.map render_msg model.messages in
  let msgs =
    if model.streaming_text <> "" then
      msgs @ [text ~text_style:Ansi.Style.(make ~fg:Ansi.Color.green ()) ("Assistant: " ^ model.streaming_text)]
    else msgs
  in

  let status_icon =
    if model.is_thinking then
      let icon = braille_spinner.(model.spinner_index) ^ " " in
      text ~text_style:Ansi.Style.(make ~fg:Ansi.Color.yellow ~bold:true ()) icon
    else text ""
  in

  box
    ~flex_direction:Column
    ~flex_grow:1.0
    [
      scroll_box ~flex_grow:1.0 msgs;
      box ~flex_direction:Row [
        status_icon;
        text ("Status: " ^ model.status_text);
      ];
      input
        ~value:model.input_text
        ~placeholder:"Type a message... (Press Escape to quit)"
        ~on_change:(fun s -> Some (Input_changed s))
        ~on_submit:(fun _ -> Some Submit)
        ();
    ]

let subscriptions _model =
    Sub.batch [
    (* 150ms spinner tick - handled by Mosaic's event loop, not an OS thread *)
    Sub.every 0.15 (fun () -> Tick);
    Sub.on_key_all (fun key ->
      let data = Event.Key.data key in
      match data.key with
      | Matrix.Input.Key.Escape -> Some Quit
      | _ -> None
    )
  ]

let run ~state ~chat_id ~persistent =
  let app = {
    init = init state chat_id persistent;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app
