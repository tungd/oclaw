open Mosaic

type agent_event =
  | Status of string * string option
  | Delta of string
  | Message of string * int option
  | Tool_call of string
  | Tool_result of string * string * bool
  | Plan of string list
  | Error of string
  | Done
  | Tick

type model = {
  messages : (string * string) list;
  input_text : string;
  status_text : string;
  streaming_text : string;
  to_agent_chan : string Domainslib.Chan.t;
  from_agent_chan : agent_event Domainslib.Chan.t;
  spinner_index : int;
  is_thinking : bool;
}

type msg =
  | Input_changed of string
  | Submit
  | Agent_event of agent_event
  | Quit

let braille_spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠇" |]

let rec poll_agent_events from_agent_chan dispatch =
  let ev = Domainslib.Chan.recv from_agent_chan in
  dispatch (Agent_event ev);
  poll_agent_events from_agent_chan dispatch

let init state chat_id persistent () =
  let to_agent_chan = Domainslib.Chan.make_unbounded () in
  let from_agent_chan = Domainslib.Chan.make_unbounded () in
  
  let _agent_domain = Domain.spawn (fun () ->
    let rec loop () =
      let prompt = Domainslib.Chan.recv to_agent_chan in
      Domainslib.Chan.send from_agent_chan (Status ("thinking", None));
      let on_text_delta delta =
        Domainslib.Chan.send from_agent_chan (Delta delta)
      in
      let on_status status =
        Domainslib.Chan.send from_agent_chan (Status (status, None))
      in
      match Agent_engine.process ~on_text_delta ~on_status state ~chat_id ~persistent prompt with
      | Ok response ->
          Domainslib.Chan.send from_agent_chan (Message (response, Some chat_id));
          Domainslib.Chan.send from_agent_chan Done;
          loop ()
      | Error err ->
          Domainslib.Chan.send from_agent_chan (Error err);
          loop ()
    in
    loop ()
  ) in

  let model = {
    messages = [];
    input_text = "";
    status_text = "Ready";
    streaming_text = "";
    to_agent_chan;
    from_agent_chan;
    spinner_index = 0;
    is_thinking = false;
  } in
  
  let poll_cmd = Cmd.Perform (fun dispatch ->
    let _poll_domain = Domain.spawn (fun () ->
      poll_agent_events from_agent_chan dispatch
    ) in
    let _tick_domain = Domain.spawn (fun () ->
      while true do
        Unix.sleepf 0.15;
        dispatch (Agent_event Tick)
      done
    ) in
    ()
  ) in
  
  (model, poll_cmd)

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
  | Agent_event ev ->
      (match ev with
       | Status (status, msg) ->
           let status_text = match msg with Some m -> status ^ ": " ^ m | None -> status in
           ({ model with status_text; is_thinking = true }, Cmd.None)
       | Delta text ->
           ({ model with streaming_text = model.streaming_text ^ text; is_thinking = false }, Cmd.None)
       | Message (text, _) ->
           ({ model with 
              messages = model.messages @ [("Assistant", text)];
              streaming_text = "";
              status_text = "Ready";
              is_thinking = false
            }, Cmd.None)
       | Tool_call name ->
           ({ model with status_text = "Working: " ^ name; is_thinking = true }, Cmd.None)
       | Tool_result (name, content, _is_error) ->
           ({ model with
              messages = model.messages @ [("Tool " ^ name, content)];
            }, Cmd.None)
       | Plan steps ->
           let content = "Plan:\n" ^ String.concat "\n" (List.map (fun s -> "- " ^ s) steps) in
           ({ model with messages = model.messages @ [("Assistant", content)] }, Cmd.None)
       | Error err ->
           ({ model with messages = model.messages @ [("Error", err)]; status_text = "Error"; is_thinking = false }, Cmd.None)
       | Done ->
           let model = 
             if model.streaming_text <> "" then
               { model with 
                 messages = model.messages @ [("Assistant", model.streaming_text)];
                 streaming_text = "" }
             else model
           in
           ({ model with status_text = "Ready"; is_thinking = false }, Cmd.None)
       | Tick ->
           if model.is_thinking then
             ({ model with spinner_index = (model.spinner_index + 1) mod 8 }, Cmd.None)
           else
             (model, Cmd.None)
      )
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
  Sub.On_key_all (fun key ->
    let data = Event.Key.data key in
    match data.key with
    | Matrix.Input.Key.Escape -> Some Quit
    | _ -> None
  )

let run ~state ~chat_id ~persistent =
  let app = {
    init = init state chat_id persistent;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app
