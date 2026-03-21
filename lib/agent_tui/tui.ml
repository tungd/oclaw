open Mosaic

let input_id = "chat-input"
let orange = Ansi.Color.of_rgb 255 149 0
let transcript_fg = Ansi.Color.white
let transcript_muted = Ansi.Color.of_rgb 165 156 132
let transcript_code = Ansi.Color.of_rgb 255 199 87
let transcript_heading = Ansi.Color.of_rgb 240 230 196
let transcript_link = Ansi.Color.of_rgb 159 193 255
let transcript_selection_bg = Ansi.Color.of_rgb 78 78 78

type pending_approval = {
  tool_call : Acp.Message.tool_call;
  options : Acp.Message.permission_option list;
  selected_index : int;
}

type runtime_command =
  | Prompt of string
  | Permission of Acp.Message.permission_outcome

type transcript_tool = {
  tool_call_id : string;
  kind_label : string;
  target : string;
  status : Acp.Message.tool_status option;
}

type transcript_item =
  | User_message of string
  | Assistant_message of string
  | Error_message of string
  | Tool_message of transcript_tool

type model = {
  messages : transcript_item list;
  input_text : string;
  status_text : string;
  streaming_text : string;
  to_agent_chan : runtime_command Domainslib.Chan.t;
  spinner_index : int;
  is_thinking : bool;
  pending_approval : pending_approval option;
  project_root : string;
  model_name : string;
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

let prompt_footer_path path =
  let home =
    try Sys.getenv "HOME" with Not_found -> ""
  in
  if home <> "" && String.starts_with ~prefix:home path then
    "~" ^ String.sub path (String.length home) (String.length path - String.length home)
  else
    path

let markdown_style = function
  | Markdown.Default -> Ansi.Style.make ~fg:transcript_fg ()
  | Heading _ -> Ansi.Style.make ~fg:transcript_heading ~bold:true ()
  | Emphasis -> Ansi.Style.make ~fg:transcript_fg ~italic:true ()
  | Strong -> Ansi.Style.make ~fg:transcript_fg ~bold:true ()
  | Code_span -> Ansi.Style.make ~fg:transcript_code ()
  | Code_block -> Ansi.Style.make ~fg:transcript_code ()
  | Link | Image ->
      Ansi.Style.make ~fg:transcript_link ~underline:true ()
  | Blockquote ->
      Ansi.Style.make ~fg:transcript_muted ~italic:true ()
  | Thematic_break | Table_border | Conceal_punctuation ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 88 88 88) ()
  | List_marker | Task_marker ->
      Ansi.Style.make ~fg:transcript_heading ~bold:true ()
  | Strikethrough ->
      Ansi.Style.make ~fg:transcript_muted ~strikethrough:true ()

let style_merge base overlay =
  Ansi.Style.merge ~base ~overlay

let make_span ?(style = Ansi.Style.default) text =
  { text; style }

let starts_with_at s i prefix =
  let len = String.length prefix in
  i + len <= String.length s && String.sub s i len = prefix

let parse_inline_spans ~base line =
  let len = String.length line in
  let flush_plain acc buf =
    if Buffer.length buf = 0 then acc
    else
      let text = Buffer.contents buf in
      Buffer.clear buf;
      acc @ [ make_span ~style:base text ]
  in
  let rec loop i acc buf =
    if i >= len then
      flush_plain acc buf
    else if starts_with_at line i "**" then
      match String.index_from_opt line (i + 2) '*' with
      | Some j when j + 1 < len && line.[j + 1] = '*' ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 2) (j - i - 2) in
          let style = style_merge base (Ansi.Style.make ~bold:true ()) in
          loop (j + 2) (acc @ [ make_span ~style inner ]) buf
      | _ ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else if line.[i] = '*' then
      match String.index_from_opt line (i + 1) '*' with
      | Some j ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 1) (j - i - 1) in
          let style = style_merge base (Ansi.Style.make ~italic:true ()) in
          loop (j + 1) (acc @ [ make_span ~style inner ]) buf
      | None ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else if line.[i] = '`' then
      match String.index_from_opt line (i + 1) '`' with
      | Some j ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 1) (j - i - 1) in
          let style = style_merge base (Ansi.Style.make ~fg:transcript_code ()) in
          loop (j + 1) (acc @ [ make_span ~style inner ]) buf
      | None ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else begin
      Buffer.add_char buf line.[i];
      loop (i + 1) acc buf
    end
  in
  loop 0 [] (Buffer.create len)

let line_prefix line =
  let rec count i =
    if i < String.length line && line.[i] = '#' then count (i + 1) else i
  in
  let hashes = count 0 in
  if hashes > 0 && hashes < String.length line && line.[hashes] = ' ' then
    Some hashes
  else
    None

let markdown_spans content =
  let lines = String.split_on_char '\n' content in
  let rec loop in_code_block acc = function
    | [] -> acc
    | line :: rest ->
        if starts_with_at line 0 "```" then
          let marker_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 88 88 88) () in
          let acc = acc @ [ make_span ~style:marker_style line; make_span "\n" ] in
          loop (not in_code_block) acc rest
        else
          let line_spans =
            if in_code_block then
              [ make_span ~style:(Ansi.Style.make ~fg:transcript_code ()) line ]
            else
              match line_prefix line with
              | Some level ->
                  let prefix = String.make level '#' ^ " " in
                  let text =
                    String.sub line (String.length prefix) (String.length line - String.length prefix)
                  in
                  let marker_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 88 88 88) () in
                  let heading_style = markdown_style (Heading level) in
                  [ make_span ~style:marker_style prefix ] @ parse_inline_spans ~base:heading_style text
              | None ->
                  parse_inline_spans ~base:(markdown_style Default) line
          in
          let acc = acc @ line_spans @ [ make_span "\n" ] in
          loop in_code_block acc rest
  in
  match loop false [] lines with
  | [] -> [ make_span "" ]
  | spans -> spans

let tool_kind_label = function
  | Some Acp.Message.Read -> "Read"
  | Some Acp.Message.Edit -> "Edit"
  | Some Acp.Message.Delete -> "Delete"
  | Some Acp.Message.Move -> "Move"
  | Some Acp.Message.Search -> "Search"
  | Some Acp.Message.Execute -> "Exec"
  | Some Acp.Message.Think -> "Think"
  | Some Acp.Message.Fetch -> "Fetch"
  | Some Acp.Message.Other | None -> "Tool"

let json_string_member name = function
  | `Assoc fields ->
      begin
        match List.assoc_opt name fields with
        | Some (`String value) when String.trim value <> "" -> Some value
        | _ -> None
      end
  | _ -> None

let tool_target tool_call =
  let from_content =
    match tool_call.Acp.Message.content with
    | Some [ Acp.Message.Content (Acp.Message.Text text) ] when String.trim text <> "" -> Some text
    | _ -> None
  in
  let from_input =
    match tool_call.Acp.Message.raw_input with
    | Some json ->
        let candidates =
          [
            json_string_member "path" json;
            json_string_member "command" json;
            json_string_member "query" json;
            json_string_member "name" json;
          ]
        in
        List.find_map Fun.id candidates
    | None -> None
  in
  Option.value
    ~default:
      (Option.value ~default:tool_call.tool_call_id tool_call.title)
    (match from_input with Some _ as value -> value | None -> from_content)

let transcript_tool_of_call tool_call =
  {
    tool_call_id = tool_call.Acp.Message.tool_call_id;
    kind_label = tool_kind_label tool_call.kind;
    target = tool_target tool_call;
    status = tool_call.status;
  }

let rec update_tool_message messages tool =
  match messages with
  | [] -> [ Tool_message tool ]
  | Tool_message existing :: rest when String.equal existing.tool_call_id tool.tool_call_id ->
      Tool_message tool :: rest
  | message :: rest -> message :: update_tool_message rest tool

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
      project_root = Agent_runtime.App.project_root state;
      model_name = Agent_runtime.App.model_name state;
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
                  messages = model.messages @ [ User_message text ];
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
                messages = model.messages @ [ Assistant_message content ];
                streaming_text = "";
                status_text = "Ready";
                is_thinking = false;
              },
              Cmd.None )
        | Acp.Message.Session_update { update = Acp.Message.Tool_call tool_call; _ } ->
            let label = Option.value tool_call.Acp.Message.title ~default:tool_call.tool_call_id in
            ( {
                model with
                messages = update_tool_message model.messages (transcript_tool_of_call tool_call);
                status_text = label;
                is_thinking = true;
              },
              Cmd.None )
        | Acp.Message.Session_update { update = Acp.Message.Tool_call_update tool_call; _ } ->
            let status =
              match tool_call.Acp.Message.status with
              | Some status -> Acp.Message.tool_status_to_string status
              | None -> "tool"
            in
            ( {
                model with
                messages = update_tool_message model.messages (transcript_tool_of_call tool_call);
                status_text = status;
                is_thinking = status = "in_progress";
              },
              Cmd.None )
        | Acp.Message.Request_permission { tool_call; options; _ } ->
            ( {
                model with
                messages = update_tool_message model.messages (transcript_tool_of_call tool_call);
                pending_approval = Some { tool_call; options; selected_index = 0 };
                status_text = "waiting_for_approval";
                is_thinking = false;
              },
              Cmd.None )
        | Acp.Message.Error { message; _ } ->
            ({ model with messages = model.messages @ [ Error_message message ]; status_text = "Error"; is_thinking = false }, Cmd.None)
        | Acp.Message.Done ->
            let model =
              if model.streaming_text <> "" then
                { model with messages = model.messages @ [ Assistant_message model.streaming_text ]; streaming_text = "" }
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
      ({ model with messages = model.messages @ [ Error_message err ]; status_text = "Error"; is_thinking = false; pending_approval = None }, Cmd.None)
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
  let render_rich_text ?(prefix = "") content =
    code
      ~spans:(markdown_spans (prefix ^ content))
      ~wrap:`Word
      ~selectable:true
      ~selection_bg:transcript_selection_bg
      ~selection_fg:transcript_fg
      ""
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
          [ render_rich_text content ];
      ]
  in
  let render_tool_msg tool =
    let status_glyph, status_style =
      match tool.status with
      | Some Acp.Message.Completed ->
          ("✓", Ansi.Style.(make ~fg:(Ansi.Color.of_rgb 172 170 44) ()))
      | Some Acp.Message.Failed ->
          ("✗", Ansi.Style.(make ~fg:Ansi.Color.red ()))
      | Some Acp.Message.In_progress ->
          ("…", Ansi.Style.(make ~fg:(Ansi.Color.of_rgb 112 170 112) ()))
      | Some Acp.Message.Pending | None ->
          ("…", Ansi.Style.(make ~fg:transcript_muted ()))
    in
    box
      ~flex_direction:Row
      ~align_items:Align.Start
      ~gap:(gap 1)
      [
        text ~text_style:status_style status_glyph;
        text
          ~text_style:Ansi.Style.(make ~fg:transcript_heading ~bold:true ())
          tool.kind_label;
        text
          ~wrap:`Word
          ~text_style:Ansi.Style.(make ~fg:(Ansi.Color.of_rgb 85 118 85) ~underline:true ())
          tool.target;
      ]
  in
  let render_msg = function
    | User_message content -> render_user_msg content
    | Assistant_message content -> render_rich_text content
    | Error_message content -> render_rich_text ~prefix:"Error: " content
    | Tool_message tool -> render_tool_msg tool
  in
  let msgs = List.map render_msg model.messages in
  let msgs =
    if model.streaming_text <> "" then
      msgs
      @ [
          render_rich_text model.streaming_text;
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
  let status_line =
    if model.is_thinking || model.status_text <> "Ready" then
      Some
        (box
           ~flex_direction:Row
           ~padding:(padding_xy 0 1)
           [ status_icon; text ("Status: " ^ model.status_text) ])
    else
      None
  in
  let prompt_footer =
    box
      ~flex_direction:Row
      ~justify_content:Justify.Space_between
      [
        text
          ~text_style:Ansi.Style.(make ~fg:transcript_muted ())
          (prompt_footer_path model.project_root);
        text
          ~text_style:Ansi.Style.(make ~fg:transcript_muted ())
          model.model_name;
      ]
  in
  box
    ~flex_direction:Column
    ~flex_grow:1.0
    ([
       scroll_box
         ~flex_grow:1.0
         ~flex_shrink:1.0
         ~padding:(padding_xy 2 1)
         ~sticky_scroll:true
         ~sticky_start:`Bottom
         msgs;
     ]
     @ (match status_line with Some line -> [ line ] | None -> [])
     @ [
         box
           ~flex_grow:0.0
           ~flex_shrink:0.0
           ~border:true
           ~border_sides:[ `Top; `Bottom ]
           ~border_color:(Ansi.Color.of_rgb 84 84 84)
           ~padding:(padding_xy 0 1)
           ~margin:(margin_xy 0 1)
           ~flex_direction:Column
           ~gap:(gap 1)
         [
          input
            ~id:input_id
            ~autofocus:true
            ~value:model.input_text
            ~placeholder:"Type a message and press Enter. Press Escape to quit."
            ~text_color:transcript_fg
            ~focused_text_color:transcript_fg
            ~placeholder_color:(Ansi.Color.of_rgb 110 110 110)
            ~cursor_style:`Block
            ~cursor_color:Ansi.Color.white
            ~cursor_blinking:false
            ~on_change:(fun s -> Some (Input_changed s))
            ~on_submit:(fun _ -> Some Submit)
            ();
          prompt_footer;
        ];
       ])

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
