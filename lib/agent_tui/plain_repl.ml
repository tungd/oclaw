module L = Layoutz

type command =
  [ `Ignore
  | `Exit
  | `Prompt of string
  ]

type deps = {
  process : emit:(Acp.Message.t -> unit) -> string -> (unit, string) result;
  resolve_permission : Acp.Message.permission_outcome -> (unit, string) result;
  history : unit -> Llm_types.message list;
  project_root : string;
  model_name : string;
  chat_label : string;
}

type pending_permission = {
  tool_call : Acp.Message.tool_call;
  options : Acp.Message.permission_option list;
}

type io = {
  input : in_channel;
  output : out_channel;
  input_isatty : bool;
  supports_ansi : bool;
  mutable last_output_ended_with_newline : bool;
}

type runtime_state = {
  io : io;
  mutable pending_permission : pending_permission option;
  mutable saw_streaming_delta : bool;
  mutable saw_visible_message : bool;
  mutable saw_error_event : bool;
  stream_buffer : Buffer.t;
  mutable stream_in_code_block : bool;
}

exception Exit_requested

type color = L.color

type style = {
  fg : color option;
  text_style : L.Style.t;
}

type styled_span = {
  text : string;
  style : style;
}

let default_style =
  {
    fg = None;
    text_style = L.Style.none;
  }

let transcript_fg = L.Color.rgb 255 255 255
let transcript_muted = L.Color.rgb 165 156 132
let transcript_code = L.Color.rgb 255 199 87
let transcript_heading = L.Color.rgb 240 230 196
let transcript_link = L.Color.rgb 159 193 255
let transcript_dim = L.Color.rgb 88 88 88

let make_style ?fg ?(text_style=L.Style.none) () =
  { fg; text_style }

let merge_style base overlay =
  {
    fg = (match overlay.fg with Some _ -> overlay.fg | None -> base.fg);
    text_style = L.Style.compose base.text_style overlay.text_style;
  }

let make_span ?(style=default_style) text =
  { text; style }

let prompt_footer_path path =
  let home =
    try Sys.getenv "HOME" with Not_found -> ""
  in
  if home <> "" && String.starts_with ~prefix:home path then
    "~" ^ String.sub path (String.length home) (String.length path - String.length home)
  else
    path

let write_raw io text =
  if text <> "" then begin
    output_string io.output text;
    flush io.output;
    io.last_output_ended_with_newline <- text.[String.length text - 1] = '\n'
  end

let write_line io text =
  write_raw io text;
  write_raw io "\n"

let ensure_newline io =
  if not io.last_output_ended_with_newline then write_raw io "\n"

let read_prompted_line io prompt =
  write_raw io prompt;
  match input_line io.input with
  | line ->
      if io.input_isatty then
        io.last_output_ended_with_newline <- true
      else begin
        write_raw io line;
        write_raw io "\n"
      end;
      Some line
  | exception End_of_file ->
      None

let classify_input line =
  let trimmed = String.trim line in
  if trimmed = "" then
    `Ignore
  else if trimmed = "/exit" || trimmed = "/quit" then
    `Exit
  else
    `Prompt line

let split_lines text =
  match String.split_on_char '\n' text with
  | [] -> [ "" ]
  | lines -> lines

let style_element ~supports_ansi ?fg ?(text_style=L.Style.none) element =
  if not supports_ansi then
    element
  else
    match fg, text_style with
    | None, style when style = L.Style.none -> element
    | Some fg, style when style = L.Style.none -> L.withStyle ~fg element
    | None, style -> L.withStyle ~style element
    | Some fg, style -> L.withStyle ~fg ~style element

let render_element element =
  L.render element

let render_user_lines ~supports_ansi text =
  let render_line ~prefix content =
    let body =
      style_element ~supports_ansi ~fg:transcript_fg (L.s content)
    in
    if supports_ansi then
      render_element (L.marginColor ~prefix ~color:transcript_dim body)
    else
      render_element (L.margin ~prefix body)
  in
  match split_lines text with
  | [] -> [ "> " ]
  | first :: rest ->
      render_line ~prefix:">" first
      :: List.map (fun line -> render_line ~prefix:" " line) rest

let partition_complete_lines ~final text =
  let len = String.length text in
  let rec loop start idx acc =
    if idx >= len then
      if final then
        let trailing =
          if start < len then [ String.sub text start (len - start) ] else []
        in
        (List.rev_append acc trailing, "")
      else
        (List.rev acc, String.sub text start (len - start))
    else if text.[idx] = '\n' then
      let line = String.sub text start (idx - start) in
      loop (idx + 1) (idx + 1) (line :: acc)
    else
      loop start (idx + 1) acc
  in
  loop 0 0 []

let starts_with_at s i prefix =
  let len = String.length prefix in
  i + len <= String.length s && String.sub s i len = prefix

let line_prefix line =
  let rec count i =
    if i < String.length line && line.[i] = '#' then count (i + 1) else i
  in
  let hashes = count 0 in
  if hashes > 0 && hashes < String.length line && line.[hashes] = ' ' then
    Some hashes
  else
    None

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
    else if line.[i] = '[' then
      match String.index_from_opt line (i + 1) ']' with
      | Some close_label when close_label + 1 < len && line.[close_label + 1] = '(' ->
          begin
            match String.index_from_opt line (close_label + 2) ')' with
            | Some close_url ->
                let acc = flush_plain acc buf in
                let label = String.sub line (i + 1) (close_label - i - 1) in
                let url =
                  String.sub line (close_label + 2) (close_url - close_label - 2)
                in
                let link_style =
                  merge_style
                    base
                    (make_style
                       ~fg:transcript_link
                       ~text_style:L.Style.(compose underline bold)
                       ())
                in
                let url_style =
                  merge_style base (make_style ~fg:transcript_muted ())
                in
                let spans =
                  if String.trim url = "" then
                    [ make_span ~style:link_style label ]
                  else
                    [
                      make_span ~style:link_style label;
                      make_span ~style:base " (";
                      make_span ~style:url_style url;
                      make_span ~style:base ")";
                    ]
                in
                loop (close_url + 1) (acc @ spans) buf
            | None ->
                Buffer.add_char buf line.[i];
                loop (i + 1) acc buf
          end
      | _ ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else if starts_with_at line i "**" then
      match String.index_from_opt line (i + 2) '*' with
      | Some j when j + 1 < len && line.[j + 1] = '*' ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 2) (j - i - 2) in
          let style = merge_style base (make_style ~text_style:L.Style.bold ()) in
          loop (j + 2) (acc @ [ make_span ~style inner ]) buf
      | _ ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else if line.[i] = '*' then
      match String.index_from_opt line (i + 1) '*' with
      | Some j ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 1) (j - i - 1) in
          let style = merge_style base (make_style ~text_style:L.Style.italic ()) in
          loop (j + 1) (acc @ [ make_span ~style inner ]) buf
      | None ->
          Buffer.add_char buf line.[i];
          loop (i + 1) acc buf
    else if line.[i] = '`' then
      match String.index_from_opt line (i + 1) '`' with
      | Some j ->
          let acc = flush_plain acc buf in
          let inner = String.sub line (i + 1) (j - i - 1) in
          let style = merge_style base (make_style ~fg:transcript_code ()) in
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

let render_spans ~supports_ansi spans =
  let elements =
    List.map
      (fun span ->
        style_element
          ~supports_ansi
          ?fg:span.style.fg
          ~text_style:span.style.text_style
          (L.s span.text))
      spans
  in
  render_element (L.tightRow elements)

let render_markdown_line ~supports_ansi ~in_code_block line =
  if starts_with_at line 0 "```" then
    let marker =
      render_element
        (style_element ~supports_ansi ~fg:transcript_dim (L.s line))
    in
    (marker, not in_code_block)
  else if in_code_block then
    let rendered =
      render_element
        (style_element ~supports_ansi ~fg:transcript_code (L.s line))
    in
    (rendered, in_code_block)
  else
    match line_prefix line with
    | Some level ->
        let prefix = String.make level '#' ^ " " in
        let text =
          String.sub line (String.length prefix) (String.length line - String.length prefix)
        in
        let marker =
          render_element
            (style_element ~supports_ansi ~fg:transcript_dim (L.s prefix))
        in
        let heading_style = make_style ~fg:transcript_heading ~text_style:L.Style.bold () in
        let rendered = render_spans ~supports_ansi (parse_inline_spans ~base:heading_style text) in
        (marker ^ rendered, in_code_block)
    | None ->
        let base =
          if starts_with_at line 0 "> " then
            make_style ~fg:transcript_muted ~text_style:L.Style.italic ()
          else
            make_style ~fg:transcript_fg ()
        in
        let rendered = render_spans ~supports_ansi (parse_inline_spans ~base line) in
        (rendered, in_code_block)

let render_markdown_lines_with_state ~supports_ansi ~in_code_block lines =
  let rec loop in_code_block acc = function
    | [] -> (List.rev acc, in_code_block)
    | line :: rest ->
        let rendered, next_in_code_block =
          render_markdown_line ~supports_ansi ~in_code_block line
        in
        loop next_in_code_block (rendered :: acc) rest
  in
  loop in_code_block [] lines

let render_markdown_lines ~supports_ansi text =
  let lines = split_lines text in
  fst (render_markdown_lines_with_state ~supports_ansi ~in_code_block:false lines)

let visible_text_of_content = function
  | Llm_types.Text_content text ->
      if text = "" then None else Some text
  | Llm_types.Blocks blocks ->
      let visible =
        blocks
        |> List.filter_map (function
               | Llm_types.Text { text } -> Some text
               | Llm_types.Image _ -> None
               | Llm_types.Tool_use _ -> None
               | Llm_types.Tool_result _ -> None)
      in
      begin
        match visible with
        | [] -> None
        | texts -> Some (String.concat "\n" texts)
      end

let render_history_message_with_ansi ~supports_ansi (message : Llm_types.message) =
  match visible_text_of_content message.content with
  | None -> []
  | Some text ->
      begin
        match message.role with
        | "user" -> render_user_lines ~supports_ansi text
        | _ -> render_markdown_lines ~supports_ansi text
      end

let render_history_message message =
  render_history_message_with_ansi ~supports_ansi:false message

let tool_kind_label = function
  | Some Acp.Message.Read -> "Read"
  | Some Acp.Message.Edit -> "Edit"
  | Some Acp.Message.Delete -> "Delete"
  | Some Acp.Message.Move -> "Move"
  | Some Acp.Message.Search -> "Search"
  | Some Acp.Message.Execute -> "Exec"
  | Some Acp.Message.Think -> "Think"
  | Some Acp.Message.Fetch -> "Fetch"
  | Some Acp.Message.Other
  | None -> "Tool"

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
    ~default:(Option.value ~default:tool_call.tool_call_id tool_call.title)
    (match from_input with Some _ as value -> value | None -> from_content)

let select_permission_option options line =
  let trimmed = String.trim line in
  match int_of_string_opt trimmed with
  | None -> Error "Enter a number."
  | Some choice ->
      if choice < 1 || choice > List.length options then
        Error (Printf.sprintf "Enter a number between 1 and %d." (List.length options))
      else
        match List.nth_opt options (choice - 1) with
        | Some option -> Ok (Acp.Message.Selected option.Acp.Message.option_id)
        | None -> Error "Invalid selection."

let print_error io message =
  ensure_newline io;
  let rendered =
    render_element
      (L.box
         ~title:"Error"
         [
           style_element ~supports_ansi:io.supports_ansi ~fg:transcript_muted (L.s message);
         ])
  in
  List.iter (write_line io) (split_lines rendered);
  write_line io ""

let reset_stream_state state =
  Buffer.clear state.stream_buffer;
  state.stream_in_code_block <- false

let reset_turn_state state =
  state.saw_streaming_delta <- false;
  state.saw_visible_message <- false;
  state.saw_error_event <- false;
  reset_stream_state state

let flush_stream_buffer ~final state =
  let buffered = Buffer.contents state.stream_buffer in
  if buffered = "" then ()
  else
    let buffered_ended_with_newline =
      let len = String.length buffered in
      len > 0 && buffered.[len - 1] = '\n'
    in
    let lines, remainder = partition_complete_lines ~final buffered in
    if lines <> [] then begin
      let rendered_lines, next_in_code_block =
        render_markdown_lines_with_state
          ~supports_ansi:state.io.supports_ansi
          ~in_code_block:state.stream_in_code_block
          lines
      in
      write_raw state.io (String.concat "\n" rendered_lines);
      if (not final) || buffered_ended_with_newline then write_raw state.io "\n";
      state.stream_in_code_block <- next_in_code_block;
      state.saw_visible_message <- true
    end;
    Buffer.clear state.stream_buffer;
    Buffer.add_string state.stream_buffer remainder;
    if final then reset_stream_state state

let finish_visible_turn state =
  flush_stream_buffer ~final:true state;
  if state.saw_streaming_delta || state.saw_visible_message then begin
    ensure_newline state.io;
    write_line state.io ""
  end;
  reset_turn_state state

let print_approval_request state pending =
  flush_stream_buffer ~final:true state;
  ensure_newline state.io;
  let info_line label value =
    L.statusCard
      ~label:(style_element ~supports_ansi:state.io.supports_ansi ~fg:transcript_dim (L.s label))
      ~content:(style_element ~supports_ansi:state.io.supports_ansi ~fg:transcript_fg (L.s value))
  in
  let options =
    pending.options
    |> List.map (fun option ->
           L.li
             (style_element
                ~supports_ansi:state.io.supports_ansi
                ~fg:transcript_muted
                (L.s option.Acp.Message.name)))
    |> L.ol
  in
  let rendered =
    render_element
      (L.box
         ~title:"Approval required"
         [
           info_line "Tool" (tool_kind_label pending.tool_call.Acp.Message.kind);
           info_line "Target" (tool_target pending.tool_call);
           options;
         ])
  in
  List.iter (write_line state.io) (split_lines rendered)

let handle_event state = function
  | Acp.Message.Agent_delta { content } ->
      if content <> "" then begin
        Buffer.add_string state.stream_buffer content;
        flush_stream_buffer ~final:false state;
        state.saw_streaming_delta <- true;
      end
  | Acp.Message.Agent_message { content; _ } ->
      if not state.saw_streaming_delta && content <> "" then begin
        let rendered_lines = render_markdown_lines ~supports_ansi:state.io.supports_ansi content in
        let rendered = String.concat "\n" rendered_lines in
        write_raw state.io rendered;
        state.saw_visible_message <- true
      end
  | Acp.Message.Done ->
      finish_visible_turn state
  | Acp.Message.Error { message; _ } ->
      flush_stream_buffer ~final:true state;
      print_error state.io message;
      state.saw_error_event <- true;
      reset_turn_state state
  | Acp.Message.Request_permission { tool_call; options; _ } ->
      let pending = { tool_call; options } in
      print_approval_request state pending;
      state.pending_permission <- Some pending
  | Acp.Message.Initialize _
  | Acp.Message.Initialized
  | Acp.Message.Status _
  | Acp.Message.Session_update _
  | Acp.Message.Agent_plan _ ->
      ()

let print_banner io deps =
  let row label value =
    L.statusCard
      ~label:(style_element ~supports_ansi:io.supports_ansi ~fg:transcript_dim (L.s label))
      ~content:(style_element ~supports_ansi:io.supports_ansi ~fg:transcript_fg (L.s value))
  in
  let rendered =
    render_element
      (L.box
         ~title:"OClaw"
         [
           row "cwd" (prompt_footer_path deps.project_root);
           row "model" deps.model_name;
           row "session" deps.chat_label;
         ])
  in
  List.iter (write_line io) (split_lines rendered);
  write_line io ""

let replay_history io messages =
  let rec loop = function
    | [] -> ()
    | message :: rest ->
        begin
          match render_history_message_with_ansi ~supports_ansi:io.supports_ansi message with
          | [] -> loop rest
          | lines ->
              List.iter (write_line io) lines;
              begin
                match message.Llm_types.role with
                | "assistant" -> write_line io ""
                | _ -> ()
              end;
              loop rest
        end
  in
  loop messages;
  if messages <> [] then write_line io ""

let rec prompt_for_permission state pending =
  match read_prompted_line state.io "choice> " with
  | None -> raise Exit_requested
  | Some line ->
      begin
        match classify_input line with
        | `Exit -> raise Exit_requested
        | `Ignore ->
            write_line state.io "Invalid choice. Enter a number.";
            prompt_for_permission state pending
        | `Prompt raw ->
            begin
              match select_permission_option pending.options raw with
              | Ok outcome -> outcome
              | Error msg ->
                  write_line state.io ("Invalid choice. " ^ msg);
                  prompt_for_permission state pending
            end
      end

let rec handle_pending_permissions state deps =
  match state.pending_permission with
  | None -> ()
  | Some pending ->
      state.pending_permission <- None;
      let outcome = prompt_for_permission state pending in
      begin
        match deps.resolve_permission outcome with
        | Ok () -> handle_pending_permissions state deps
        | Error err ->
            if not state.saw_error_event then print_error state.io err;
            handle_pending_permissions state deps
      end

let run_loop ~input ~output ~persistent deps =
  let io =
    {
      input;
      output;
      input_isatty = Unix.isatty (Unix.descr_of_in_channel input);
      supports_ansi =
        Unix.isatty (Unix.descr_of_out_channel output)
        &&
        (match Sys.getenv_opt "TERM" with
         | Some "dumb"
         | None -> false
         | Some _ -> true);
      last_output_ended_with_newline = true;
    }
  in
  let state =
    {
      io;
      pending_permission = None;
      saw_streaming_delta = false;
      saw_visible_message = false;
      saw_error_event = false;
      stream_buffer = Buffer.create 256;
      stream_in_code_block = false;
    }
  in
  let emit message = handle_event state message in
  print_banner io deps;
  if persistent then replay_history io (deps.history ());
  let rec loop () =
    match read_prompted_line io "> " with
    | None ->
        ensure_newline io
    | Some line ->
        begin
          match classify_input line with
          | `Ignore -> loop ()
          | `Exit ->
              ensure_newline io
          | `Prompt prompt ->
              state.saw_error_event <- false;
              begin
                match deps.process ~emit prompt with
                | Ok () -> ()
                | Error err ->
                    flush_stream_buffer ~final:true state;
                    if not state.saw_error_event then print_error io err
              end;
              handle_pending_permissions state deps;
              loop ()
        end
  in
  try loop () with Exit_requested -> ensure_newline io

let run ~state ~chat_id ~persistent =
  let deps =
    {
      process =
        (fun ~emit prompt ->
          Agent_runtime.Session.process ~emit state ~chat_id ~persistent prompt);
      resolve_permission =
        (fun outcome -> Agent_runtime.Session.resolve_permission state ~chat_id outcome);
      history = (fun () -> Agent_runtime.Session.history state ~chat_id);
      project_root = Agent_runtime.App.project_root state;
      model_name = Agent_runtime.App.model_name state;
      chat_label =
        if persistent then Printf.sprintf "chat:%d persistent" chat_id
        else Printf.sprintf "chat:%d ephemeral" chat_id;
    }
  in
  run_loop ~input:stdin ~output:stdout ~persistent deps
