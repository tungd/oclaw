module L = Layoutz

type command =
  [ `Ignore
  | `Exit
  | `New
  | `Fork
  | `Prompt of string
  ]

type deps = {
  process : emit:(Acp.Message.t -> unit) -> string -> (unit, string) result;
  create_conversation : unit -> (int, string) result;
  fork_conversation : unit -> (int, string) result;
  resolve_permission : Acp.Message.permission_outcome -> (unit, string) result;
  history : unit -> Llm_types.message list;
  project_root : string;
  model_name : string;
  chat_label : unit -> string;
  git_branch : unit -> string option;
  token_usage : unit -> string;
}

type pending_permission = {
  tool_call : Acp.Message.tool_call;
  options : Acp.Message.permission_option list;
}

type table_alignment =
  | Align_left
  | Align_center
  | Align_right

type table_block = {
  headers : string list;
  alignments : table_alignment list;
  rows : string list list;
}

type markdown_state = {
  in_code_block : bool;
  pending_table_header : string option;
  pending_table : table_block option;
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
  mutable stream_markdown_state : markdown_state;
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

let get_git_branch ~project_root =
  try
    let cmd = Printf.sprintf "cd %s && git rev-parse --abbrev-ref HEAD 2>/dev/null" (Filename.quote project_root) in
    let chan = Unix.open_process_in cmd in
    let branch = input_line chan in
    let _ = Unix.close_process_in chan in
    if branch <> "" then Some branch else None
  with _ ->
    None

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

let print_prompt_header io (deps : deps) =
  ensure_newline io;
  let header : Prompt_header.t =
    {
      project_root = deps.project_root;
      model_name = deps.model_name;
      chat_label = deps.chat_label ();
      git_branch = deps.git_branch ();
      token_usage = deps.token_usage ();
    }
  in
  let rendered =
    Prompt_header.render
      ~supports_ansi:io.supports_ansi
      header
  in
  write_line io rendered

let read_main_prompted_line io deps =
  print_prompt_header io deps;
  read_prompted_line io "> "

let classify_input line =
  let trimmed = String.trim line in
  if trimmed = "" then
    `Ignore
  else if trimmed = "/exit" || trimmed = "/quit" then
    `Exit
  else if trimmed = "/new" then
    `New
  else if trimmed = "/fork" then
    `Fork
  else
    `Prompt line

let split_lines text =
  match String.split_on_char '\n' text with
  | [] -> [ "" ]
  | lines -> lines

let strip_ansi text =
  let buf = Buffer.create (String.length text) in
  let rec loop idx in_escape =
    if idx >= String.length text then
      Buffer.contents buf
    else
      let c = text.[idx] in
      if in_escape then
        if c = 'm' then loop (idx + 1) false else loop (idx + 1) true
      else if c = '\027' && idx + 1 < String.length text && text.[idx + 1] = '[' then
        loop (idx + 2) true
      else begin
        Buffer.add_char buf c;
        loop (idx + 1) false
      end
  in
  loop 0 false

let utf8_length text =
  let rec loop idx count =
    if idx >= String.length text then count
    else
      let c = Char.code text.[idx] in
      if c land 0xC0 = 0x80 then loop (idx + 1) count
      else loop (idx + 1) (count + 1)
  in
  loop 0 0

let visible_length text =
  utf8_length (strip_ansi text)

let repeat_string count text =
  if count <= 0 then ""
  else String.concat "" (List.init count (fun _ -> text))

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

let initial_markdown_state =
  {
    in_code_block = false;
    pending_table_header = None;
    pending_table = None;
  }

let render_basic_markdown_line ~supports_ansi ~in_code_block line =
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

let split_table_cells line =
  let trimmed = String.trim line in
  if trimmed = "" || not (String.contains trimmed '|') then
    None
  else
    let parts = String.split_on_char '|' trimmed in
    let parts =
      match parts with
      | "" :: rest -> rest
      | _ -> parts
    in
    let parts =
      List.rev parts
      |> (function
          | "" :: rest -> List.rev rest
          | items -> List.rev items)
    in
    let cells = List.map String.trim parts in
    if cells = [] then None else Some cells

let table_alignment_of_cell cell =
  let trimmed = String.trim cell in
  let len = String.length trimmed in
  let left = len > 0 && trimmed.[0] = ':' in
  let right = len > 0 && trimmed.[len - 1] = ':' in
  match left, right with
  | true, true -> Align_center
  | false, true -> Align_right
  | _ -> Align_left

let separator_cell_is_valid cell =
  let trimmed = String.trim cell in
  let len = String.length trimmed in
  if len < 3 then
    false
  else
    let start = if trimmed.[0] = ':' then 1 else 0 in
    let stop =
      if len > start && trimmed.[len - 1] = ':' then len - 1 else len
    in
    let dash_count = stop - start in
    dash_count >= 3
    && String.for_all
         (fun c -> c = '-')
         (String.sub trimmed start dash_count)

let parse_table_separator ~columns line =
  match split_table_cells line with
  | Some cells when List.length cells = columns && List.for_all separator_cell_is_valid cells ->
      Some (List.map table_alignment_of_cell cells)
  | _ -> None

let parse_table_row ~columns line =
  match split_table_cells line with
  | Some cells when List.length cells = columns -> Some cells
  | _ -> None

let is_potential_table_header line =
  match split_table_cells line with
  | Some cells -> cells <> []
  | None -> false

let pad_cell ~alignment width text =
  let len = visible_length text in
  let padding = max 0 (width - len) in
  match alignment with
  | Align_left -> text ^ String.make padding ' '
  | Align_right -> String.make padding ' ' ^ text
  | Align_center ->
      let left_padding = padding / 2 in
      let right_padding = padding - left_padding in
      String.make left_padding ' ' ^ text ^ String.make right_padding ' '

let render_table_lines ~supports_ansi (table : table_block) =
  let header_style = make_style ~fg:transcript_heading ~text_style:L.Style.bold () in
  let cell_style = make_style ~fg:transcript_fg () in
  let render_cell ~style text =
    render_spans ~supports_ansi (parse_inline_spans ~base:style text)
  in
  let rendered_headers =
    List.map (render_cell ~style:header_style) table.headers
  in
  let rendered_rows =
    List.map
      (List.map (render_cell ~style:cell_style))
      table.rows
  in
  let widths =
    List.mapi
      (fun index header ->
        let header_width = visible_length header in
        List.fold_left
          (fun acc row ->
            match List.nth_opt row index with
            | Some cell -> max acc (visible_length cell)
            | None -> acc)
          header_width
          rendered_rows)
      rendered_headers
  in
  let make_border left connector right =
    let parts = List.map (fun width -> repeat_string width "─") widths in
    left ^ "─" ^ String.concat ("─" ^ connector ^ "─") parts ^ "─" ^ right
  in
  let make_row alignments cells =
    let padded =
      List.map2
        (fun width (alignment, cell) -> pad_cell ~alignment width cell)
        widths
        (List.combine alignments cells)
    in
    "│ " ^ String.concat " │ " padded ^ " │"
  in
  let header_row = make_row table.alignments rendered_headers in
  let data_rows =
    List.map (make_row table.alignments) rendered_rows
  in
  [
    make_border "┌" "┬" "┐";
    header_row;
    make_border "├" "┼" "┤";
  ]
  @ data_rows
  @ [ make_border "└" "┴" "┘" ]

let rec render_markdown_line_with_state ~supports_ansi state line =
  match state.pending_table with
  | Some table ->
      begin
        match parse_table_row ~columns:(List.length table.headers) line with
        | Some row ->
            ([],
             {
               state with
               pending_table = Some { table with rows = table.rows @ [ row ] };
             })
        | None ->
            let rendered_table = render_table_lines ~supports_ansi table in
            let rendered_line, next_state =
              render_markdown_line_with_state
                ~supports_ansi
                { state with pending_table = None }
                line
            in
            (rendered_table @ rendered_line, next_state)
      end
  | None ->
      begin
        match state.pending_table_header with
        | Some header_line ->
            begin
              match split_table_cells header_line with
              | Some headers ->
                  begin
                    match parse_table_separator ~columns:(List.length headers) line with
                    | Some alignments ->
                        ([],
                         {
                           state with
                           pending_table_header = None;
                           pending_table = Some { headers; alignments; rows = [] };
                         })
                    | None ->
                        let rendered_header, next_in_code_block =
                          render_basic_markdown_line
                            ~supports_ansi
                            ~in_code_block:state.in_code_block
                            header_line
                        in
                        let rendered_line, next_state =
                          render_markdown_line_with_state
                            ~supports_ansi
                            {
                              in_code_block = next_in_code_block;
                              pending_table_header = None;
                              pending_table = None;
                            }
                            line
                        in
                        (rendered_header :: rendered_line, next_state)
                  end
              | None ->
                  let rendered_header, next_in_code_block =
                    render_basic_markdown_line
                      ~supports_ansi
                      ~in_code_block:state.in_code_block
                      header_line
                  in
                  let rendered_line, next_state =
                    render_markdown_line_with_state
                      ~supports_ansi
                      {
                        in_code_block = next_in_code_block;
                        pending_table_header = None;
                        pending_table = None;
                      }
                      line
                  in
                  (rendered_header :: rendered_line, next_state)
            end
        | None ->
            if state.in_code_block then
              let rendered, next_in_code_block =
                render_basic_markdown_line ~supports_ansi ~in_code_block:true line
              in
              ([ rendered ], { state with in_code_block = next_in_code_block })
            else if starts_with_at line 0 "```" then
              let rendered, next_in_code_block =
                render_basic_markdown_line ~supports_ansi ~in_code_block:false line
              in
              ([ rendered ], { state with in_code_block = next_in_code_block })
            else if is_potential_table_header line then
              ([], { state with pending_table_header = Some line })
            else
              let rendered, next_in_code_block =
                render_basic_markdown_line ~supports_ansi ~in_code_block:false line
              in
              ([ rendered ], { state with in_code_block = next_in_code_block })
      end

let finalize_markdown_state ~supports_ansi state =
  match state.pending_table, state.pending_table_header with
  | Some table, _ ->
      (render_table_lines ~supports_ansi table, { state with pending_table = None })
  | None, Some header_line ->
      let rendered, next_in_code_block =
        render_basic_markdown_line
          ~supports_ansi
          ~in_code_block:state.in_code_block
          header_line
      in
      ([ rendered ],
       {
         in_code_block = next_in_code_block;
         pending_table_header = None;
         pending_table = None;
       })
  | None, None -> ([], state)

let render_markdown_lines_with_state ~supports_ansi ~state ~final lines =
  let rec loop current_state acc = function
    | [] ->
        if final then
          let trailing, next_state = finalize_markdown_state ~supports_ansi current_state in
          (List.rev acc @ trailing, next_state)
        else
          (List.rev acc, current_state)
    | line :: rest ->
        let rendered, next_state =
          render_markdown_line_with_state ~supports_ansi current_state line
        in
        loop next_state (List.rev_append rendered acc) rest
  in
  loop state [] lines

let render_markdown_lines ~supports_ansi text =
  let lines = split_lines text in
  fst
    (render_markdown_lines_with_state
       ~supports_ansi
       ~state:initial_markdown_state
       ~final:true
       lines)

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

let json_int_member name = function
  | `Assoc fields ->
      begin
        match List.assoc_opt name fields with
        | Some (`Int value) -> Some value
        | Some (`Intlit value) -> int_of_string_opt value
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

let tool_kind_of_name = function
  | "read_file" -> Some Acp.Message.Read
  | "write_file"
  | "edit_file" -> Some Acp.Message.Edit
  | "bash" -> Some Acp.Message.Execute
  | "skill_search" -> Some Acp.Message.Search
  | _ -> None

let tool_name_of_title = function
  | Some title when String.starts_with ~prefix:"Tool: " title ->
      let prefix_len = String.length "Tool: " in
      let len = String.length title - prefix_len in
      Some (String.trim (String.sub title prefix_len len))
  | Some title when String.trim title <> "" -> Some (String.trim title)
  | _ -> None

let tool_target_of_input ?default input =
  let fallback = Option.value ~default:"Tool" default in
  match input with
  | Some json ->
      let candidates =
        [
          json_string_member "path" json;
          json_string_member "command" json;
          json_string_member "query" json;
          json_string_member "name" json;
        ]
      in
      Option.value ~default:fallback (List.find_map Fun.id candidates)
  | None -> fallback

let flatten_preview text =
  split_lines text
  |> List.map String.trim
  |> List.filter (fun line -> line <> "")
  |> String.concat " "

let truncate_preview ?(max_len=96) text =
  let preview = flatten_preview text in
  if preview = "" || String.length preview <= max_len then
    preview
  else
    String.sub preview 0 max_len ^ "..."

let format_duration_ms duration_ms =
  if duration_ms < 1000 then
    Printf.sprintf "%dms" duration_ms
  else if duration_ms < 10_000 then
    Printf.sprintf "%.2fs" (float_of_int duration_ms /. 1000.0)
  else
    Printf.sprintf "%.1fs" (float_of_int duration_ms /. 1000.0)

type tool_log_status =
  | Tool_started
  | Tool_completed
  | Tool_failed

type tool_descriptor = {
  label : string;
  target : string;
}

let tool_label ?kind ?name ?title () =
  match kind, name with
  | Some Acp.Message.Other, Some raw_name -> raw_name
  | Some kind, _ -> tool_kind_label (Some kind)
  | None, Some raw_name -> raw_name
  | None, None ->
      begin
        match tool_name_of_title title with
        | Some value -> value
        | None -> tool_kind_label None
      end

let tool_descriptor_of_call tool_call =
  let name = tool_name_of_title tool_call.Acp.Message.title in
  {
    label = tool_label ?kind:tool_call.kind ?name ?title:tool_call.title ();
    target = tool_target tool_call;
  }

let tool_descriptor_of_use (tool_use : Llm_types.tool_use_block) =
  let kind = tool_kind_of_name tool_use.name in
  {
    label = tool_label ?kind ?name:(Some tool_use.name) ();
    target = tool_target_of_input ~default:tool_use.name (Some tool_use.input);
  }

let tool_result_detail_of_call tool_call =
  let from_content =
    match tool_call.Acp.Message.content with
    | Some blocks ->
        begin
          match
            blocks
            |> List.filter_map (function
                   | Acp.Message.Content (Acp.Message.Text text) when String.trim text <> "" -> Some text
                   | _ -> None)
          with
          | [] -> None
          | texts -> Some (String.concat "\n" texts)
        end
    | None -> None
  in
  let from_output =
    match tool_call.Acp.Message.raw_output with
    | Some json -> json_string_member "content" json
    | None -> None
  in
  match from_content with
  | Some text when String.trim text <> "" -> Some (truncate_preview text)
  | _ ->
      begin
        match from_output with
        | Some text when String.trim text <> "" -> Some (truncate_preview text)
        | _ -> None
      end

let render_tool_log_line ~supports_ansi ~status descriptor ?duration ?detail () =
  let status_label =
    match status with
    | Tool_started -> descriptor.label
    | Tool_completed -> descriptor.label ^ " done"
    | Tool_failed -> descriptor.label ^ " failed"
  in
  let meta_parts =
    (match duration with
     | Some value when value <> "" -> [ value ]
     | _ -> [])
    @
    match detail with
    | Some value when String.trim value <> "" -> [ value ]
    | _ -> []
  in
  let spans =
    [
      make_span ~style:(make_style ~fg:transcript_dim ()) "tool> ";
      make_span
        ~style:(make_style ~fg:transcript_muted ~text_style:L.Style.bold ())
        status_label;
      make_span ": ";
      make_span ~style:(make_style ~fg:transcript_code ()) descriptor.target;
    ]
    @
    match meta_parts with
    | value :: rest ->
        let meta = String.concat "; " (value :: rest) in
        [
          make_span " (";
          make_span ~style:(make_style ~fg:transcript_muted ()) meta;
          make_span ")";
        ]
    | _ -> []
  in
  render_spans ~supports_ansi spans

let render_tool_call_update_with_ansi ~supports_ansi tool_call =
  let descriptor = tool_descriptor_of_call tool_call in
  let duration =
    match tool_call.Acp.Message.raw_output with
    | Some json ->
        begin
          match json_int_member "durationMs" json with
          | Some value -> Some (format_duration_ms value)
          | None -> None
        end
    | None -> None
  in
  match tool_call.Acp.Message.status with
  | Some Acp.Message.Completed ->
      Some
        (render_tool_log_line
           ~supports_ansi
           ~status:Tool_completed
           descriptor
           ?duration
           ())
  | Some Acp.Message.Failed ->
      Some
        (render_tool_log_line
           ~supports_ansi
           ~status:Tool_failed
           descriptor
           ?duration
           ?detail:(tool_result_detail_of_call tool_call)
           ())
  | _ -> None

let render_history_message_with_tools ~supports_ansi tool_descriptors (message : Llm_types.message) =
  let render_text role text =
    match role with
    | "user" -> render_user_lines ~supports_ansi text
    | _ -> render_markdown_lines ~supports_ansi text
  in
  match message.content with
  | Llm_types.Text_content text ->
      if text = "" then [] else render_text message.role text
  | Llm_types.Blocks blocks ->
      List.fold_left
        (fun acc block ->
          let rendered =
            match block with
            | Llm_types.Text { text } ->
                if text = "" then [] else render_text message.role text
            | Llm_types.Image _ -> []
            | Llm_types.Tool_use tool_use ->
                let descriptor = tool_descriptor_of_use tool_use in
                Hashtbl.replace tool_descriptors tool_use.id descriptor;
                [
                  render_tool_log_line
                    ~supports_ansi
                    ~status:Tool_started
                    descriptor
                    ();
                ]
            | Llm_types.Tool_result tool_result ->
                let descriptor =
                  match Hashtbl.find_opt tool_descriptors tool_result.tool_use_id with
                  | Some known -> known
                  | None ->
                      {
                        label = tool_kind_label None;
                        target = tool_result.tool_use_id;
                      }
                in
                let status =
                  match tool_result.Llm_types.is_error with
                  | Some true -> Tool_failed
                  | _ -> Tool_completed
                in
                let detail =
                  match status with
                  | Tool_failed -> Some (truncate_preview tool_result.content)
                  | _ -> None
                in
                [
                  render_tool_log_line
                    ~supports_ansi
                    ~status
                    descriptor
                    ?detail
                    ();
                ]
          in
          acc @ rendered)
        []
        blocks

let render_history_message_with_ansi ~supports_ansi message =
  let tool_descriptors = Hashtbl.create 8 in
  render_history_message_with_tools ~supports_ansi tool_descriptors message

let render_history_message message =
  render_history_message_with_ansi ~supports_ansi:false message

let user_message_is_replay_hidden_command (message : Llm_types.message) =
  match message.role, message.content with
  | "user", Llm_types.Text_content text ->
      let trimmed = String.trim text in
      String.equal trimmed "/permissions"
      || String.starts_with ~prefix:"/permissions " trimmed
  | _ -> false

let assistant_message_has_trailing_tool_use (message : Llm_types.message) =
  match message.role, message.content with
  | "assistant", Llm_types.Blocks blocks ->
      begin
        match
          List.fold_left
            (fun last block ->
              match block with
              | Llm_types.Image _ -> last
              | other -> Some other)
            None
            blocks
        with
        | Some (Llm_types.Tool_use _) -> true
        | _ -> false
      end
  | _ -> false

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

let print_notice io ~title message =
  ensure_newline io;
  let rendered =
    render_element
      (L.box
         ~title
         [
           style_element ~supports_ansi:io.supports_ansi ~fg:transcript_fg (L.s message);
         ])
  in
  List.iter (write_line io) (split_lines rendered);
  write_line io ""

let reset_stream_state state =
  Buffer.clear state.stream_buffer;
  state.stream_markdown_state <- initial_markdown_state

let reset_turn_state state =
  state.saw_streaming_delta <- false;
  state.saw_visible_message <- false;
  state.saw_error_event <- false;
  reset_stream_state state

let flush_stream_buffer ~final state =
  let buffered = Buffer.contents state.stream_buffer in
  if buffered = "" && not final then ()
  else
    let buffered_ended_with_newline =
      let len = String.length buffered in
      len > 0 && buffered.[len - 1] = '\n'
    in
    let lines, remainder = partition_complete_lines ~final buffered in
    let rendered_lines, next_markdown_state =
      render_markdown_lines_with_state
        ~supports_ansi:state.io.supports_ansi
        ~state:state.stream_markdown_state
        ~final
        lines
    in
    if rendered_lines <> [] then begin
      write_raw state.io (String.concat "\n" rendered_lines);
      if (not final) || buffered_ended_with_newline then write_raw state.io "\n";
      state.saw_visible_message <- true
    end;
    state.stream_markdown_state <- next_markdown_state;
    Buffer.clear state.stream_buffer;
    Buffer.add_string state.stream_buffer remainder;
    if final then reset_stream_state state

let print_tool_log_lines state lines =
  match lines with
  | [] -> ()
  | _ ->
      flush_stream_buffer ~final:true state;
      ensure_newline state.io;
      List.iter (write_line state.io) lines;
      state.saw_visible_message <- true

let print_tool_call_event state update =
  let rendered_lines =
    match update with
    | Acp.Message.Tool_call tool_call ->
        let descriptor = tool_descriptor_of_call tool_call in
        [
          render_tool_log_line
            ~supports_ansi:state.io.supports_ansi
            ~status:Tool_started
            descriptor
            ();
        ]
    | Acp.Message.Tool_call_update tool_call ->
        begin
          match render_tool_call_update_with_ansi ~supports_ansi:state.io.supports_ansi tool_call with
          | Some line -> [ line ]
          | None -> []
        end
  in
  print_tool_log_lines state rendered_lines

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
  | Acp.Message.Session_update { update; _ } ->
      print_tool_call_event state update
  | Acp.Message.Initialize _
  | Acp.Message.Initialized
  | Acp.Message.Status _
  | Acp.Message.Agent_plan _ ->
      ()

let replay_history io messages =
  let tool_descriptors = Hashtbl.create 16 in
  let rec loop skip_hidden_response = function
    | [] -> ()
    | message :: rest ->
        if user_message_is_replay_hidden_command message then
          loop true rest
        else if skip_hidden_response && String.equal message.Llm_types.role "assistant" then
          loop false rest
        else begin
          match
            render_history_message_with_tools
              ~supports_ansi:io.supports_ansi
              tool_descriptors
              message
          with
          | [] -> loop false rest
          | lines ->
              List.iter (write_line io) lines;
              begin
                match message.Llm_types.role with
                | "assistant" when not (assistant_message_has_trailing_tool_use message) ->
                    write_line io ""
                | _ -> ()
              end;
              loop false rest
        end
  in
  loop false messages;
  if messages <> [] then write_line io ""

let rec prompt_for_permission state pending =
  match read_prompted_line state.io "choice> " with
  | None -> raise Exit_requested
  | Some line ->
      begin
        match classify_input line with
        | `Exit -> raise Exit_requested
        | `New
        | `Fork
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

let handle_session_command io deps ~persistent command =
  let command_name, action, success_message =
    match command with
    | `New ->
        ( "/new",
          deps.create_conversation,
          (fun chat_id ->
            Printf.sprintf "Created new conversation and switched to chat:%d." chat_id) )
    | `Fork ->
        ( "/fork",
          deps.fork_conversation,
          (fun chat_id ->
            Printf.sprintf "Forked current conversation and switched to chat:%d." chat_id) )
    | _ -> invalid_arg "handle_session_command expects /new or /fork"
  in
  if not persistent then
    print_error io (Printf.sprintf "%s requires --persistent." command_name)
  else
    match action () with
    | Ok chat_id ->
        print_notice io ~title:"Session" (success_message chat_id)
    | Error err ->
        print_error io err

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
      stream_markdown_state = initial_markdown_state;
    }
  in
  let emit message = handle_event state message in
  if persistent then replay_history io (deps.history ());
  let rec loop () =
    match read_main_prompted_line io deps with
    | None ->
        ensure_newline io
    | Some line ->
        begin
          match classify_input line with
          | `Ignore -> loop ()
          | `Exit ->
              ensure_newline io
          | `New ->
              handle_session_command io deps ~persistent `New;
              loop ()
          | `Fork ->
              handle_session_command io deps ~persistent `Fork;
              loop ()
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
  let current_chat_id = ref chat_id in
  let project_root = Agent_runtime.App.project_root state in
  let model_name = Agent_runtime.App.model_name state in
  let token_estimator = Llm_provider.Token_estimator.for_model model_name in
  let deps =
    {
      process =
        (fun ~emit prompt ->
          Agent_runtime.Session.process ~emit state ~chat_id:!current_chat_id ~persistent prompt);
      create_conversation =
        (fun () ->
          let new_chat_id = Agent_runtime.Session.create_conversation state () in
          current_chat_id := new_chat_id;
          Ok new_chat_id);
      fork_conversation =
        (fun () ->
          match Agent_runtime.Session.fork_latest_conversation state ~chat_id:!current_chat_id () with
          | Ok new_chat_id ->
              current_chat_id := new_chat_id;
              Ok new_chat_id
          | Error err -> Error err);
      resolve_permission =
        (fun outcome -> Agent_runtime.Session.resolve_permission state ~chat_id:!current_chat_id outcome);
      history = (fun () -> Agent_runtime.Session.history state ~chat_id:!current_chat_id);
      project_root;
      model_name;
      chat_label =
        (fun () ->
          if persistent then Printf.sprintf "chat:%d persistent" !current_chat_id
          else Printf.sprintf "chat:%d ephemeral" !current_chat_id);
      git_branch = (fun () -> get_git_branch ~project_root);
      token_usage =
        (fun () ->
          let messages = Agent_runtime.Session.history state ~chat_id:!current_chat_id in
          let messages_for_counting =
            List.filter_map
              (fun (msg : Llm_types.message) ->
                match msg.content with
                | Llm_types.Text_content text -> Some (msg.role, text)
                | Llm_types.Blocks _ -> None)
              messages
          in
          let prompt_tokens = Llm_provider.Token_estimator.count_messages token_estimator messages_for_counting in
          let usage = Llm_provider.Token_estimator.calculate_usage ~prompt_tokens token_estimator in
          (* Format as "0.0%/128k" *)
          let context_k = usage.context_limit / 1000 in
          Printf.sprintf "%.1f%%/%dk" usage.percentage context_k);
    }
  in
  run_loop ~input:stdin ~output:stdout ~persistent deps
