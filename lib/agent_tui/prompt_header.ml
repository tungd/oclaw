module L = Layoutz

type t = {
  project_root : string;
  model_name : string;
  chat_label : string;
  git_branch : string option;
  token_usage : string;
}

let transcript_subdued = L.Color.rgb 140 140 140

let style_element ~supports_ansi ?fg ?(text_style=L.Style.none) element =
  if not supports_ansi then
    element
  else
    match fg, text_style with
    | None, style when style = L.Style.none -> element
    | Some fg, style when style = L.Style.none -> L.withStyle ~fg element
    | None, style -> L.withStyle ~style element
    | Some fg, style -> L.withStyle ~fg ~style element

let prompt_footer_path path =
  let home =
    try Sys.getenv "HOME" with Not_found -> ""
  in
  if home <> "" && String.starts_with ~prefix:home path then
    "~" ^ String.sub path (String.length home) (String.length path - String.length home)
  else
    path

let compact_chat_label label =
  if String.contains label ' ' then
    String.sub label 0 (String.index label ' ')
  else
    label

let text header =
  let cwd_path = prompt_footer_path header.project_root in
  let branch_str =
    match header.git_branch with
    | Some branch -> Printf.sprintf " (%s)" branch
    | None -> ""
  in
  Printf.sprintf "%s%s -- %s -- %s -- %s"
    cwd_path branch_str header.model_name (compact_chat_label header.chat_label) header.token_usage

let render ~supports_ansi header =
  let line = text header in
  L.render
    (style_element
       ~supports_ansi
       ~fg:transcript_subdued
       (L.s line))
