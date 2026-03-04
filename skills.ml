(** Skills module for OClaw - loading and prompt injection of SKILL.md files. *)

open Yojson.Safe
open Yojson.Safe.Util

module Skill = struct
  type t = {
    name : string;
    description : string;
    version : string;
    always : bool;
    prompts : string list;
    location : string;
  }
end

let strip_quotes s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 &&
     ((s.[0] = '"' && s.[len-1] = '"') || (s.[0] = '\'' && s.[len-1] = '\'')) then
    String.sub s 1 (len - 2)
  else s

let parse_front_matter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | "---" :: rest ->
      let rec collect_pairs acc = function
        | [] -> (List.rev acc, "")
        | "---" :: tail -> (List.rev acc, String.concat "\n" tail)
        | line :: tail ->
            (match String.split_on_char ':' line with
            | key :: value_parts ->
                let key = String.trim (String.lowercase_ascii key) in
                let value = strip_quotes (String.concat ":" value_parts) in
                if key <> "" && value <> "" then
                  collect_pairs ((key, value) :: acc) tail
                else
                  collect_pairs acc tail
            | _ -> collect_pairs acc tail)
      in
      collect_pairs [] rest
  | _ -> ([], content)

let get_fm_string pairs key default =
  match List.assoc_opt key pairs with
  | Some v -> v
  | None -> default

let get_fm_bool pairs key default =
  match List.assoc_opt key pairs with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      v = "true" || v = "yes" || v = "1"
  | None -> default

let load_skill_md path =
  try
    let channel = open_in path in
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;

    let fm, body = parse_front_matter content in
    let dir = Filename.dirname path in
    let name = get_fm_string fm "name" (Filename.basename dir) in
    let description = get_fm_string fm "description" "No description" in
    let version = get_fm_string fm "version" "0.1.0" in
    let always = get_fm_bool fm "always" false in

    Some {
      Skill.name;
      description;
      version;
      always;
      prompts = [String.trim body];
      location = path;
    }
  with _ -> None

let load_skills ?(skills_dir = "skills") () =
  if not (Sys.file_exists skills_dir && Sys.is_directory skills_dir) then
    []
  else
    let entries = Sys.readdir skills_dir in
    Array.to_list entries
    |> List.filter_map (fun entry ->
        let path = Filename.concat skills_dir entry in
        if Sys.is_directory path then
          let skill_md = Filename.concat path "SKILL.md" in
          if Sys.file_exists skill_md then
            load_skill_md skill_md
          else None
        else None
       )

let escape_xml text =
  let b = Buffer.create (String.length text) in
  String.iter (function
    | '&' -> Buffer.add_string b "&amp;"
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '\'' -> Buffer.add_string b "&apos;"
    | c -> Buffer.add_char b c
  ) text;
  Buffer.contents b

let skills_to_prompt skills =
  if skills = [] then ""
  else
    let b = Buffer.create 1024 in
    Buffer.add_string b "## Available Skills\n\n";
    Buffer.add_string b "Skill instructions and tool metadata are preloaded below.\n";
    Buffer.add_string b "Follow these instructions directly; do not read skill files at runtime unless the user asks.\n\n";
    Buffer.add_string b "<available_skills>\n";

    List.iter (fun (skill : Skill.t) ->
      Buffer.add_string b "  <skill>\n";
      Printf.bprintf b "    <name>%s</name>\n" (escape_xml skill.name);
      Printf.bprintf b "    <description>%s</description>\n" (escape_xml skill.description);
      Printf.bprintf b "    <location>%s</location>\n" (escape_xml skill.location);

      if skill.prompts <> [] then (
        Buffer.add_string b "    <instructions>\n";
        List.iter (fun p ->
          Printf.bprintf b "      <instruction>%s</instruction>\n" (escape_xml p)
        ) skill.prompts;
        Buffer.add_string b "    </instructions>\n"
      );

      Buffer.add_string b "  </skill>\n"
    ) skills;

    Buffer.add_string b "</available_skills>";
    Buffer.contents b
