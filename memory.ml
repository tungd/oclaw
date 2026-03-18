type t = {
  data_dir : string;
  runtime_dir : string;
}

let create ~data_dir ~runtime_dir () =
  { data_dir; runtime_dir }

let global_memory_path t =
  Filename.concat t.data_dir "AGENTS.md"

let chat_memory_path t chat_id =
  Filename.concat t.runtime_dir (Filename.concat "groups" (Filename.concat (string_of_int chat_id) "AGENTS.md"))

let read_file path =
  try Some (Stdlib.In_channel.with_open_bin path Stdlib.In_channel.input_all)
  with _ -> None

let ensure_parent_dir path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" then ()
    else if Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    )
  in
  mkdir_p (Filename.dirname path)

let write_file path content =
  try
    ensure_parent_dir path;
    Stdlib.Out_channel.with_open_bin path (fun channel -> output_string channel content);
    Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let read_global_memory t =
  read_file (global_memory_path t)

let read_chat_memory t chat_id =
  read_file (chat_memory_path t chat_id)

let write_global_memory t content =
  write_file (global_memory_path t) content

let write_chat_memory t chat_id content =
  write_file (chat_memory_path t chat_id) content

let append_memory_section buffer tag content =
  if String.trim content <> "" then (
    Buffer.add_string buffer ("<" ^ tag ^ ">\n");
    Buffer.add_string buffer content;
    Buffer.add_string buffer ("\n</" ^ tag ^ ">\n\n")
  )

let build_memory_context t chat_id =
  let buffer = Buffer.create 256 in
  begin
    match read_global_memory t with
    | Some content -> append_memory_section buffer "global_memory" content
    | None -> ()
  end;
  begin
    match read_chat_memory t chat_id with
    | Some content -> append_memory_section buffer "chat_memory" content
    | None -> ()
  end;
  Buffer.contents buffer

(* Vector search integration *)
let search_similar_memories db ~chat_id ~query ~limit ~threshold =
  match Vector_search.search_similar db ~chat_id query ~limit ~threshold with
  | Ok results -> results  (* Already tuples: (id, memory_id, text, similarity) *)
  | Error _ -> []

let auto_embed_memory db ~chat_id ~memory_id ~content =
  Vector_search.store_embedding db ~chat_id ~memory_id content

let get_vector_stats db ~chat_id =
  match Vector_search.get_stats db ~chat_id with
  | Ok stats -> Some (stats.count, stats.avg_size, stats.created_min, stats.created_max)
  | Error _ -> None
