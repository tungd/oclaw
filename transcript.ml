(** Tree-structured conversation storage with materialized paths. *)

open Sqlite3
open Sqlite3.Data
open Yojson.Safe
module Json = Protocol_conv_json.Json

type node_id = int
type chat_id = int

let node_id_to_yojson n = `Int n
let node_id_of_yojson = function `Int n -> n | _ -> failwith "Expected int for node_id"

let chat_id_to_yojson n = `Int n
let chat_id_of_yojson = function `Int n -> n | _ -> failwith "Expected int for chat_id"

type node_kind = UserPrompt | LLMResponse | ToolCall | ToolResult

type node_metadata = {
  tool_name : string option;
  tool_result_status : string option;
  fork_point : bool;
}
[@@deriving protocol ~driver:(module Json)]

let node_metadata_to_yojson = node_metadata_to_json
let node_metadata_of_yojson json =
  match node_metadata_of_json json with
  | Ok v -> v
  | Error e -> failwith (Json.error_to_string_hum e)

type tree_node = {
  id : node_id;
  path : string;
  chat_id : chat_id;
  kind : node_kind;
  content : Llm_types.message_content;
  model : string option;
  metadata : node_metadata;
  timestamp : float;
}

type conversation_info = {
  id : chat_id;
  title : string option;
  parent_chat_id : chat_id option;
  parent_node_id : node_id option;
  timestamp : float;
}

let conversation_info_to_yojson c =
  `Assoc [
    ("id", `Int c.id);
    ("title", (match c.title with Some s -> `String s | None -> `Null));
    ("parent_chat_id", (match c.parent_chat_id with Some i -> `Int i | None -> `Null));
    ("parent_node_id", (match c.parent_node_id with Some i -> `Int i | None -> `Null));
    ("timestamp", `Float c.timestamp);
  ]

let conversation_info_of_yojson = function
  | `Assoc fields ->
      let id = match List.assoc_opt "id" fields with Some (`Int i) -> i | _ -> failwith "Missing id" in
      let title = match List.assoc_opt "title" fields with Some (`String s) -> Some s | Some `Null -> None | _ -> None in
      let parent_chat_id = match List.assoc_opt "parent_chat_id" fields with Some (`Int i) -> Some i | Some `Null -> None | _ -> None in
      let parent_node_id = match List.assoc_opt "parent_node_id" fields with Some (`Int i) -> Some i | Some `Null -> None | _ -> None in
      let timestamp = match List.assoc_opt "timestamp" fields with Some (`Float f) -> f | _ -> failwith "Missing timestamp" in
      { id; title; parent_chat_id; parent_node_id; timestamp }
  | _ -> failwith "Expected conversation_info object"

type t = { db : Sqlite3.db; data_dir : string }

let node_kind_to_string = function
  | UserPrompt -> "user_prompt" | LLMResponse -> "llm_response"
  | ToolCall -> "tool_call" | ToolResult -> "tool_result"

let node_kind_of_string = function
  | "user_prompt" -> UserPrompt | "llm_response" -> LLMResponse
  | "tool_call" -> ToolCall | "tool_result" -> ToolResult
  | s -> failwith (Printf.sprintf "Unknown node_kind: %s" s)

let message_content_to_yojson = Llm_types.message_content_to_yojson
let message_content_of_yojson = Llm_types.message_content_of_yojson

let init_db db =
  let exec sql = match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc -> failwith (Printf.sprintf "DB error [%s]" (Sqlite3.Rc.to_string rc))
  in
  exec "DROP TABLE IF EXISTS transcripts";
  exec "DROP TABLE IF EXISTS conversations";
  exec "CREATE TABLE conversations (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, parent_chat_id INTEGER NULL, parent_node_id INTEGER NULL, timestamp REAL NOT NULL)";
  exec "CREATE TABLE transcripts (id INTEGER PRIMARY KEY AUTOINCREMENT, chat_id INTEGER NOT NULL, path TEXT NOT NULL, kind TEXT NOT NULL, content TEXT NOT NULL, model TEXT NULL, metadata TEXT NOT NULL, timestamp REAL NOT NULL, FOREIGN KEY(chat_id) REFERENCES conversations(id))";
  exec "CREATE INDEX idx_transcripts_chat ON transcripts(chat_id)";
  exec "CREATE INDEX idx_transcripts_path ON transcripts(chat_id, path)";
  exec "CREATE INDEX idx_conversations_parent ON conversations(parent_chat_id)";
  exec "INSERT INTO conversations (title, timestamp) VALUES ('Default', (SELECT strftime('%s', 'now')))"

let next_node_id t =
  let sql = "SELECT COALESCE(MAX(id), 0) + 1 FROM transcripts" in
  let stmt = Sqlite3.prepare t.db sql in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> let id = Sqlite3.column_int stmt 0 in let _ = Sqlite3.finalize stmt in id
  | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in 1
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Printf.sprintf "DB error: %s" (Sqlite3.Rc.to_string rc))

let insert_node t ~chat_id ~path ~kind ~content ~model ~metadata =
  let id = next_node_id t in
  let kind_str = node_kind_to_string kind in
  let content_json = Yojson.Safe.to_string (message_content_to_yojson content) in
  let metadata_json = Yojson.Safe.to_string (node_metadata_to_yojson metadata) in
  let model_opt = Option.value model ~default:"" in
  let timestamp = Unix.gettimeofday () in
  let sql = "INSERT INTO transcripts (id, chat_id, path, kind, content, model, metadata, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 id in
  let _ = Sqlite3.bind_int stmt 2 chat_id in
  let _ = Sqlite3.bind_text stmt 3 path in
  let _ = Sqlite3.bind_text stmt 4 kind_str in
  let _ = Sqlite3.bind_text stmt 5 content_json in
  let _ = Sqlite3.bind_text stmt 6 model_opt in
  let _ = Sqlite3.bind_text stmt 7 metadata_json in
  let _ = Sqlite3.bind_double stmt 8 timestamp in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in id
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Printf.sprintf "DB error: %s" (Sqlite3.Rc.to_string rc))
  | Sqlite3.Rc.ROW -> let _ = Sqlite3.finalize stmt in failwith "Unexpected row"

let row_to_node stmt =
  let id = Sqlite3.column_int stmt 0 in
  let path = Sqlite3.column_text stmt 1 in
  let chat_id = Sqlite3.column_int stmt 2 in
  let kind = node_kind_of_string (Sqlite3.column_text stmt 3) in
  let content_json = Yojson.Safe.from_string (Sqlite3.column_text stmt 4) in
  let model = let m = Sqlite3.column_text stmt 5 in if m = "" then None else Some m in
  let metadata_json = Yojson.Safe.from_string (Sqlite3.column_text stmt 6) in
  let timestamp = Sqlite3.column_double stmt 7 in
  let content = match message_content_of_yojson content_json with Ok c -> c | Error e -> failwith e in
  let metadata = node_metadata_of_yojson metadata_json in
  { id; path; chat_id; kind; content; model; metadata; timestamp }

let create ~data_dir ~runtime_dir:_ =
  let db_path = Filename.concat data_dir "transcript.db" in
  let db = Sqlite3.db_open db_path in
  init_db db;
  { db; data_dir }

let close t = let _ = Sqlite3.db_close t.db in ()

let create_conversation t ?title ?parent_chat_id ?parent_node_id () =
  let title_opt = Option.value title ~default:"" in
  let timestamp = Unix.gettimeofday () in
  let sql = "INSERT INTO conversations (title, parent_chat_id, parent_node_id, timestamp) VALUES (?, ?, ?, ?)" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_text stmt 1 title_opt in
  let _ = match parent_chat_id with None -> Sqlite3.bind stmt 2 Data.NULL | Some id -> Sqlite3.bind_int stmt 2 id in
  let _ = match parent_node_id with None -> Sqlite3.bind stmt 3 Data.NULL | Some id -> Sqlite3.bind_int stmt 3 id in
  let _ = Sqlite3.bind_double stmt 4 timestamp in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> let id = Int64.to_int (Sqlite3.last_insert_rowid t.db) in let _ = Sqlite3.finalize stmt in id
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  | Sqlite3.Rc.ROW -> let _ = Sqlite3.finalize stmt in failwith "Unexpected row"

let get_conversation t chat_id =
  let sql = "SELECT id, title, parent_chat_id, parent_node_id, timestamp FROM conversations WHERE id = ?" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 chat_id in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let id = Sqlite3.column_int stmt 0 in
      let title = Sqlite3.column_text stmt 1 in
      let parent_chat_id = match Sqlite3.column stmt 2 with Data.INT i -> Some (Int64.to_int i) | _ -> None in
      let parent_node_id = match Sqlite3.column stmt 3 with Data.INT i -> Some (Int64.to_int i) | _ -> None in
      let timestamp = Sqlite3.column_double stmt 4 in
      let _ = Sqlite3.finalize stmt in
      Some { id; title = if title = "" then None else Some title; parent_chat_id; parent_node_id; timestamp }
  | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in None
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)

let get_all_conversations t =
  let sql = "SELECT id, title, parent_chat_id, parent_node_id, timestamp FROM conversations ORDER BY timestamp DESC" in
  let stmt = Sqlite3.prepare t.db sql in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let id = Sqlite3.column_int stmt 0 in
        let title = Sqlite3.column_text stmt 1 in
        let parent_chat_id = match Sqlite3.column stmt 2 with Data.INT i -> Some (Int64.to_int i) | _ -> None in
        let parent_node_id = match Sqlite3.column stmt 3 with Data.INT i -> Some (Int64.to_int i) | _ -> None in
        let timestamp = Sqlite3.column_double stmt 4 in
        loop ({ id; title = if title = "" then None else Some title; parent_chat_id; parent_node_id; timestamp } :: acc)
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  in loop []

let delete_conversation t chat_id =
  let stmt = Sqlite3.prepare t.db "DELETE FROM transcripts WHERE chat_id = ?" in
  let _ = Sqlite3.bind_int stmt 1 chat_id in
  let _ = Sqlite3.step stmt in
  let _ = Sqlite3.finalize stmt in
  let stmt = Sqlite3.prepare t.db "DELETE FROM conversations WHERE id = ?" in
  let _ = Sqlite3.bind_int stmt 1 chat_id in
  let _ = Sqlite3.step stmt in
  let _ = Sqlite3.finalize stmt in
  ()

let rec get_node t node_id =
  let sql = "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE id = ?" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 node_id in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> let node = row_to_node stmt in let _ = Sqlite3.finalize stmt in Some node
  | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in None
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)

let add_user_prompt t ~chat_id ?parent_id ~content () =
  let metadata = { tool_name = None; tool_result_status = None; fork_point = false } in
  match parent_id with
  | Some pid ->
      let parent = match get_node t pid with Some n -> n | None -> failwith "Parent not found" in
      if parent.chat_id <> chat_id then failwith "parent_id must belong to same chat";
      let new_id = next_node_id t in
      let path = Tree.make_child_path ~parent_path:parent.path ~child_id:new_id in
      ignore (insert_node t ~chat_id ~path ~kind:UserPrompt ~content:(Llm_types.Text_content content) ~model:None ~metadata);
      new_id
  | None ->
      let new_id = next_node_id t in
      let path = Printf.sprintf "/%d" new_id in
      ignore (insert_node t ~chat_id ~path ~kind:UserPrompt ~content:(Llm_types.Text_content content) ~model:None ~metadata);
      new_id

let add_llm_response t ~parent_id ?model ~content () =
  let parent = match get_node t parent_id with Some n -> n | None -> failwith "Parent not found" in
  let metadata = { tool_name = None; tool_result_status = None; fork_point = false } in
  let new_id = next_node_id t in
  let path = Tree.make_child_path ~parent_path:parent.path ~child_id:new_id in
  ignore (insert_node t ~chat_id:parent.chat_id ~path ~kind:LLMResponse ~content ~model ~metadata);
  new_id

let add_tool_call t ~parent_id ~tool_name ~input =
  let parent = match get_node t parent_id with Some n -> n | None -> failwith "Parent not found" in
  let metadata = { tool_name = Some tool_name; tool_result_status = None; fork_point = false } in
  let content = Llm_types.Blocks [Llm_types.Tool_use { id = ""; name = tool_name; input }] in
  let new_id = next_node_id t in
  let path = Tree.make_child_path ~parent_path:parent.path ~child_id:new_id in
  ignore (insert_node t ~chat_id:parent.chat_id ~path ~kind:ToolCall ~content ~model:None ~metadata);
  new_id

let add_tool_result t ~parent_id ~tool_use_id ~content ~is_error =
  let parent = match get_node t parent_id with Some n -> n | None -> failwith "Parent not found" in
  let status = if is_error then Some "error" else Some "success" in
  let metadata = { tool_name = None; tool_result_status = status; fork_point = false } in
  let content = Llm_types.Blocks [Llm_types.Tool_result { tool_use_id; content; is_error = Some is_error }] in
  let new_id = next_node_id t in
  let path = Tree.make_child_path ~parent_path:parent.path ~child_id:new_id in
  ignore (insert_node t ~chat_id:parent.chat_id ~path ~kind:ToolResult ~content ~model:None ~metadata);
  new_id

let get_children t node_id =
  let node = match get_node t node_id with Some n -> n | None -> failwith "Node not found" in
  let child_prefix = node.path ^ "/" in
  let sql = "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE chat_id = ? AND path LIKE ? ORDER BY path" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 node.chat_id in
  let _ = Sqlite3.bind_text stmt 2 (child_prefix ^ "%") in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let candidate = row_to_node stmt in
        if Tree.path_depth candidate.path = Tree.path_depth node.path + 1 then loop (candidate :: acc) else loop acc
    | Sqlite3.Rc.DONE -> List.rev acc
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  in loop []

let get_parent t node_id =
  let node = match get_node t node_id with Some n -> n | None -> failwith "Node not found" in
  match Tree.parent_path node.path with
  | None -> None
  | Some parent_path_str ->
      let sql = "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE chat_id = ? AND path = ?" in
      let stmt = Sqlite3.prepare t.db sql in
      let _ = Sqlite3.bind_int stmt 1 node.chat_id in
      let _ = Sqlite3.bind_text stmt 2 parent_path_str in
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> let parent = row_to_node stmt in let _ = Sqlite3.finalize stmt in Some parent
      | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in None
      | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)

let get_path_to_root t node_id =
  let node = match get_node t node_id with Some n -> n | None -> failwith "Node not found" in
  let ids = Tree.path_to_ids node.path in
  let id_list = String.concat "," (List.map string_of_int ids) in
  let sql = Printf.sprintf "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE id IN (%s) AND chat_id = ? ORDER BY path ASC" id_list in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 node.chat_id in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> let n = row_to_node stmt in loop (n :: acc)
    | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in List.sort (fun a b -> compare (Tree.path_depth a.path) (Tree.path_depth b.path)) acc
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  in loop []

let get_roots t ~chat_id =
  let sql = "SELECT id FROM transcripts WHERE chat_id = ? AND kind = 'user_prompt' AND path LIKE '/%' ORDER BY timestamp" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 chat_id in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> let id = Sqlite3.column_int stmt 0 in loop (id :: acc)
    | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in List.rev acc
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  in loop []

let get_branch t node_id =
  let _ = match get_node t node_id with Some n -> n | None -> failwith "Node not found" in
  let path_nodes = get_path_to_root t node_id in
  let node_to_message n = match n.kind with
    | UserPrompt -> { Llm_types.role = "user"; content = n.content }
    | LLMResponse -> { Llm_types.role = "assistant"; content = n.content }
    | ToolCall -> { Llm_types.role = "assistant"; content = n.content }
    | ToolResult -> { Llm_types.role = "user"; content = n.content }
  in List.map node_to_message path_nodes

let get_subtree t node_id =
  let node = match get_node t node_id with Some n -> n | None -> failwith "Node not found" in
  let prefix = node.path ^ "/" in
  let sql = "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE chat_id = ? AND path LIKE ? ORDER BY path" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 node.chat_id in
  let _ = Sqlite3.bind_text stmt 2 (prefix ^ "%") in
  let rec loop acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> let n = row_to_node stmt in loop (n :: acc)
    | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in List.rev acc
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
  in loop []

let get_latest_node t ~chat_id =
  let sql = "SELECT id FROM transcripts WHERE chat_id = ? ORDER BY timestamp DESC LIMIT 1" in
  let stmt = Sqlite3.prepare t.db sql in
  let _ = Sqlite3.bind_int stmt 1 chat_id in
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> let id = Sqlite3.column_int stmt 0 in let _ = Sqlite3.finalize stmt in Some id
  | Sqlite3.Rc.DONE -> let _ = Sqlite3.finalize stmt in None
  | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)

let fork_conversation t ~chat_id node_id ?title () =
  let branch = get_path_to_root t node_id in
  let branch = List.sort (fun a b -> compare (Tree.path_depth a.path) (Tree.path_depth b.path)) branch in
  let new_chat_id = create_conversation t ~title:(Option.value ~default:"Forked conversation" title) ~parent_chat_id:chat_id ~parent_node_id:node_id () in
  let path_map = ref [] in
  List.iter (fun old_node ->
    let new_parent_path = match Tree.parent_path old_node.path with None -> "" | Some pp -> List.assoc pp !path_map in
    let content_json = Yojson.Safe.to_string (message_content_to_yojson old_node.content) in
    let metadata_json = Yojson.Safe.to_string (node_metadata_to_yojson old_node.metadata) in
    let model_opt = Option.value old_node.model ~default:"" in
    let sql = "INSERT INTO transcripts (chat_id, path, kind, content, model, metadata, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?)" in
    let stmt = Sqlite3.prepare t.db sql in
    let _ = Sqlite3.bind_int stmt 1 new_chat_id in
    let temp_path = if new_parent_path = "" then "/0" else Tree.make_child_path ~parent_path:new_parent_path ~child_id:0 in
    let _ = Sqlite3.bind_text stmt 2 temp_path in
    let _ = Sqlite3.bind_text stmt 3 (node_kind_to_string old_node.kind) in
    let _ = Sqlite3.bind_text stmt 4 content_json in
    let _ = Sqlite3.bind_text stmt 5 model_opt in
    let _ = Sqlite3.bind_text stmt 6 metadata_json in
    let _ = Sqlite3.bind_double stmt 7 old_node.timestamp in
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE ->
        let new_id = Int64.to_int (Sqlite3.last_insert_rowid t.db) in
        let _ = Sqlite3.finalize stmt in
        let new_path = if new_parent_path = "" then Printf.sprintf "/%d" new_id else Tree.make_child_path ~parent_path:new_parent_path ~child_id:new_id in
        path_map := (old_node.path, new_path) :: !path_map
    | rc -> let _ = Sqlite3.finalize stmt in failwith (Sqlite3.Rc.to_string rc)
    | Sqlite3.Rc.ROW -> let _ = Sqlite3.finalize stmt in failwith "Unexpected row"
  ) branch;
  new_chat_id

let tree_node_to_yojson (n : tree_node) =
  `Assoc [
    ("id", `Int n.id);
    ("path", `String n.path);
    ("kind", `String (node_kind_to_string n.kind));
    ("content", Llm_types.message_content_to_yojson n.content);
    ("model", (match n.model with Some s -> `String s | None -> `Null));
    ("metadata", node_metadata_to_yojson n.metadata);
    ("timestamp", `Float n.timestamp);
  ]

let export_json t ~chat_id =
  let conversations = get_all_conversations t in
  match List.find_opt (fun c -> c.id = chat_id) conversations with
  | None -> "{}"
  | Some c ->
      let sql = "SELECT id, path, chat_id, kind, content, model, metadata, timestamp FROM transcripts WHERE chat_id = ? ORDER BY path" in
      let stmt = Sqlite3.prepare t.db sql in
      let _ = Sqlite3.bind_int stmt 1 chat_id in
      let nodes_list : tree_node list ref = ref [] in
      let rec load_nodes () = match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> let n = row_to_node stmt in nodes_list := n :: !nodes_list; load_nodes ()
        | Sqlite3.Rc.DONE -> ()
        | rc -> failwith (Sqlite3.Rc.to_string rc)
      in
      let _ = load_nodes () in
      let _ = Sqlite3.finalize stmt in
      let nodes_json = List.map tree_node_to_yojson (List.rev !nodes_list) in
      let conv_json = conversation_info_to_yojson c in
      Yojson.Safe.to_string (`Assoc [("conversation", conv_json); ("nodes", `List nodes_json)])

let to_linear_messages t ~chat_id = match get_latest_node t ~chat_id with Some node_id -> get_branch t node_id | None -> []

open Tyxml.Html

let kind_to_string_html = function UserPrompt -> "User" | LLMResponse -> "Assistant" | ToolCall -> "Tool Call" | ToolResult -> "Tool Result"
let kind_to_class_html = function UserPrompt -> "user-prompt" | LLMResponse -> "llm-response" | ToolCall -> "tool-call" | ToolResult -> "tool-result"
let kind_to_icon_html = function UserPrompt -> "📝" | LLMResponse -> "🤖" | ToolCall -> "🔧" | ToolResult -> "✅"

let format_timestamp ts =
  let tm = Unix.localtime ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let content_to_string_html = function
  | Llm_types.Text_content s -> s
  | Llm_types.Blocks blocks ->
      let block_to_string = function
        | Llm_types.Text { text } -> text
        | Llm_types.Image { source = _ } -> "[Image]"
        | Llm_types.Tool_use { name; input; _ } -> Printf.sprintf "[Tool: %s] %s" name (Yojson.Safe.to_string input)
        | Llm_types.Tool_result { tool_use_id; content; _ } -> Printf.sprintf "[Tool Result: %s] %s" tool_use_id content
      in String.concat "\n" (List.map block_to_string blocks)

let rec render_node t node =
  let kind_class = kind_to_class_html node.kind in
  let fork_class = if node.metadata.fork_point then " fork-marker" else "" in
  let kind_icon = kind_to_icon_html node.kind in
  let model_badge = match node.model with Some m -> span ~a:[a_class ["model-badge"]] [txt m] | None -> txt "" in
  let children = get_children t node.id in
  let children_html = List.map (render_node t) children in
  div ~a:[a_class ["tree-node"; kind_class; fork_class]] [
    div ~a:[a_class ["node-header"]] [
      span ~a:[a_class ["kind-icon"]] [txt kind_icon];
      span ~a:[a_class ["kind-label"]] [txt (kind_to_string_html node.kind)];
      model_badge;
      span ~a:[a_class ["timestamp"]] [txt (format_timestamp node.timestamp)]
    ];
    div ~a:[a_class ["node-content"]] [pre [txt (content_to_string_html node.content)]];
    if children_html <> [] then div ~a:[a_class ["children"]] children_html else txt ""
  ]

let css_style =
  style [txt {|
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f5f5f5; padding: 20px; line-height: 1.6; }
    h1 { margin-bottom: 20px; color: #333; font-size: 24px; }
    .tree { max-width: 1200px; margin: 0 auto; }
    .tree-node { background: white; border-radius: 8px; margin-bottom: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); overflow: hidden; }
    .node-header { display: flex; align-items: center; gap: 8px; padding: 10px 14px; background: #f8f9fa; border-bottom: 1px solid #eee; font-size: 13px; }
    .kind-icon { font-size: 16px; }
    .kind-label { font-weight: 600; color: #555; }
    .model-badge { background: #e3f2fd; color: #1976d2; padding: 2px 8px; border-radius: 12px; font-size: 11px; font-weight: 500; }
    .timestamp { margin-left: auto; color: #999; font-size: 11px; }
    .node-content { padding: 14px; }
    .node-content pre { white-space: pre-wrap; word-wrap: break-word; font-family: 'SF Mono', Monaco, monospace; font-size: 13px; color: #333; max-height: 400px; overflow-y: auto; }
    .children { padding: 0 14px 14px 14px; border-top: 1px solid #f0f0f0; margin-top: 10px; padding-top: 10px; }
    .user-prompt .node-header { background: #e8f5e9; }
    .llm-response .node-header { background: #e3f2fd; }
    .tool-call .node-header { background: #fff3e0; }
    .tool-result .node-header { background: #f3e5f5; }
    .fork-marker .node-header { border-left: 3px solid #ffc107; }
  |}]

let export_html t ~chat_id ~out_path =
  let conv = get_conversation t chat_id in
  let roots = get_roots t ~chat_id in
  let root_nodes = List.filter_map (fun id -> get_node t id) roots in
  let page_title = match conv with Some c -> (match c.title with Some t -> t | None -> Printf.sprintf "Chat #%d" chat_id) | None -> Printf.sprintf "Chat #%d" chat_id in
  let root_elements = List.map (render_node t) root_nodes in
  let html = html (head (Tyxml.Html.title (txt (Printf.sprintf "OClaw Transcript - %s" page_title))) [css_style])
      (body [h1 [txt (Printf.sprintf "Transcript - %s" page_title)]; div ~a:[a_class ["tree"]] root_elements]) in
  let oc = open_out out_path in
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp () fmt html;
  Format.pp_print_flush fmt ();
  close_out oc
