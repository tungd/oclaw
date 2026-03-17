(** Graph-based execution engine for OClaw with parallel support *)

open Yojson.Safe
module Log = (val Logs.src_log (Logs.Src.create "graph_engine") : Logs.LOG)

type node_id = string
type edge_id = string

type node_type =
  | LLMNode of Llm_provider.provider_config
  | ToolNode of string
  | MemoryReadNode of memory_query
  | MemoryWriteNode of memory_write
  | MemorySummarizeNode
  | ConditionNode of (unit -> bool)
  | ParallelNode of node_id list
  | EndNode

and memory_query = {
  query_type : memory_query_type;
  keys : string list;
  limit : int option;
  filters : (string * string) list;
}

and memory_query_type =
  | ExactMatch
  | SemanticSearch
  | TemporalRange
  | TagBased

and memory_write = {
  write_type : memory_write_type;
  key : string;
  value : Yojson.Safe.t;
  metadata : memory_metadata;
}

and memory_write_type =
  | Set
  | Append
  | Merge
  | Delete

and memory_metadata = {
  created_at : float;
  updated_at : float;
  importance : float;
  tags : string list;
  source : string;
  ttl : float option;
  summary : string option;
}

type edge = {
  edge_id : edge_id;
  from_node : node_id;
  to_node : node_id;
  condition : (memory_context -> bool) option;
  priority : int;
}

type node = {
  node_id : node_id;
  node_type : node_type;
  description : string;
  input_schema : Yojson.Safe.t option;
  output_schema : Yojson.Safe.t option;
  retry_count : int;
  timeout_seconds : int option;
}

and memory_context = {
  working_memory : (string * Yojson.Safe.t) list;
  conversation_history : Llm_types.message list;
  execution_trace : execution_step list;
  variables : (string * Yojson.Safe.t) list;
}

and execution_step = {
  step_id : int;
  node_id : node_id;
  timestamp : float;
  input : Yojson.Safe.t option;
  output : Yojson.Safe.t option;
  error : string option;
  duration_ms : int;
}

type execution_graph = {
  graph_id : string;
  nodes : (node_id * node) list;
  edges : (edge_id * edge) list;
  start_node : node_id;
  end_nodes : node_id list;
  metadata : (string * string) list;
}

type execution_result =
  | Success of Yojson.Safe.t * memory_context
  | Failure of string * memory_context
  | Partial of Yojson.Safe.t * memory_context * node_id

type graph_builder = {
  mutable nodes : (node_id * node) list;
  mutable edges : (edge_id * edge) list;
  mutable start_node : node_id option;
  mutable end_nodes : node_id list;
  mutable next_id : int;
}

module type MEMORY_STORE = sig
  type t
  
  val create : unit -> t
  val get : t -> string -> Yojson.Safe.t option
  val set : t -> string -> Yojson.Safe.t -> memory_metadata -> unit
  val delete : t -> string -> unit
  val search : t -> memory_query -> (string * Yojson.Safe.t * memory_metadata) list
  val summarize : t -> string list -> Yojson.Safe.t
  val export : t -> Yojson.Safe.t
  val import : t -> Yojson.Safe.t -> unit
  val clear : t -> unit
end

module type EXECUTION_ENGINE = sig
  type t
  
  val create : execution_graph -> memory_store -> t
  val execute : t -> node_id -> Yojson.Safe.t option -> memory_context -> execution_result
  val execute_from_start : t -> Yojson.Safe.t option -> memory_context -> execution_result
  val step : t -> node_id -> Yojson.Safe.t option -> memory_context -> (execution_result * t)
  val get_current_node : t -> node_id option
  val get_execution_trace : t -> execution_step list
end

(* In-memory memory store implementation *)
module MemoryStore : MEMORY_STORE = struct
  type t = {
    data : (string * (Yojson.Safe.t * memory_metadata)) list ref;
  }
  
  let create () = { data = ref [] }
  
  let get store key =
    List.assoc_opt key !(store.data) |> Option.map fst
  
  let set store key value metadata =
    let entry = (key, (value, { metadata with updated_at = Unix.gettimeofday () })) in
    store.data := List.remove_assoc key !(store.data);
    store.data := entry :: !(store.data)
  
  let delete store key =
    store.data := List.remove_assoc key !(store.data)
  
  let search store query =
    !(store.data)
    |> List.filter_map (fun (key, (value, meta)) ->
         if List.mem_assoc key query.keys || meta.tags |> List.exists (fun t -> List.mem_assoc t query.filters) then
           Some (key, value, meta)
         else
           None)
    |> function
       | results when query.limit <> None -> List.take (Option.get query.limit) results
       | results -> results
  
  let summarize store keys =
    `Assoc [
      ("keys", `List (List.map (fun k -> `String k) keys));
      ("count", `Int (List.length keys));
      ("timestamp", `Float (Unix.gettimeofday ()));
    ]
  
  let export store =
    `Assoc (List.map (fun (k, (v, _)) -> (k, v)) !(store.data))
  
  let import store json =
    match json with
    | `Assoc pairs ->
        store.data := List.map (fun (k, v) -> (k, (v, {
          created_at = Unix.gettimeofday ();
          updated_at = Unix.gettimeofday ();
          importance = 0.5;
          tags = [];
          source = "import";
          ttl = None;
          summary = None;
        }))) pairs |> ref
    | _ -> ()
  
  let clear store =
    store.data := []
end

(* Execution engine with parallel support using Domainslib *)
module ExecutionEngine : EXECUTION_ENGINE = struct
  type t = {
    graph : execution_graph;
    memory : MemoryStore.t;
    mutable current_node : node_id option;
    mutable trace : execution_step list;
    mutable step_counter : int;
  }
  
  let create graph memory = {
    graph;
    memory;
    current_node = None;
    trace = [];
    step_counter = 0;
  }
  
  let add_step t node_id input output error duration_ms =
    let step = {
      step_id = t.step_counter;
      node_id;
      timestamp = Unix.gettimeofday ();
      input;
      output;
      error;
      duration_ms;
    } in
    t.step_counter <- t.step_counter + 1;
    t.trace <- t.trace @ [step]
  
  let find_node graph node_id =
    List.assoc_opt node_id graph.nodes
  
  let find_next_nodes graph node_id context =
    List.filter_map (fun (_, edge) ->
      if edge.from_node = node_id then
        match edge.condition with
        | None -> Some (edge.to_node, edge.priority)
        | Some cond -> if cond context then Some (edge.to_node, edge.priority) else None
      else
        None
    ) graph.edges
    |> List.sort (fun (_, p1) (_, p2) -> compare p2 p1)
    |> List.map fst
  
  let execute_tool tools ~chat_id name input =
    let started = Unix.gettimeofday () in
    let result = Tools.execute tools ~chat_id name input in
    let duration_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
    (result, duration_ms)
  
  let execute_node t node_id input context tools ~chat_id =
    match find_node t.graph node_id with
    | None -> 
        let error = Printf.sprintf "Node not found: %s" node_id in
        (Failure (error, context), t)
    | Some node ->
        t.current_node <- Some node_id;
        let started = Unix.gettimeofday () in
        let result =
          match node.node_type with
          | ToolNode tool_name ->
              let input_json = Option.value ~default:`Null input in
              let (tool_result, duration_ms) = execute_tool tools ~chat_id tool_name input_json in
              let output = 
                if tool_result.Tools.is_error then
                  `Assoc [("error", `String tool_result.Tools.content)]
                else
                  `String tool_result.Tools.content
              in
              add_step t node_id input (Some output) None duration_ms;
              if tool_result.Tools.is_error then
                Failure (tool_result.Tools.content, context)
              else
                Success (output, context)
          
          | ConditionNode cond ->
              let result = if cond () then `Bool true else `Bool false in
              add_step t node_id input (Some result) None 0;
              Success (result, context)
          
          | MemoryReadNode query ->
              let results = MemoryStore.search t.memory query in
              let output = `List (List.map (fun (_, v, _) -> v) results) in
              add_step t node_id input (Some output) None 0;
              Success (output, context)
          
          | MemoryWriteNode write ->
              MemoryStore.set t.memory write.key write.value write.metadata;
              let output = `Assoc [("status", `String "written"); ("key", `String write.key)] in
              add_step t node_id input (Some output) None 0;
              Success (output, context)
          
          | MemorySummarizeNode ->
              let keys = List.map fst context.working_memory in
              let summary = MemoryStore.summarize t.memory keys in
              add_step t node_id input (Some summary) None 0;
              Success (summary, context)
          
          | ParallelNode node_ids ->
              (* Execute multiple nodes in parallel using Domainslib *)
              let started = Unix.gettimeofday () in
              let pool = Domainslib.Task.setup_pool ~num_domains:(min 4 (Sys.cpu_count ())) () in
              try
                let futures = List.map (fun nid ->
                  Domainslib.Task.run pool (fun _ ->
                    execute_node t nid input context tools ~chat_id
                  )
                ) node_ids in
                
                let results = List.map (Domainslib.Task.await pool) futures in
                
                Domainslib.Task.teardown_pool pool;
                
                let outputs = List.filter_map (function
                  | (Success (out, _), _) -> Some out
                  | (Failure (err, _), _) -> Some (`String err)
                  | (Partial (out, _, _), _) -> Some out
                ) results in
                
                let errors = List.filter_map (function
                  | (Failure (err, _), _) -> Some err
                  | _ -> None
                ) results in
                
                let duration_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
                let output = `Assoc [
                  ("parallel_results", `List outputs);
                  ("errors", `List (List.map (fun e -> `String e) errors));
                  ("node_count", `Int (List.length node_ids));
                ] in
                add_step t node_id input (Some output) (if errors <> [] then Some (String.concat ", " errors) else None) duration_ms;
                
                if errors <> [] then
                  Failure (String.concat ", " errors, context)
                else
                  Success (output, context)
              with exn ->
                Domainslib.Task.teardown_pool pool;
                let error = Printexc.to_string exn in
                add_step t node_id input None (Some error) 0;
                (Failure (error, context), t)
          
          | LLMNode _ ->
              (* LLM execution would go here - for now just return success *)
              add_step t node_id input None None 0;
              Success (`String "LLM execution not yet implemented", context)
          
          | EndNode ->
              add_step t node_id input None None 0;
              Success (`String "end", context)
        in
        (result, t)
  
  let rec execute t node_id input context tools ~chat_id =
    let (result, t') = execute_node t node_id input context tools ~chat_id in
    match result with
    | Success (_, ctx) ->
        let next_nodes = find_next_nodes t'.graph node_id ctx in
        begin match next_nodes with
          | [] -> (result, t')
          | [next] -> execute t' next input ctx tools ~chat_id
          | _ -> 
              (* Multiple next nodes - pick highest priority or first *)
              execute t' (List.hd next_nodes) input ctx tools ~chat_id
        end
    | Failure _ | Partial _ -> (result, t')
  
  let execute_from_start t input context tools ~chat_id =
    execute t t.graph.start_node input context tools ~chat_id
  
  let step t node_id input context tools ~chat_id =
    execute_node t node_id input context tools ~chat_id
  
  let get_current_node t = t.current_node
  let get_execution_trace t = t.trace
end

(* Helper functions for building execution graphs *)
let create_builder () = {
  nodes = [];
  edges = [];
  start_node = None;
  end_nodes = [];
  next_id = 0;
}

let gen_id builder =
  let id = Printf.sprintf "node_%d" builder.next_id in
  builder.next_id <- builder.next_id + 1;
  id

let add_node builder node_type ?description ?input_schema ?output_schema ?(retry_count=0) ?timeout_seconds node_id =
  let node = {
    node_id;
    node_type;
    description = Option.value ~default:"" description;
    input_schema;
    output_schema;
    retry_count;
    timeout_seconds;
  } in
  builder.nodes <- (node_id, node) :: builder.nodes;
  builder

let add_edge builder from_node to_node ?condition ?(priority=0) edge_id =
  let edge = {
    edge_id;
    from_node;
    to_node;
    condition;
    priority;
  } in
  builder.edges <- (edge_id, edge) :: builder.edges;
  builder

let set_start builder node_id =
  builder.start_node <- Some node_id;
  builder

let add_end builder node_id =
  builder.end_nodes <- node_id :: builder.end_nodes;
  builder

let build_graph builder graph_id =
  match builder.start_node with
  | None -> Error "Start node not set"
  | Some start ->
      Ok {
        graph_id;
        nodes = builder.nodes;
        edges = builder.edges;
        start_node = start;
        end_nodes = builder.end_nodes;
        metadata = [];
      }
