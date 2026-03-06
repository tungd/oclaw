# OClaw Architecture with Effect Handlers

## Core Design Principles

1. **Effect Handlers** for all async/external operations
2. **Yojson deriving** for type-safe JSON serialization
3. **No manual JSON construction** - use `[@@deriving yojson]`
4. **Simple HTTP server** using Unix sockets without complex schedulers

## Effect Handlers

### Core Effects

```ocaml
(* LLM Request Effect *)
type llm_request = {
  messages : Llm_message.t list;
  tools : Tool_definition.t list option;
  model : Model.t option;
}
effect llm_request : llm_request -> Llm_response.t

(* Tool Call Effect *)
type tool_call = {
  name : string;
  arguments : string;  (* JSON string *)
}
effect tool_call : tool_call -> string

(* Agent Wait Effect - for waiting/polling *)
effect agent_wait : float -> unit  (* wait for N seconds *)

(* Agent Load Skill Effect *)
effect load_skill : string -> Skill.t option

(* RPC Request Effect - for client-server communication *)
type rpc_request = {
  method_ : string;
  params : string;  (* JSON string *)
}
effect rpc_request : rpc_request -> string
```

## Type Definitions with Yojson Deriving

```ocaml
(* All types use @@deriving yojson *)

type message_role = System | User | Assistant | Tool
[@@deriving yojson { strict = false }]

type llm_message = {
  role : message_role;
  content : string;
  tool_calls : tool_call list option;
  tool_call_id : string option;
}
[@@deriving yojson { strict = false }]

type tool_call = {
  id : string;
  type_ : string;  (* "function" *)
  function_name : string;
  function_args : string;
}
[@@deriving yojson { strict = false }]

type model = {
  id : string;
  name : string;
  context_window : int;
}
[@@deriving yojson]

type tool_definition = {
  type_ : string;  (* "function" *)
  function_ : function_def;
}
[@@deriving yojson { strict = false }]

and function_def = {
  name : string;
  description : string;
  parameters : string;  (* JSON schema string *)
}
[@@deriving yojson { strict = false }]
```

## HTTP Server

Production path uses `h1` + `iomux`:

```ocaml
(* Event-driven HTTP server *)
(* - h1 handles HTTP/1.1 parsing and serialization *)
(* - iomux poll loop drives read/write readiness *)
(* - route handlers receive H1.Reqd.t *)
(* - blocking route work can be offloaded to worker domains *)
```

## Module Structure

```
lib/
  effects/          - Effect definitions and handlers
    llm.mli         - LLM request effect
    tools.mli       - Tool call effect
    agent.mli       - Agent effects (wait, load_skill)
    rpc.mli         - RPC effect for client-server

  models/           - Type definitions with yojson deriving
    llm.ml          - LLM types
    tool.ml         - Tool types
    session.ml      - Session types
    rpc.ml          - RPC types

  handlers/         - Effect handler implementations
    llm_handler.ml  - HTTP-based LLM handler
    tool_handler.ml - Tool execution handler
    rpc_handler.ml  - RPC client/server handler

  server/           - HTTP server and API
    http_server.ml  - h1 + iomux server runtime
    Oclaw_server.ml - API endpoints and route registration

  client/           - Client implementations
    tui.ml          - TUI client using RPC effect
    telegram.ml     - Telegram client using RPC effect

  oclaw.ml          - Main entry point with effect handlers
```

## Task API and Storage

OClaw now includes a Beads-inspired task layer for durable agent/subagent coordination:

- **SQLite-backed storage** (`task_store.ml`)
  - Tables: `tasks`, `task_dependencies`, `task_events`
  - WAL mode + busy timeout
  - Transactional write path with event append on every mutation
- **Task service** (`task_service.ml`)
  - Lifecycle rules and status transitions
  - Ready-task query with dependency blocking semantics
  - CRUD + claim/close/cancel + dependency/event operations
- **Task API client** (`task_api_client.ml`)
  - Internal tools call the central server via HTTP JSON endpoints
- **Server endpoints** (`Oclaw_server.ml`)
  - `POST /api/tasks`
  - `GET /api/tasks`
  - `GET /api/tasks/ready`
  - `GET /api/tasks/:id`
  - `POST /api/tasks/:id/update`
  - `POST /api/tasks/:id/claim`
  - `POST /api/tasks/:id/close`
  - `POST /api/tasks/:id/cancel`
  - `POST /api/tasks/:id/dependencies`
  - `GET /api/tasks/:id/dependencies`
  - `GET /api/tasks/:id/events`

## Example Usage

```ocaml
(* Agent code using effects *)
let process_query (query : string) =
  (* Load skills *)
  let skills = effload_skill "coding" in

  (* Call LLM *)
  let response = llm_request {
    messages = [{ role = User; content = query }];
    tools = Some default_tools;
    model = None;
  } in

  (* Handle tool calls *)
  match response.tool_calls with
  | Some calls ->
      List.map (fun call ->
        let result = efftool_call call in
        (* Continue with tool results... *)
      ) calls
  | None -> response.content
```

## Build Changes

Update dune-project:
```ocaml
(package
 (name oclaw)
 (synopsis "OCaml 5 AI assistant with effect handlers")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.15))
  (yojson (>= 2.0))
  (lwt (>= 5.7))  (* For Lwt-based HTTP *)
  (dream (>= 1.0)) (* Optional: for production HTTP server *)
  (logs)))
```
