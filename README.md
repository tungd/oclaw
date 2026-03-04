# OCaml OClaw - Ultra-Lightweight Personal AI Assistant

## Overview

**OCaml OClaw** is a port of the Python nanobot project to OCaml, leveraging OCaml 5's native Domains capability for parallelism. This implementation uses `h1` + `iomux` for the HTTP server and curl/curl.multi + iomux for the HTTP client.

## Features

- **OCaml 5 Native Domains**: Full support for parallel execution using OCaml's native concurrency
- **Efficient HTTP Client**: Built on curl.multi and iomux for concurrent requests
- **HTTP Server Core**: Event-driven HTTP/1.1 server built on h1 + iomux
- **LLM Provider Integration**: OpenAI-compatible API support with DashScope Qwen3.5+ integration
- **Modular Tool System**: Web search, file operations, and command execution tools
- **Memory Management**: Conversation history with time-based decay and token-based limitations
- **Configuration System**: JSON-based configuration with automatic file creation and validation
- **Extensible Architecture**: Modular design for easy addition of new features

## Project Structure

```
oclaw/
├── dune                  # OCaml build system configuration
├── dune-project          # Project metadata
├── http_client.ml/mli    # HTTP client using curl.multi
├── http_server.ml        # HTTP server using h1 + iomux
├── Oclaw_server.ml       # API routes and server integration
├── llm_provider.ml/mli   # LLM provider module
├── memory.ml/mli         # Memory and conversation history
├── tools.ml/mli           # Tool system implementation
├── config.ml/mli         # YAML-based Configuration system
├── oclaw.ml              # Main agent implementation
├── test_*.ml             # Test suites
└── README.md              # This file
```

## Coding Conventions

### Type Organization

This project follows a **module-based type organization** pattern:

**Preferred:**
```ocaml
module HttpMethod = struct
  type t = GET | POST | PUT | DELETE
  
  let to_string = function
    | GET -> "GET"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
  
  let of_string = function
    | "GET" -> Some GET
    | "POST" -> Some POST
    | "PUT" -> Some PUT
    | "DELETE" -> Some DELETE
    | _ -> None
end
```

**Avoid:**
```ocaml
(* Old style - not preferred *)
type http_method = GET | POST | PUT | DELETE
```

### Module Structure

Each major component is organized as a separate module with:
- Clear type definitions in `.mli` files
- Implementation in `.ml` files
- Helper functions and utilities within the module
- Proper documentation for all public interfaces

### Error Handling

- Use `Result` types for functions that can fail
- Provide meaningful error messages
- Use exceptions sparingly (only for truly exceptional cases)

### Naming Conventions

- **Modules**: `PascalCase` (e.g., `HttpMethod`, `MemorySystem`)
- **Types**: `lowercase_with_underscores` (e.g., `type t = ...`)
- **Functions**: `lowercase_with_underscores` (e.g., `create_request`, `perform_request`)
- **Variables**: `lowercase_with_underscores` (e.g., `http_response`, `error_msg`)

## Key Components

### HTTP Client

The `http_client` module provides a robust HTTP client with:

```ocaml
module HttpMethod = struct
  type t = GET | POST | PUT | DELETE
  (* Conversion functions *)
end

module HttpRequest = struct
  type t = {
    method_ : HttpMethod.t;
    url : string;
    headers : (string * string) list;
    body : string option;
    timeout : int;
  }
  (* Helper functions *)
end

module HttpResponse = struct
  type t = {
    status : int;
    headers : (string * string) list;
    body : string;
    error : string option;
  }
  (* Helper functions *)
end
```

### LLM Provider

The `llm_provider` module handles communication with LLM APIs:

```ocaml
type llm_model = {
  id : string;
  name : string;
  provider : string;
  max_tokens : int;
  temperature : float;
}

type message_role = System | User | Assistant | Tool
```

### Memory System

The `memory` module manages conversation history with cleanup policies:

```ocaml
type cleanup_policy = {
  max_age_seconds : float;      (* Messages older than this may be pruned *)
  max_tokens : int;            (* Approximate token limit for context *)
  max_messages : int;          (* Hard limit on number of messages *)
  importance_threshold : float (* Messages with lower importance may be pruned *)
}

type conversation_message = {
  role : string;
  content : string;
  timestamp : float;
  importance : float;
  estimated_tokens : int;
  metadata : (string * string) list
}
```

### Tool System

The `tools` module provides extensible tool functionality:

```ocaml
type tool_definition = {
  name : string;
  description : string;
  parameters : (string * string) list; (* name, type *)
  handler : string list -> string;    (* function to execute tool *)
}
```

## Building and Running

### Prerequisites

- OCaml 5.4.0+
- opam package manager
- Required libraries: `curl`, `iomux`, `yojson`, `yaml`, `ppx_protocol_conv`, `ppx_protocol_conv_json`, `ppx_protocol_conv_yaml`

### Installation

```bash
# Install dependencies
opam install curl iomux yojson yaml ppx_protocol_conv ppx_protocol_conv_json ppx_protocol_conv_yaml

# Build the project
cd oclaw
dune build
```

### Running Tests

```bash
# Run HTTP client tests
dune exec ./test_http.exe

# Run memory system tests
dune exec ./test_memory.exe

# Run LLM provider tests
dune exec ./test_llm.exe
```

### Running the Agent

```bash
# Start interactive mode
dune exec ./oclaw.exe

# Single-shot mode
echo "What is the capital of France?" | dune exec ./oclaw.exe -- --single-shot
```

## Configuration

The agent uses a JSON configuration file (`config.json`):

```json
{
  "provider": {
    "name": "dashscope",
    "api_key": "your-api-key-here",
    "model": "qwen3.5-plus",
    "timeout": 30
  },
  "memory": {
    "max_age_seconds": 3600,
    "max_tokens": 4000,
    "max_messages": 50,
    "importance_threshold": 0.3
  },
  "tools": [
    {"name": "web_search", "enabled": true},
    {"name": "file_read", "enabled": true},
    {"name": "execute_command", "enabled": false}
  ]
}
```

## Parallel Execution

OCaml OClaw leverages OCaml 5's Domains for parallel tool execution:

```ocaml
(* Example: Parallel tool execution *)
let execute_tools_parallel tools inputs =
  let domain_count = Domain.recommended_domain_count () in
  let results = Array.make (List.length tools) None in
  
  let worker tool input idx =
    try
      let result = Tool.execute tool input in
      results.(idx) <- Some (Ok result)
    with exn ->
      results.(idx) <- Some (Error (Printexc.to_string exn))
  done
  
  let domains = List.mapi (fun i tool ->
    Domain.spawn (fun () -> worker tool (List.nth inputs i) i)
  ) tools in
  
  List.iter Domain.join domains;
  Array.to_list results
```

## Future Enhancements

- **Directory Listing Tool**: Add file system navigation capabilities
- **Enhanced Parallelism**: Optimize domain usage for better performance
- **Additional LLM Providers**: Support for more LLM APIs
- **Improved Memory**: Better token counting and context management
- **Plugin System**: Dynamic loading of tools and extensions

## Contributing

Contributions are welcome! Please follow the coding conventions and submit pull requests with:
- Clear commit messages
- Comprehensive tests
- Updated documentation
- Type-safe implementations

## License

This project is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgments

- Inspired by the original Python nanobot project (but named OClaw)
- Built with OCaml 5's advanced concurrency features
- Uses efficient system libraries for optimal performance
