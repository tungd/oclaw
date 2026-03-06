# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OClaw is an OCaml 5 port of the Python nanobot project - an ultra-lightweight personal AI assistant that leverages OCaml's native Domains for parallelism. It uses curl.multi and iomux for efficient concurrent HTTP requests, with OpenAI-compatible LLM API support (DashScope Qwen3.5+).

## Build and Test Commands

```bash
# Build the project
dune build

# Run the main agent executable (interactive mode)
dune exec ./oclaw.exe

# Run in single-shot mode (read from stdin, output response, exit)
echo "Your question here" | dune exec ./oclaw.exe -- --single-shot

# Run specific test suites
dune exec ./test_http.exe       # HTTP client tests
dune exec ./test_memory.exe     # Memory system tests
dune exec ./test_llm.exe        # LLM provider tests

# Clean build artifacts
dune clean
```

## Architecture

### Module Structure

The project is organized as separate libraries (defined in `dune`):

- **http_client** - HTTP client using curl.multi and iomux for concurrent requests
- **llm_provider** - OpenAI-compatible LLM API integration
- **memory** - Conversation history with time-based decay and token limits
- **tools** - Primitive runtime tool system (filesystem, shell, tasks, subagents)
- **skills** - SKILL.md file loading and prompt injection
- **oclaw_config** - YAML-based configuration system

### Main Entry Point

`oclaw.ml` is the main executable that:
1. Loads configuration from `config.yaml` (creates default if missing)
2. Initializes the tools system via `Tools.init_default_tools ()`
3. Creates LLM provider config from `Config.to_llm_provider_config`
4. Runs an interactive agent loop via `agent_loop`

### Module Aliases

The main module uses convenient aliases:
```ocaml
module Http = Http_client
module LLM = Llm_provider
module Tools = Tools
module Memory = Memory
module Config = Oclaw_config.Config
```

## Coding Conventions

### Module-Based Type Organization

**This is the most important convention** - types are organized within nested modules, not as standalone types:

```ocaml
(* Preferred - module-based *)
module HttpMethod = struct
  type t = GET | POST | PUT | DELETE
  let to_string = function ...
  let of_string = function ...
end

module HttpRequest = struct
  type t = { method_ : HttpMethod.t; url : string; ... }
  let create = fun ~method_ ~url ?headers ?body ?timeout () -> ...
end

(* Avoid - standalone types *)
type http_method = GET | POST  (* Don't do this *)
```

### Naming Conventions

- **Modules**: `PascalCase` (e.g., `HttpMethod`, `HttpRequest`, `HttpResponse`)
- **Types within modules**: `lowercase_with_underscores` (e.g., `type t = ...`)
- **Functions**: `lowercase_with_underscores` (e.g., `create_request`, `perform_request`)
- **Variables**: `lowercase_with_underscores`

### Error Handling

- Use `Result` types (`Ok value | Error error`) for functions that can fail
- Provide meaningful error messages
- Use exceptions sparingly (only for truly exceptional cases)

### Module Signatures

Public interfaces are defined in `.mli` files with:
- Clear type definitions
- Helper functions and utilities within modules
- Proper documentation

## Configuration

The agent loads configuration from `config.yaml`. If missing, it creates a default config with:
- Provider: DashScope (Alibaba Cloud)
- Model: qwen3.5-plus
- API base: `https://coding-intl.dashscope.aliyuncs.com/v1`

Key config fields:
- `llm_api_key` - API key for LLM provider
- `llm_timeout` - Request timeout in seconds
- `tools_max_concurrent` - Max parallel tool executions
- `agent_memory_window` - Conversation history size
- `debug` - Enable verbose logging

## Skills System

Skills are loaded from `skills/*/SKILL.md` files with YAML front matter:

```yaml
---
name: OCaml Pro
description: Specialist in OCaml development
version: 1.1.0
---
Skill instructions go here...
```

The `skills.ml` module parses these and injects them into the system prompt via `skills_to_prompt`.

## Workspace Prompt System

OClaw uses a file-based prompt system (mirroring picoclaw) that loads markdown files from `workspace/`:

- **IDENTITY.md** - Agent name, description, purpose, capabilities
- **AGENTS.md** - Agent instructions, OCaml-specific guidelines, project knowledge
- **SOUL.md** - Personality, values, communication style
- **USER.md** - User preferences and information (template for customization)

These files are loaded at startup and concatenated with skills to build the complete system prompt. This allows easy customization without code changes.

## Tool Calling System

OClaw implements OpenAI-compatible function calling:

1. Tools are registered via `Tools.init_default_tools()` and defined in `tools.ml`
2. Tools are converted to OpenAI JSON Schema format via `Tools.tools_to_json()`
3. Tools are passed to LLM via `LLM.call_llm ~tools:(Some tools_json)`
4. When LLM responds with `tool_calls`, they are executed via `Tools.execute_tool()`
5. Tool results are sent back to LLM as messages with `role = Tool`
6. Process repeats until LLM responds with a text message

Default runtime tools:
- `read_file` / `write_file` / `edit_file` / `append_file`
- `list_directory` / `list_dir`
- `execute_command` / `exec`
- task and subagent tools remain built-in

Higher-level web research, Python automation, skill operations, and scheduler workflows are skill-guided and should be done through the primitive tools rather than dedicated built-in tools.

Tool parameters use format: `"name", `String "type: description"` (e.g., `"path", `String "string: The file path"`)

## Key Module Dependencies

- `http_client` → curl, iomux, yojson
- `llm_provider` → http_client, curl, iomux, yojson
- `tools` → http_client, curl, iomux, yojson
- `memory` → yojson, unix
- `skills` → yojson, unix
- `oclaw_config` → yaml, yojson, llm_provider
- `oclaw` → all of the above

## Parallel Execution

The project is designed for OCaml 5's Domains for parallelism. The `call_llm_api` function integrates with the memory system to build conversation context, and tools can be executed in parallel using `Domain.spawn`.
