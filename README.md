# OClaw

OClaw is a CLI-first OCaml AI assistant with a structured, tool-first runtime:

- **One binary**: `oclaw`
- **Three runtime modes**: TUI (default), single-shot, and ACP (Agent Client Protocol)
- **Agent loop**: Structured messages, tool calls, and persistent sessions via SQLite
- **OCaml 5 powered**: Parallel tool execution using domains
- **Extensible**: Skills system and adapter-ready runtime seams for future channels

External channels, schedulers, and MCP are not shipped in this repo, but the runtime is structured so they can be added without architectural rewrites.

## Usage

### Interactive TUI (default)

Run the interactive TUI with streaming responses:

```bash
dune exec ./oclaw.exe
```

The TUI features:
- Real-time streaming of assistant responses
- Braille spinner during thinking states
- Conversation history in the session
- Press `Escape` to quit

### Single-shot mode

Run a single prompt with a positional argument:

```bash
dune exec ./oclaw.exe -- "Summarize the files in this workspace"
```

Run a single prompt from stdin:

```bash
printf 'Explain the last command output' | dune exec ./oclaw.exe -- --single-shot
```

### Persistent conversations

Use a persistent conversation ID to maintain history across sessions:

```bash
dune exec ./oclaw.exe -- --chat-id 42 --persistent
```

### ACP mode (Agent Client Protocol)

Run in ACP JSON-RPC mode via stdio for integration with other tools:

```bash
dune exec ./oclaw.exe -- --acp
```

## Configuration

OClaw is flags-first. Runtime settings can come from:

1. CLI flags (highest priority)
2. Environment variables
3. `config.yaml` if present
4. Built-in defaults

### Configuration file

Example `config.yaml`:

```yaml
llm_model: glm-5
llm_api_key: sk-your-api-key
llm_api_base: https://api.example.com/v1
data_dir: workspace
max_tool_iterations: 256
debug: false
```

### Environment variables

- `OCLAW_API_KEY` - LLM API key
- `OCLAW_API_BASE` - LLM API base URL
- `OCLAW_MODEL` - Model name to use
- `OCLAW_DATA_DIR` - Runtime data directory
- `OCLAW_DEBUG` - Enable debug logging

### CLI flags

- `--single-shot` - Run one prompt and exit
- `--persistent` - Enable persistent chat history
- `--chat-id <int>` - Use this persistent chat/session ID (default: 1)
- `--config <path>` - Load configuration from YAML file
- `--acp` - Run in ACP JSON-RPC mode via stdio
- `--model <name>` - Override the model name
- `--api-key <key>` - Override the API key
- `--api-base <url>` - Override the API base URL
- `--data-dir <path>` - Set the runtime data root
- `--max-tool-iterations <int>` - Set max tool iterations per request
- `--debug` - Enable debug logging

## Runtime State

Runtime state lives under `workspace/` by default:

- **SQLite DB**: `workspace/runtime/oclaw.db`
  - Resumable sessions stored by `chat_id`
  - Message history persisted per conversation
- **Skills**: `workspace/skills/`

## Built-In Tools

The default tool registry is CLI-focused with structured input/output:

| Tool | Description |
|------|-------------|
| `bash` | Execute a shell command |
| `read_file` | Read and return the contents of a file |
| `write_file` | Write content to a file (atomic writes) |
| `edit_file` | Edit a file by replacing one exact text block |

Tools execute in parallel using OCaml 5 domains when multiple tool calls are made in a single response.

## Skills

Skills are stored in `workspace/skills/` and can be activated via the `activate_skill` workflow. The system prompt automatically includes available skills from the skills directory.

Available skill packs:
- `git-expert` - Git operations and workflows
- `ocaml-pro` - OCaml development assistance
- `python-cli` - Python CLI development
- `scheduler` - Task scheduling workflows
- `skill-ops` - Skill management operations
- `web-research` - Web research workflows

## Build and Test

### Requirements

- OCaml 5.0+
- Dune 3.15+

### Build

```bash
dune build
```

### Run tests

```bash
# Run individual test suites
dune exec ./test_config.exe
dune exec ./test_http_client.exe
dune exec ./test_tools_registry.exe
dune exec ./test_llm_provider.exe
dune exec ./test_schedule_spec.exe
dune exec ./test_assistant_runtime.exe
```

## Architecture

```
oclaw.ml          # Main entry point, CLI parsing
├── runtime.ml    # App state creation and LLM call interface
├── agent_engine.ml  # Agent loop, tool execution, message management
├── tools.ml      # Tool registry and implementations
├── llm_provider.ml  # LLM API communication
├── db.ml         # SQLite persistence layer
├── skills.ml     # Skills system
├── tui.ml        # TUI using Mosaic library
└── lib/acp/      # Agent Client Protocol implementation
```

## License

MIT
