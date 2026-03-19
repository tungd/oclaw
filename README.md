# OClaw

OClaw is a CLI-first OCaml AI assistant with a structured, tool-first runtime:

- **One binary**: `oclaw`
- **Three runtime modes**: TUI (default), single-shot, and ACP (Agent Client Protocol)
- **Agent loop**: Structured messages, tool calls, and persistent sessions via SQLite
- **OCaml 5 powered**: Parallel tool execution using domains
- **Extensible**: Skills system and adapter-ready runtime seams for future channels

## Project Structure

```
oclaw/
├── bin/oclaw/          # Main executable entry point
├── lib/
│   ├── acp/            # Agent Client Protocol types (standalone, reusable)
│   ├── agent_core/     # Core agent logic
│   │   ├── config.ml   # Configuration management
│   │   ├── db.ml       # SQLite persistence layer
│   │   ├── transcript.ml  # Tree-structured conversation storage
│   │   ├── memory.ml   # Memory management
│   │   ├── runtime.ml  # App state and LLM call interface
│   │   ├── agent_engine.ml  # Agent loop, tool execution
│   │   └── assistant_runtime.ml  # High-level assistant API
│   ├── agent_tools/    # Tool implementations (standalone)
│   ├── agent_skills/   # Skills system
│   ├── agent_tui/      # TUI frontend (Mosaic-based)
│   ├── httpkit/        # HTTP client library
│   └── llm_provider/   # LLM provider + types
└── test/               # Test suites
```

### Library Dependencies

```
agent_core → agent_tools, agent_skills, agent_db (embedded), llm_provider, httpkit
agent_tools → llm_types, httpkit (standalone, no core/skills/db deps)
agent_skills → (standalone)
agent_tui → agent_core, mosaic
llm_provider → llm_types, httpkit
httpkit → (standalone)
acp → (standalone protocol types)
```

## Usage

### Interactive TUI (default)

Run the interactive TUI with streaming responses:

```bash
dune exec ./bin/oclaw/oclaw.exe
```

The TUI features:
- Real-time streaming of assistant responses
- Braille spinner during thinking states
- Conversation history in the session
- Press `Escape` to quit

### Single-shot mode

Run a single prompt with a positional argument:

```bash
dune exec ./bin/oclaw/oclaw.exe -- "Summarize the files in this workspace"
```

Run a single prompt from stdin:

```bash
printf 'Explain the last command output' | dune exec ./bin/oclaw/oclaw.exe -- --single-shot
```

### Persistent conversations

Use a persistent conversation ID to maintain history across sessions:

```bash
dune exec ./bin/oclaw/oclaw.exe -- --chat-id 42 --persistent
```

### ACP mode (Agent Client Protocol)

Run in ACP JSON-RPC mode via stdio for integration with other tools:

```bash
dune exec ./bin/oclaw/oclaw.exe -- --acp
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
  - Message history persisted per conversation (tree-structured)
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

The `agent_tools` library is standalone and can be reused independently of the core agent.

## Skills

Skills are stored in `workspace/skills/` and can be activated via the `activate_skill` workflow. The system prompt automatically includes available skills from the skills directory.

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
# Run all tests
dune runtest

# Individual test suites are auto-discovered by dune
```

Test coverage:
- `test_config.ml` - Configuration loading and merging
- `test_http_client.ml` - HTTP client library
- `test_tools_registry.ml` - Tool registry and definitions
- `test_llm_provider.ml` - LLM provider request serialization
- `test_assistant_runtime.ml` - Full agent loop, persistence, tool execution

## Architecture

### Core Agent Loop (`lib/agent_core/agent_engine.ml`)

1. User prompt → stored in transcript DB
2. Build system prompt (includes skills catalog)
3. Call LLM with message history + tool definitions
4. If tool calls:
   - Execute tools in parallel (OCaml domains)
   - Store tool calls/results in transcript
   - Loop back to step 3
5. If text response → store and return

### Persistence (`lib/agent_core/transcript.ml`)

Tree-structured conversation storage with materialized paths:
- Each message is a node with a path (e.g., "1/2/3")
- Supports branching/forking conversations
- Tool calls and results are separate node types
- SQLite with indexes for fast path queries

### Tool Architecture (`lib/agent_tools/`)

Tools are standalone - no dependency on core agent, skills, or database:
- Each tool has a JSON schema definition
- Execution returns structured `tool_result`
- Registry pattern for tool discovery

## License

MIT
