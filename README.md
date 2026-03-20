# OClaw

OClaw is a CLI-first AI assistant written in OCaml 5. It ships as a single executable with:

- An interactive TUI frontend
- An ACP JSON-RPC mode over stdio
- A tool-first agent loop with streaming text deltas
- Persistent transcript storage under a local workspace directory
- A local skills directory that is discovered and injected into the system prompt

The codebase is organized as a set of reusable libraries around a small `oclaw` entrypoint.

## Current Status

What is implemented today:

- TUI chat mode with streaming assistant output and status updates
- ACP mode for JSON-RPC clients
- Persistent conversations keyed by `chat_id`
- Tree-structured transcript storage with conversation export to HTML and JSON
- Built-in tools: `bash`, `read_file`, `write_file`, `edit_file`
- Local skill discovery from `workspace/skills/`
- OpenAI-compatible chat-completions provider integration
- Retry logic and tool error classification covered by tests

What is not here:

- Sandboxed tool execution
- A broad tool/plugin ecosystem beyond the four built-in tools
- A polished multi-client protocol layer beyond the current ACP implementation

## Project Layout

```text
oclaw/
├── bin/oclaw/          # CLI entrypoint
├── lib/
│   ├── acp/            # ACP message and stdio transport support
│   ├── agent_core/     # Runtime, transcript store, config, agent loop
│   ├── agent_skills/   # Local skill discovery and sync
│   ├── agent_tools/    # Built-in tool registry and execution
│   ├── agent_tui/      # Mosaic-based TUI frontend
│   ├── httpkit/        # Minimal HTTP client
│   ├── llm_provider/   # OpenAI-compatible provider + retry logic
│   └── llm_types/      # Shared message/tool types
└── test/               # Standalone executable test suites
```

## Requirements

- OCaml 5.0+
- Dune 3.15+

Main package dependencies are declared in [`dune-project`](/Users/tung/Projects/std23/oclaw/dune-project).

## Build

```bash
dune build
```

Run the executable:

```bash
dune exec ./bin/oclaw/oclaw.exe
```

## Usage

### Interactive TUI

The default mode launches the terminal UI.

```bash
dune exec ./bin/oclaw/oclaw.exe
```

Current TUI behavior:

- Streams assistant text as it arrives
- Shows simple status updates and a spinner while work is in progress
- Keeps in-memory UI state for the current run
- Exits on `Escape`

### Persistent Conversations

Persistence is opt-in. When enabled, OClaw resumes the latest node for the selected `chat_id`.

```bash
dune exec ./bin/oclaw/oclaw.exe -- --persistent --chat-id 42
```

### ACP Mode

Run OClaw as a stdio JSON-RPC service:

```bash
dune exec ./bin/oclaw/oclaw.exe -- --acp
```

The current ACP implementation supports:

- `initialize`
- `initialized`
- `agent/message`
- `agent/delta`
- `status`
- `done`

### Export a Conversation

Export a persisted conversation as static HTML:

```bash
dune exec ./bin/oclaw/oclaw.exe -- --export 42 -o transcript_42.html
```

## CLI Flags

The checked-in executable currently supports these flags:

- `--acp`
- `--persistent`
- `--chat-id <int>`
- `--config <path>`
- `--model <name>`
- `--api-key <key>`
- `--api-base <url>`
- `--data-dir <path>`
- `--max-tool-iterations <int>`
- `--debug`
- `--export <chat_id>`
- `-o <path>`

## Configuration

Configuration precedence is:

1. CLI overrides passed by the executable
2. Environment variables
3. YAML config file loaded with `--config`
4. Built-in defaults

### Environment Variables

- `OCLAW_API_KEY`
- `OCLAW_API_BASE`
- `OCLAW_MODEL`
- `OCLAW_DATA_DIR`
- `OCLAW_MAX_TOOL_ITERATIONS`
- `OCLAW_DEBUG`
- `OCLAW_API_RETRY_ENABLED`
- `OCLAW_API_RETRY_MAX_RETRIES`
- `OCLAW_API_RETRY_BASE_DELAY_MS`
- `OCLAW_API_RETRY_MAX_DELAY_MS`

### Example `config.yaml`

```yaml
llm_model: qwen3.5-plus
llm_api_key: sk-your-api-key
llm_api_base: https://coding-intl.dashscope.aliyuncs.com/v1
data_dir: workspace
max_tool_iterations: 256
debug: false
api_retry_enabled: true
api_retry_max_retries: 3
api_retry_base_delay_ms: 1000
api_retry_max_delay_ms: 30000
```

Notes:

- `llm_api_key` is required
- `max_tool_iterations` must be positive
- The default data directory is `workspace`

## Runtime Data

By default, OClaw writes state under `workspace/`:

- `workspace/runtime/` for transcript data
- `workspace/skills/` for local skills

The transcript layer supports:

- Multiple conversations
- Tree-structured nodes with parent/child relationships
- Forking conversations from an existing node
- Export to JSON and HTML

## Built-in Tools

The default registry contains four tools:

- `bash`
- `read_file`
- `write_file`
- `edit_file`

Tool execution behavior today:

- Tool calls are exposed to the model through JSON schema definitions
- Multiple tool calls in a single assistant turn can be executed in parallel via `domainslib`
- Tool failures are classified into structured categories with recovery hints
- Tool output is truncated before being sent back to the model, while full content remains in the transcript

Important limitation:

- The built-in tools are not sandboxed. `bash` executes shell commands directly in the host environment.

## Skills

Skills are discovered from `workspace/skills/<name>/SKILL.md`.

Current capabilities:

- Discover local skills and include a catalog in the system prompt
- Read a specific skill by name
- Sync a skill from a remote GitHub repo into the local skills directory

This is currently a local file-based workflow, not a full packaged plugin system.

## Architecture Notes

### Agent Loop

The core loop in [`lib/agent_core/agent_engine.ml`](/Users/tung/Projects/std23/oclaw/lib/agent_core/agent_engine.ml) does the following:

1. Store the user prompt in the transcript
2. Build the system prompt, including discovered skills
3. Call the LLM provider with message history and tool definitions
4. If the model requests tools, execute them and append tool results
5. Repeat until a final text response is produced or the tool iteration limit is reached

### Provider Layer

The provider code in [`lib/llm_provider/llm_provider.ml`](/Users/tung/Projects/std23/oclaw/lib/llm_provider/llm_provider.ml) is an OpenAI-compatible transport adapter over the shared `Llm_types` AST and supports:

- Standard request serialization
- Streaming response parsing
- Tool-call reconstruction from streaming deltas
- Configurable retry policy via the retry module

### TUI

The TUI in [`lib/agent_tui/tui.ml`](/Users/tung/Projects/std23/oclaw/lib/agent_tui/tui.ml) is built on Mosaic and communicates with the agent loop through channels.

## Tests

Run all tests:

```bash
dune runtest
```

Current test suites include:

- [`test/test_config.ml`](/Users/tung/Projects/std23/oclaw/test/test_config.ml)
- [`test/test_http_client.ml`](/Users/tung/Projects/std23/oclaw/test/test_http_client.ml)
- [`test/test_tools_registry.ml`](/Users/tung/Projects/std23/oclaw/test/test_tools_registry.ml)
- [`test/test_assistant_runtime.ml`](/Users/tung/Projects/std23/oclaw/test/test_assistant_runtime.ml)
- [`test/test_llm_provider.ml`](/Users/tung/Projects/std23/oclaw/test/test_llm_provider.ml)
- [`test/test_retry.ml`](/Users/tung/Projects/std23/oclaw/test/test_retry.ml)
- [`test/test_tool_errors.ml`](/Users/tung/Projects/std23/oclaw/test/test_tool_errors.ml)

## License

MIT
