# OClaw

OClaw is a CLI-first AI assistant written in OCaml 5. It ships as a single executable with:

- An interactive append-only REPL frontend with `layoutz`-based terminal rendering
- An ACP JSON-RPC mode over stdio
- A tool-first agent loop with streaming text deltas
- Persistent transcript storage under a local workspace directory
- A local skills directory that is discovered and injected into the system prompt

The codebase is organized as a set of reusable libraries around a small `oclaw` entrypoint.

The supported runtime surface is `agent_runtime`. Skill parsing and discovery remain in `agent_skills`, while tool execution, skill trust, and activation approvals live inside the runtime.

## Current Status

What is implemented today:

- Plain REPL chat mode with streaming assistant output and markdown-aware terminal rendering
- ACP mode for JSON-RPC clients
- Persistent conversations keyed by `chat_id`
- Tree-structured transcript storage with conversation export to HTML and JSON
- Built-in tools: `bash`, `read_file`, `write_file`, `edit_file`, `skill_list`, `skill_search`, `skill_install`
- Agent Skills discovery, trust gating, activation, and remote install/search support
- OpenAI-compatible chat-completions provider integration
- Token estimation with tiktoken-compatible BPE tokenizer
- Context usage tracking and warnings
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
│   ├── agent_runtime/  # Runtime, transcript store, config, agent loop, and tools
│   ├── agent_skills/   # Skill discovery, parsing, and install/search data
│   ├── agent_tui/      # Layoutz-backed append-only REPL frontend
│   ├── httpkit/        # Minimal HTTP client
│   ├── llm_provider/   # OpenAI-compatible provider + retry logic
│   └── llm_types/      # Shared message/tool types
└── test/               # Standalone executable test suites
```

## Requirements

- OCaml 5.0+
- Dune 3.15+

Main package dependencies are declared in [`dune-project`](dune-project).

## Build

```bash
dune build
```

Run the executable:

```bash
dune exec ./bin/oclaw/oclaw.exe
```

## Usage

### Interactive REPL

The default mode launches the append-only terminal REPL.

```bash
dune exec ./bin/oclaw/oclaw.exe
```

Current interactive behavior:

- Streams assistant text as it arrives
- Renders transcript output with `layoutz` while still appending directly to stdout
- Replays the current persistent branch on startup when `--persistent` is enabled
- Uses boxed numbered approval prompts for tool permissions
- Supports `/new` and `/fork` in the TUI REPL for creating or forking persistent conversations
- Supports `/permissions` and `/permissions <read|write|exec|install>` for reviewing approvals
- Exits with `/exit` or `/quit`

### Persistent Conversations

Persistence is opt-in. When enabled, OClaw resumes the latest node for the selected `chat_id`.
In TUI mode, `/new` creates a fresh conversation and `/fork` branches from the current latest node, then switches the active session to the new chat.

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

By default, OClaw writes project state under `<project>/.agents/`:

- `<project>/.agents/oclaw.db` for transcript data and approvals
- `<project>/.agents/skills/` for project-scoped skills
- `<project>/.agents/user/skills/` as a fallback user-scope skills directory when `~/.oclaw` is not writable

When writable, OClaw also uses `~/.oclaw/` for user-scoped skills and remote catalog cache.

The transcript layer supports:

- Multiple conversations
- Tree-structured nodes with parent/child relationships
- Forking conversations from an existing node
- Export to JSON and HTML

## Token Estimation

OClaw includes built-in token counting using a tiktoken-compatible BPE tokenizer. This allows tracking context usage and warning when approaching model limits.

Features:

- Automatic token counting before each LLM call
- Context usage tracking (system prompt + messages + tool definitions)
- Warnings at 80%, 90%, and 100% of context limit
- Support for multiple encodings:
  - `cl100k_base` (GPT-4, GPT-3.5-turbo)
  - `o200k_base` (GPT-4o, GPT-4o-mini)
- Automatic model-to-encoding mapping

Token usage is logged and displayed in status updates when approaching limits.

### Context Limits

Supported models and their context limits:

- GPT-4, GPT-4-32k: 8,192 / 32,768 tokens
- GPT-3.5-turbo, GPT-3.5-turbo-16k: 4,096 / 16,385 tokens
- GPT-4o, GPT-4o-mini: 128,000 tokens
- Qwen models: 32,768 tokens (default)

The token estimator automatically detects the model and applies the appropriate encoding and context limit.

## Built-in Tools

The default registry contains seven always-available tools:

- `bash`
- `read_file`
- `write_file`
- `edit_file`
- `skill_list`
- `skill_search`
- `skill_install`

When at least one trusted skill is available, OClaw also exposes `activate_skill`.

Tool execution behavior today:

- Tool calls are exposed to the model through JSON schema definitions
- Multiple tool calls in a single assistant turn can be executed in parallel via `domainslib`
- Tool failures are classified into structured categories with recovery hints
- Tool output is truncated before being sent back to the model, while full content remains in the transcript

Important limitation:

- The built-in tools are not sandboxed. `bash` executes shell commands directly in the host environment.

## Skills

OClaw follows the `agentskills.io` `SKILL.md` model with scoped discovery and explicit activation.

Discovery scopes:

- Built-in skills shipped with OClaw
- User skills from `~/.oclaw/skills/` when writable
- Project skills from `<project>/.agents/skills/`

Trust behavior:

- Project skills are hidden from the model until the project is trusted
- User and built-in skills are available without project trust
- Skills are disclosed progressively: only metadata goes into the system prompt, and full instructions are loaded via `activate_skill`
- Trust is enforced by `agent_runtime`, not by `agent_skills`

User and agent entrypoints:

- `skill_list` lists discoverable skills
- `skill_search` searches local and remote official catalog skills
- `skill_install` installs a remote skill after explicit approval
- `activate_skill` loads a trusted skill into the conversation
- Users can explicitly activate skills with `/skill <name>` or `$skill-name`

Approval behavior:

- `skill_install` requires `/approve install <skill-name>`
- `/permissions` lists current executable, read, write, and install approvals
- Skill `allowed-tools` entries currently pre-approve supported `Read`, `Write`, and `Bash(...)` entries during activation

## Architecture Notes

### Agent Loop

The public runtime entrypoint is exposed through `Agent_runtime.App`, `Agent_runtime.Session`, and `Agent_runtime.Export`.

Internally, the core loop in [`lib/agent_runtime/agent_engine.ml`](lib/agent_runtime/agent_engine.ml) does the following:

1. Store the user prompt in the transcript
2. Build the system prompt, including discovered skills
3. Call the LLM provider with message history and tool definitions
4. If the model requests tools, execute them and append tool results
5. Repeat until a final text response is produced or the tool iteration limit is reached

### Provider Layer

The provider code in [`lib/llm_provider/llm_provider.ml`](lib/llm_provider/llm_provider.ml) is an OpenAI-compatible transport adapter over the shared `Llm_types` AST and supports:

- Standard request serialization
- Streaming response parsing
- Tool-call reconstruction from streaming deltas
- Configurable retry policy via the retry module

### Interactive Frontend

The interactive frontend in [`lib/agent_tui/plain_repl.ml`](lib/agent_tui/plain_repl.ml) is a synchronous stdio REPL. It uses `layoutz` for the startup banner, approval blocks, and markdown-aware transcript rendering while still replaying persistent history on startup and appending output directly to stdout.

## Tests

Run all tests:

```bash
dune runtest
```

Current test suites include:

- [`test/test_config.ml`](test/test_config.ml)
- [`test/test_http_client.ml`](test/test_http_client.ml)
- [`test/test_tools_registry.ml`](test/test_tools_registry.ml)
- [`test/test_assistant_runtime.ml`](test/test_assistant_runtime.ml)
- [`test/test_llm_provider.ml`](test/test_llm_provider.ml)
- [`test/test_retry.ml`](test/test_retry.ml)
- [`test/test_tool_errors.ml`](test/test_tool_errors.ml)
- [`test/test_plain_repl.ml`](test/test_plain_repl.ml)

## License

MIT
