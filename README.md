# OClaw

OClaw is a CLI-first OCaml assistant with a structured, tool-first runtime:

- one binary: `oclaw`
- two runtime modes: REPL and single-shot
- one agent loop: structured messages, tool calls, and persistent sessions
- one default deployment shape: local CLI, with adapter-ready runtime seams for future channels

External channels, schedulers, and ACP/MCP are not shipped in this repo, but the runtime is structured so they can be added without another architectural rewrite.

## Usage

Run the interactive REPL:

```bash
dune exec ./oclaw.exe
```

Run a single prompt with a positional argument:

```bash
dune exec ./oclaw.exe -- "Summarize the files in this workspace"
```

Run a single prompt from stdin:

```bash
printf 'Explain the last command output' | dune exec ./oclaw.exe -- --single-shot
```

Use a persistent conversation id:

```bash
dune exec ./oclaw.exe -- --chat-id 42
```

## Configuration

OClaw is flags-first. Runtime settings can come from:

1. CLI flags
2. environment variables
3. `config.yaml` if present
4. built-in defaults

The remaining configuration surface includes:

- model/provider settings
- runtime data directories
- session/history limits
- tool sandbox settings
- web tool limits
- scheduler polling
- debug logging

Useful environment variables:

- `OCLAW_API_KEY` - LLM API key
- `OCLAW_API_BASE` - LLM API base URL
- `OCLAW_MODEL` - Model name to use
- `OCLAW_TIMEOUT` - HTTP timeout in seconds
- `OCLAW_DATA_DIR` - Runtime data directory
- `OCLAW_SKILLS_DIR` - Skills directory
- `OCLAW_MAX_HISTORY_MESSAGES` - Max messages in history
- `OCLAW_MAX_TOOL_ITERATIONS` - Max tool iterations per request
- `OCLAW_WORKSPACE` - Workspace root for tools
- `OCLAW_ALLOW_READ_PATHS` - Colon-separated paths allowed for reads
- `OCLAW_ALLOW_WRITE_PATHS` - Colon-separated paths allowed for writes
- `OCLAW_EXEC_TIMEOUT` - Shell command timeout in seconds
- `OCLAW_WEB_REQUEST_TIMEOUT` - Web request timeout
- `OCLAW_WEB_FETCH_MAX_BYTES` - Max bytes to fetch from web
- `OCLAW_WEB_SEARCH_MAX_RESULTS` - Max web search results
- `OCLAW_SCHEDULER_ENABLED` - Enable/disable scheduler
- `OCLAW_SCHEDULER_POLL_INTERVAL` - Scheduler poll interval in seconds
- `OCLAW_DEBUG` - Enable debug logging

Useful CLI flags:

- `--single-shot`
- `--chat-id`
- `--config`
- `--model`
- `--api-key`
- `--api-base`
- `--timeout`
- `--data-dir`
- `--skills-dir`
- `--workspace`
- `--allow-read-path`
- `--allow-write-path`
- `--no-workspace-restriction`
- `--exec-timeout`
- `--debug`

## Runtime State

Runtime state lives under `workspace/runtime/` by default:

- SQLite DB: `workspace/runtime/oclaw.db`
- Resumable sessions: stored in SQLite by `chat_id`
- Todo lists: stored in SQLite by `chat_id`
- Scheduled tasks and run history: stored in SQLite by `chat_id`

Local skills live under `workspace/skills/`.

## Built-In Tools

The default tool registry is CLI-focused with structured input/output:

- `bash`
- `read_file`
- `write_file`
- `edit_file`
- `glob`
- `grep`
- `web_search`
- `web_fetch`
- `todo_read`
- `todo_write`
- `activate_skill`
- `sync_skills`
- `schedule_task`
- `list_scheduled_tasks`
- `pause_scheduled_task`
- `resume_scheduled_task`
- `cancel_scheduled_task`
- `get_task_history`
- `export_chat`

## Build and Test

```bash
dune build

# Run individual test suites
dune exec ./test_config.exe
dune exec ./test_http_client.exe
dune exec ./test_tools_registry.exe
dune exec ./test_llm_provider.exe
dune exec ./test_schedule_spec.exe
dune exec ./test_assistant_runtime.exe
```
