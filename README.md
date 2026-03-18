# OClaw

OClaw is a CLI-first OCaml assistant with a structured, tool-first runtime:

- one binary: `oclaw`
- two runtime modes: REPL and single-shot
- one agent loop: structured messages, tool calls, persistent sessions, and file-backed memory
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

- `OCLAW_API_KEY`
- `OCLAW_API_BASE`
- `OCLAW_MODEL`
- `OCLAW_TIMEOUT`
- `OCLAW_DATA_DIR`
- `OCLAW_SKILLS_DIR`
- `OCLAW_EMBEDDING_MODEL_DIR`
- `OCLAW_MAX_HISTORY_MESSAGES`
- `OCLAW_MAX_TOOL_ITERATIONS`
- `OCLAW_WORKSPACE`
- `OCLAW_ALLOW_READ_PATHS`
- `OCLAW_ALLOW_WRITE_PATHS`
- `OCLAW_EXEC_TIMEOUT`
- `OCLAW_WEB_REQUEST_TIMEOUT`
- `OCLAW_WEB_FETCH_MAX_BYTES`
- `OCLAW_WEB_SEARCH_MAX_RESULTS`
- `OCLAW_SCHEDULER_ENABLED`
- `OCLAW_SCHEDULER_POLL_INTERVAL`
- `OCLAW_DEBUG`

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

## Prompt and Memory Layout

OClaw builds its system prompt from:

- `workspace/SOUL.md`
- `workspace/IDENTITY.md`
- `workspace/USER.md`

Persistent memory is file-backed:

- global memory: `workspace/AGENTS.md`
- chat memory: `workspace/runtime/groups/<chat_id>/AGENTS.md`

Runtime state lives under `workspace/runtime/` by default:

- SQLite DB: `workspace/runtime/oclaw.db`
- resumable sessions: stored in SQLite by `chat_id`
- todo lists: stored in SQLite by `chat_id`
- scheduled tasks and run history: stored in SQLite by `chat_id`

Local skills live under `workspace/skills/`.

## Local Embeddings

Vector search now uses a local model bundle instead of the placeholder hash embedder. Point OClaw at a bundle with:

- `model.onnx`
- `vocab.txt` or `tokenizer.json`
- optional `tokenizer_config.json` / `config.json`

Set the model bundle path with:

```bash
export OCLAW_EMBEDDING_MODEL_DIR=/absolute/path/to/bge-small-en-v1.5
```

The current local backend loads tokenizer assets plus the ONNX embedding initializers and produces normalized sentence vectors for SQLite-backed similarity search.

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
- `read_memory`
- `write_memory`
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
