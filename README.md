# OClaw

OClaw is a CLI-first OCaml assistant with a rayclaw-style internal runtime:

- one binary: `oclaw`
- two runtime modes: REPL and single-shot
- one agent loop: structured messages, tool calls, persistent sessions, and file-backed memory
- one default deployment shape: local CLI, with adapter-ready runtime seams for future channels

External channels, schedulers, ACP/MCP, and embeddings are not shipped in this repo, but the runtime is structured so they can be added without another architectural rewrite.

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
- debug logging

Useful environment variables:

- `OCLAW_API_KEY`
- `OCLAW_API_BASE`
- `OCLAW_MODEL`
- `OCLAW_TEMPERATURE`
- `OCLAW_MAX_TOKENS`
- `OCLAW_TIMEOUT`
- `OCLAW_DATA_DIR`
- `OCLAW_SKILLS_DIR`
- `OCLAW_MAX_HISTORY_MESSAGES`
- `OCLAW_MAX_TOOL_ITERATIONS`
- `OCLAW_WORKSPACE`
- `OCLAW_ALLOW_READ_PATHS`
- `OCLAW_ALLOW_WRITE_PATHS`
- `OCLAW_EXEC_TIMEOUT`
- `OCLAW_DEBUG`

Useful CLI flags:

- `--single-shot`
- `--chat-id`
- `--config`
- `--model`
- `--api-key`
- `--api-base`
- `--temperature`
- `--max-tokens`
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

Local skills live under `workspace/skills/`.

## Built-In Tools

The default registry is rayclaw-style and CLI-focused:

- `bash`
- `read_file`
- `write_file`
- `edit_file`
- `glob`
- `grep`
- `read_memory`
- `write_memory`
- `todo_read`
- `todo_write`
- `activate_skill`
- `sync_skills`
- `export_chat`

## Build and Test

```bash
dune build
_build/default/test_tools_registry.exe
dune exec ./test_assistant_runtime.exe
_build/default/test_llm_provider.exe
_build/default/test_config.exe
```
