# OClaw

OClaw is a CLI-first OCaml assistant. The product surface is intentionally small:

- one binary: `oclaw`
- two runtime modes: REPL and single-shot
- one capability layer: primitive filesystem and shell tools
- one memory model: in-process session history for the current REPL only

ACP, Telegram, task orchestration, and server-side protocol adapters are no longer part of the project.

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

## Configuration

OClaw is flags-first. Runtime settings can come from:

1. CLI flags
2. environment variables
3. `config.yaml` if present
4. built-in defaults

The remaining configuration surface is limited to:

- model/provider settings
- API base and key
- tool sandbox settings
- debug logging

Useful environment variables:

- `OCLAW_API_KEY`
- `OCLAW_API_BASE`
- `OCLAW_MODEL`
- `OCLAW_TEMPERATURE`
- `OCLAW_MAX_TOKENS`
- `OCLAW_TIMEOUT`
- `OCLAW_WORKSPACE`
- `OCLAW_ALLOW_READ_PATHS`
- `OCLAW_ALLOW_WRITE_PATHS`
- `OCLAW_EXEC_TIMEOUT`
- `OCLAW_DEBUG`

Useful CLI flags:

- `--single-shot`
- `--config`
- `--model`
- `--api-key`
- `--api-base`
- `--temperature`
- `--max-tokens`
- `--timeout`
- `--workspace`
- `--allow-read-path`
- `--allow-write-path`
- `--no-workspace-restriction`
- `--exec-timeout`
- `--debug`

## Prompt Context

If present, OClaw loads these workspace files into the system prompt:

- `workspace/IDENTITY.md`
- `workspace/AGENTS.md`
- `workspace/SOUL.md`
- `workspace/USER.md`

This prompt context is read at startup. No skill system or persistent memory layer remains in the core product.

## Built-In Tools

The default tool registry is intentionally primitive:

- `read_file`
- `write_file`
- `edit_file`
- `append_file`
- `list_directory`
- `list_dir`
- `execute_command`
- `exec`

## Build and Test

```bash
dune build
dune exec ./test_tools_registry.exe
dune exec ./test_assistant_runtime.exe
dune exec ./test_config.exe
```
