---
name: Python CLI
description: Run one-shot Python automation through the shell tool. Use when a task needs Python but does not require built-in persistent Python sessions.
version: 1.0.0
---
Use `execute_command` with `python3` for Python automation.

- Treat every invocation as stateless.
- Persist state explicitly in workspace files if later steps need it.
- Keep scripts small and task-specific; prefer here-doc execution for clarity.
- Use `read_file` and `write_file` when file edits are simpler than embedding large strings in shell commands.
- Prefer stdlib first. Only assume third-party packages if the repo or environment already proves they exist.

Typical pattern:
```sh
python3 <<'PY'
import json
from pathlib import Path

path = Path("workspace/tmp/result.json")
path.parent.mkdir(parents=True, exist_ok=True)
path.write_text(json.dumps({"ok": True}), encoding="utf-8")
print(path)
PY
```
