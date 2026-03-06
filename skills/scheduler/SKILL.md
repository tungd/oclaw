---
name: Scheduler
description: Manage the workspace cron store directly with primitive tools. Use when adding, listing, enabling, disabling, or removing scheduled jobs without a built-in cron tool.
version: 1.0.0
---
The scheduler state lives in `workspace/cron/jobs.json`.

- Read and write that JSON file directly with `read_file`, `write_file`, or one-shot `python3`.
- Preserve the current schema for each job: `id`, `task`, `at_seconds`, `every_seconds`, `cron_expr`, `enabled`, `created_at`.
- Use exactly one schedule field when creating a job: `at_seconds`, `every_seconds`, or `cron_expr`.
- Keep IDs stable when editing existing jobs.
- Use Python for non-trivial JSON updates so formatting and escaping stay correct.

Typical pattern:
```sh
python3 <<'PY'
import json, time
from pathlib import Path

path = Path("workspace/cron/jobs.json")
jobs = json.loads(path.read_text(encoding="utf-8")) if path.exists() else []
jobs.append({
    "id": f"job-{int(time.time())}",
    "task": "Example task",
    "at_seconds": 300,
    "every_seconds": None,
    "cron_expr": None,
    "enabled": True,
    "created_at": time.time(),
})
path.parent.mkdir(parents=True, exist_ok=True)
path.write_text(json.dumps(jobs, indent=2), encoding="utf-8")
PY
```
