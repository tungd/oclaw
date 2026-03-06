---
name: Web Research
description: Research the web using primitive tools only. Use when the task needs browsing, page fetching, or lightweight search workflows without built-in web tools.
version: 1.0.0
---
Use `execute_command` with one-shot `python3` for web work.

- Do not rely on built-in web tools.
- Do not use `curl` or `wget`; they are blocked by the shell guard.
- Prefer Python stdlib: `urllib.request`, `urllib.parse`, `html.parser`, `json`, `re`.
- Keep each command self-contained. Save intermediate HTML, text, or JSON into workspace files when the task spans multiple steps.
- For search-like workflows, fetch a known search/result page with Python, extract candidate links, then fetch the selected pages in separate steps.
- Summarize fetched content in the final answer instead of dumping raw HTML.

Typical pattern:
```sh
python3 <<'PY'
from urllib.request import Request, urlopen

req = Request("https://example.com", headers={"User-Agent": "Mozilla/5.0"})
with urlopen(req, timeout=20) as resp:
    html = resp.read().decode("utf-8", errors="replace")
print(html[:4000])
PY
```
