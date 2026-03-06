---
name: Skill Ops
description: Inspect, copy, and maintain local skills using primitive filesystem and shell tools. Use when working with SKILL.md files or moving skills in and out of the repo.
version: 1.0.0
---
Manage skills with primitive tools only.

- Inspect installed skills with `list_directory` and `read_file`.
- Check both `skills/` and `workspace/skills/` when looking for local skill folders.
- Copy or install a skill with `execute_command` plus normal shell utilities such as `mkdir` and `cp -R`.
- Validate that a skill folder contains `SKILL.md` before copying it.
- Keep skill bodies concise and procedural; prefer small focused instructions over long explanations.

Typical pattern:
```sh
mkdir -p workspace/skills && cp -R skills/web-research workspace/skills/
```
