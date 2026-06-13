#!/usr/bin/env bash
# PreToolUse(Bash) guard: block any git command that would WRITE to `main`.
#
# The assistant works on feature branches and must never merge/commit/push to
# main (see meta/workflow.md). This hook is the durable backstop: it inspects the
# Bash command, and if it would write to main it returns a "deny" decision to
# Claude Code. It fails OPEN (allows) on any parse/other error so it can never
# wedge a session; following the rule is still the primary mechanism.
set -uo pipefail

payload="$(cat)"
branch="$(git -C "${CLAUDE_PROJECT_DIR:-$PWD}" symbolic-ref --short HEAD 2>/dev/null || true)"

BRANCH="$branch" PAYLOAD="$payload" python3 <<'PY'
import json, os, re, sys

try:
    data = json.loads(os.environ.get("PAYLOAD", ""))
except Exception:
    sys.exit(0)  # unparseable -> allow

cmd = ((data.get("tool_input") or {}).get("command") or "")
# Strip quoted strings first, so a commit/-m message that merely mentions "push"
# or "main" can't trip the guard (only the actual command words are inspected).
unquoted = re.sub(r'"[^"]*"', " ", cmd)
unquoted = re.sub(r"'[^']*'", " ", unquoted)
low = " ".join(unquoted.lower().split())
branch = os.environ.get("BRANCH", "")
on_main = branch in ("main", "master")

if "git" not in low:
    sys.exit(0)

def deny(reason: str) -> None:
    print(json.dumps({"hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": reason,
    }}))
    sys.exit(0)

WRITE = r"\b(merge|rebase|reset|commit|cherry-pick|am|push)\b"

# 1) a push that names main/master as a target ref
if re.search(r"\bgit\b[^;&|]*\bpush\b", low) and re.search(r"\b(main|master)\b", low):
    deny("Blocked by guard-main: pushing to main is not allowed. Push a feature "
         "branch and open a PR — merging to main is the user's job.")

# 2) any write op issued while currently ON main/master
if on_main and re.search(rf"\bgit\b[^;&|]*{WRITE}", low):
    deny(f"Blocked by guard-main: you are on '{branch}'. Work on a feature branch; "
         "writing to main (merge/commit/push/reset) is the user's job.")

# 3) switching to main and then writing in the same command line
if re.search(r"\b(checkout|switch)\b[^;&|]*\b(main|master)\b", low) and re.search(WRITE, low):
    deny("Blocked by guard-main: this switches to main and then writes to it. "
         "Do work on a feature branch; merging to main is the user's job.")

sys.exit(0)  # allow
PY
exit 0
