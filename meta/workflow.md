# Workflow & assistant autonomy

## Git workflow — branches only; never merge to `main`

- **All work happens on a feature branch** (e.g. `feature/osc-tax-levies`). The
  assistant creates the branch, commits to it freely, and may push the *feature
  branch* to `origin`.
- **The assistant must never write to `main`:** no `git merge`, `git rebase`,
  `git reset`, `git commit`, or `git push` that targets `main`. **Merging a branch
  into `main` is the user's job** (typically via a reviewed PR).
- One unit of work = one branch. Open a PR for the user to review and merge.

## Assistant authority

The user has granted the assistant authority to **do everything except merge to
`main`**. Concretely:

- **Permission mode** is `acceptEdits` (set in `.claude/settings.local.json`): file
  edits/creates and an allow-listed set of routine Bash (git status/add/commit/
  branch/checkout/diff/log, `.venv/bin/python`, `curl`, etc.) run without
  prompting.
- **A guardrail hook** ([`.claude/hooks/guard-main.sh`](../.claude/hooks/guard-main.sh),
  registered as a `PreToolUse` hook on `Bash`) **blocks any git command that would
  write to `main`** — merge/commit/rebase/reset while on `main`, or any push that
  targets `main`. This enforces the rule above even if a command slips through; it
  is the durable backstop, not a substitute for following the rule.
- This is local config. After the hook/settings are first added (or changed),
  Claude Code may need a session restart to load them, and may show a one-time
  "hooks modified" review — that confirmation is expected.

## Practical notes

- Commit/push when it makes sense for branch hygiene; the "never touch `main`" rule
  is the only hard line. Outward-facing actions (publishing, posting) still get a
  confirmation first.
- Keep `meta/` and the relevant `SOURCE.md` updated **in the same change** as the
  code/data they describe.
