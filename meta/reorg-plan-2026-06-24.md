# Book reorganization — status, plan, and open questions (2026-06-24)

> **Purpose of this file.** Don asked Claude to read the `meta/` docs and the book
> code and report status before reorganizing the `ccs` book. This file captures
> that assessment, a proposed plan, and the decisions/outline still needed, so a
> **fresh session** can pick up the work. Nothing has been changed yet — Don wants
> to grant more permissions and supply a front-part outline first.
>
> **For the next session:** start by reading `meta/` (README, conventions,
> workflow, decisions) per the standing rule, then read this file, then ask Don
> for his answers to the "Decisions needed" and "Outline needed" sections below
> before touching anything.

---

## The goal Don stated

The prime output is a **Quarto book on the web (GitHub Pages)** for an **area
audience deeply interested in Cambridge Central School District (CCS) and its
troubles** — non-economists, non-quantitative, but receptive to clear tables,
graphs, and analysis, and willing to read only short-to-mid pieces at a time.

Don wants to **reorganize the in-progress book into two parts**:

1. **A front part** — well-written, understandable, polished *parts and chapters*
   that will **evolve over time**. This is the public face.
2. **A back (exploratory) part** — the existing, half-formed, exploratory
   analyses, used to **inform** the front part. Audience = **Don himself**, plus
   readers who want to dig deeper. May also include a **more formal appendix**
   intended for readers.

Don's stated concern: Claude wrote some interpretive material headed for the back
part, but **the interpretation generally wasn't good**. So interpretation in the
front part needs to be done carefully, with Don closely involved.

Don's proposed sequence (from his own words):

1. Claude reads meta docs + code, reports status. *(done — this file)*
2. Make a plan.
3. Create **stubs for the front part**.
4. **Move existing book matter to an exploratory part / appendix.**
5. Don prepares a **rough outline of the front part** for discussion.
6. Start working the front part — drawing from the exploratory part, possibly
   creating more exploratory chapters, and fleshing out the front part.

He said **don't start yet** — wants to grant more permissions first.

---

## Status as Claude sees it

### What exists now

**One Quarto book** at `books/cambridge-comparisons/` (title: *Cambridge in
Comparison*, subtitle *How Cambridge CSD compares with nearby New York school
districts*). Config in `_quarto.yml`: `type: book`, cosmo theme, `jupyter:
python3`, code-fold on, giscus reader-comments wired to this repo's GitHub
Discussions, repo-actions (source/issue) enabled.

| File | Role | Approx size | State |
|---|---|---|---|
| `index.qmd` | Overview — comparison groups (graph=5, table=12 districts), year-label and measure cautions | ~405 words | solid, reusable as front-part overview |
| `class-sizes.qmd` | "Students per teacher" — class size + staffing; **6+ charts/tables** (median class size, grades 3–8 core, HS gateway, HS all-ten Regents, students/teacher, teachers-per-100, NCES FTE cross-check) | ~3,100 words | the big, multi-measure exploratory chapter |
| `spending.qmd` | Current operating spending per pupil — 1 chart + 1 table | ~740 words | shorter, fairly clean |
| `sources.qmd` | Appendix — full data-source index + APA references | ~1,600 words | thorough, in good shape |
| `comparisons.py` | Shared helpers: comparison groups, colour palette, data loaders, `staffing_table()`, `theme_ccs()` | ~430 lines | **well-built; the right abstraction.** Keep and build on. |

`_quarto.yml` currently lists chapters `index → class-sizes → spending` and one
appendix `sources`. **No parts yet** — flat.

### Data pipeline (strong)

`src/` has download + build scripts per source; `data/` keeps the documented
**raw / processed / crosswalks** split, each raw source carrying a tracked
`SOURCE.md`. `nysed_district_cd` (8-digit) is the stable join key.

Processed panels on disk (`data/processed/`):

- **Used in the book today:** `district_enrollment_teachers_panel.parquet`
  (enrollment + teachers), `class_size_by_district.parquet`,
  `ccd_pupil_teacher_ny.parquet` (NCES FTE), `spending_per_pupil.parquet`.
- **Built but no chapter yet:** `osc_school_district_tax_levies.parquet`
  (property taxes), `property_tax_report_card.parquet` (proposed budgets/levies),
  `school_finance.parquet` (full OSC finance), `cambridge_finance.parquet`.
- **⚠ Not on this machine:** `assessments_em_ela_math.parquet` (grades 3–8
  ELA/Math). The branch/PR for it landed (see `decisions.md`), but the parquet is
  **absent from `data/processed/`** here — it would need rebuilding (raw zips are
  ~3.3 GB, git-ignored) before a student-performance chapter can render.

### Loose ends

- **`nys_contingency_budgets.qmd` + `nys_contingency_budgets.html`** sit at the
  **repo root**, outside the book. The `.qmd` is **R / tidyverse**, not Python —
  standalone exploratory work on NYSED budget votes / contingency budgets. The
  2.3 MB `.html` is a built artifact tracked in the repo root.
- **Lingering local branches:** `feature/report-card-assessments`,
  `feature/student-performance` (current `main` is clean). `gh-pages` exists for
  publishing.
- `output/` directory at repo root (not yet inspected in detail).

### The interpretation problem — Claude's honest read

The **data discipline is excellent** (provenance, caveats, the NYSED-headcount
vs. NCES-FTE cross-check are genuinely careful). The **prose is the weak spot**,
and it matches what Don flagged:

1. **Descriptive, not interpretive.** The text reliably says *what* a chart shows
   ("Cambridge has the fewest students per teacher") but seldom *why it matters*
   or *what it means for Cambridge's situation*. E.g. it notes enrollment fell
   ~15% while teachers stayed flat — the mechanical cause — but never connects
   that to Cambridge's cost structure, budget stress, or "troubles," which is
   exactly what this audience cares about.
2. **Redundant across measures.** The same headline ("Cambridge is the
   smallest / lowest") lands ~6 times in one chapter (median class size → grades
   3–8 core → HS gateway → HS all-ten → students-per-teacher → teachers-per-100 →
   NCES). A lay reader finishes unsure which single fact mattered. This is the
   *deliberate* exploratory "keep multiple measures" strategy from `decisions.md`
   — which is fine for the back part, but is precisely what the front part must
   distill to one best measure.
3. **Method leads, stakes lag.** Chapters open with measure definitions and
   caveats before giving a lay reader a reason to care.
4. **Headlines occasionally outrun the data's stability** — bold takeaways rest
   on small-N, volatile high-school sections; the body hedges, but the bold
   headline doesn't.

Conclusion: the existing chapters are **good exploratory material** but read like
a workbook, not a public narrative. Don's instinct — move them to a back room and
write a clean front — is right.

---

## Proposed plan

### Shape: one book, two parts + a formal appendix

Claude's recommendation is **one book with Quarto `parts`**, not two separate
books — it avoids duplicating config/helpers and keeps the exploratory work one
click away for curious readers. (The `decisions.md` log floated a *separate* clean
summary book later; that remains an option, but parts-in-one-book is the lighter
first move and can be split later if needed.)

Proposed `_quarto.yml` structure (names are placeholders for discussion):

```
index.qmd                          # Overview (keep, lightly revised)

Part 1 — "Cambridge in context"    # polished front: ONE clean chapter per topic
  front/enrollment.qmd             #   (best single measure + the "why it matters")
  front/class-size.qmd
  front/spending.qmd
  front/taxes.qmd
  front/test-results.qmd
  ... (driven by Don's outline)

Part 2 — "Behind the numbers"      # exploratory, half-formed, for Don + deep readers
  exploratory/class-size-all-measures.qmd   # = today's class-sizes.qmd, moved
  exploratory/spending.qmd                  # = today's spending.qmd, moved
  exploratory/contingency-budgets.qmd       # ported/moved from the root R file (TBD)
  ...

Appendices (formal, for readers)
  sources.qmd                      # keep
  methods.qmd                      # new: measure definitions, caveats, cross-checks
```

Open layout choice: **subfolders** (`front/`, `exploratory/`, `appendix/`) vs.
**flat files with name prefixes**. Claude leans subfolders for legibility as it
grows; `comparisons.py` already finds the repo root robustly, but moving `.qmd`
files into subfolders needs a check that the `sys.path.insert(0, os.getcwd())` +
`import comparisons as cc` pattern still resolves (may need the import shim
adjusted, or `comparisons.py` kept at the book root and the path insert pointed at
it).

### Sequencing

- **Phase 0 — align (needs Don).** Don answers the decisions below and supplies a
  rough front-part outline (topics, order, the angle/spine).
- **Phase 1 — restructure, no new content (one feature branch).** Rewrite
  `_quarto.yml` into the parts layout; **move** today's `class-sizes.qmd` and
  `spending.qmd` into the exploratory part (unchanged); create **front-part
  stubs** (each: title + one orienting sentence + a visible `TODO` placeholder);
  add a `methods.qmd` appendix stub; confirm the book still renders end-to-end.
  Nothing is deleted; the exploratory material just moves. Open a PR for Don.
- **Phase 2 — write the front, one chapter at a time (iterative, Don in the
  loop).** Per front chapter: pick the **single best measure**, write a narrative
  that leads with stakes and adds the "why it matters for Cambridge" layer, pull
  **one** chart + maybe one table, and push the alternative measures / cross-checks
  down into the exploratory part or the methods appendix. Draft → Don edits
  interpretation → iterate. (Interpretation is the known weak spot, so Don reviews
  every front chapter's prose closely.)
- **Phase 3 — new chapters** as data + outline allow: property taxes (data
  ready), Property Tax Report Card, student test results (needs the assessments
  parquet rebuilt first), state aid (candidate source).

### Guardrails to honor (from `meta/`)

- Work on a **feature branch**; **never** write to `main` (a guard hook
  enforces). One unit of work = one branch + PR for Don to merge.
- **Chart titles state only facts**; interpretation goes in a headline *above* the
  plot. Plain-language prose; explain any technical term. Year labels =
  school-year-ending (2025 = 2024–25).
- Keep `meta/` (and any `SOURCE.md`) updated **in the same change** as the work.
- Don't drop existing exploratory measures — **move**, don't delete (the
  "multiple measures now, best measure later" decision).

---

## Decisions needed from Don

1. **One book with parts, or two separate books?** Claude leans **one book with
   parts** (shared config + `comparisons.py`, exploratory work stays one click
   away). Two books is cleaner separation but more duplication.
2. **Subfolders (`front/`, `exploratory/`, `appendix/`) or flat files with name
   prefixes?** Claude leans **subfolders** (pending the import-path check above).
3. **The root `nys_contingency_budgets` R file** — three options:
   (a) **port to Python** into the exploratory part now (consistent toolchain,
   more work); (b) **move it as-is** into the exploratory part and keep it R for
   now (Quarto can run R + Python); (c) **leave it at the repo root**, out of the
   book, for later. Also: should the tracked 2.3 MB built `.html` at the repo root
   be removed/git-ignored?
4. **Part titles.** Working names are *"Cambridge in context"* (front) and
   *"Behind the numbers"* (exploratory). Better names? Does the book title
   *Cambridge in Comparison* still fit, or should the front part broaden beyond
   "comparison" (e.g. toward Cambridge's own trajectory and "troubles")?
5. **Scope of the front "comparison" framing.** The current book is explicitly
   *comparative* (Cambridge vs. nearby districts). Does the front part stay
   comparison-first, or shift toward **Cambridge's own story** (enrollment
   decline, finances, the troubles) with comparisons as support?
6. **Assessments data.** Should the next session **rebuild
   `assessments_em_ela_math.parquet`** (raw zips ~3.3 GB, git-ignored — confirm
   they're still on disk) so a test-results chapter is possible, or defer student
   performance until later?

---

## Outline needed from Don (the key input for Phase 2)

A **rough outline of the front part**: which topics, in what order, and the angle
for each. The central question is the **spine** — e.g.:

- *Option A (causal narrative):* enrollment decline → staffing pressure →
  spending per pupil → property taxes → outcomes/test results. "Fewer students,
  steady staff, rising per-pupil cost, and what it buys."
- *Option B (topic tour):* independent chapters (class size, spending, taxes,
  outcomes), each self-contained, lighter on through-line.
- *Option C (the "troubles" frame):* lead with whatever the community's actual
  concerns are (budget votes? a specific fiscal event? consolidation pressure?)
  and organize the data around answering them.

For each front chapter Don ideally notes: the **one question** it answers, the
**single best measure** to answer it, and the **takeaway** he wants a lay reader
to leave with. Claude will draft from there and Don edits the interpretation.

---

## Suggested first action for the next session

1. Read `meta/` docs, then this file.
2. Get Don's answers to **Decisions needed** + his **front-part outline**.
3. Then (and only then) execute **Phase 1**: a feature branch that restructures
   `_quarto.yml` into parts, moves the two existing chapters into the exploratory
   part, creates front-part stubs + a methods appendix stub, verifies the render,
   and opens a PR — **no new interpretive prose yet**.
