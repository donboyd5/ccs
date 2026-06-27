# NYSED — school district budget vote & re-vote results

Statewide results of the annual May school-budget votes (and any June re-votes)
that New York districts hold to adopt their budgets, published by NYSED Office of
Educational Management Services.

| item | value |
|---|---|
| publisher | NYSED, Educational Management Services |
| source index | <https://www.p12.nysed.gov/mgtserv/> (budget vote results) |
| coverage | vote years **2013–2026** (+ re-votes most years; 2018 has no published re-vote) |
| grain | one row per district, per vote |
| time convention | on-disk `year` = budget **span-end** year (NYSED "2024-25" → 2025); the builder relabels to the vote's **calendar** year (`year − 1`) |

## Filenames — original → on-disk

The on-disk names `<year>_<vote|revote>.<ext>` (where `<year>` is the budget
span-end year and `<ext>` follows the source URL) are **generated deterministically
by [`src/download_budget_votes.py`](../../../src/download_budget_votes.py)**, which
keeps the **original NYSED URL** for every file in its `REGISTRY`. The original
NYSED filenames are wildly inconsistent:

| on-disk | original NYSED URL |
|---|---|
| `2026_vote.xlsx`   | …/votingresults/2025-26-budget-vote-results.xlsx |
| `2026_revote.xlsx` | …/votingresults/2025-26-budget-revote-results.xlsx |
| `2025_vote.xlsx`   | …/votingresults/2024-25-budget-vote-results-final.xlsx |
| `2025_revote.xlsx` | …/votingresults/2024-25-budget-revote-results-final.xlsx |
| `2024_vote.xlsx`   | …/votingresults/2023-24_Budget_Vote_Results_FINAL.xlsx |
| `2024_revote.xlsx` | …/votingresults/2023-24_Budget_Revote_Results_FINAL.xlsx |
| `2023_vote.xlsx`   | …/votingresults/docs/2022-23_Budget_Vote_Results_FINAL.xlsx |
| `2023_revote.xlsx` | …/votingresults/docs/2022-23-budget-revote-results.xlsx |
| `2022_vote.xlsx`   | …/documents/2021-22-budget-vote-results_final.xlsx |
| `2022_revote.xlsx` | …/documents/2021-22-budget-revote-results_final.xlsx |
| `2021_vote.xlsx`   | …/documents/2020-21-budget-vote-results_post.xlsx |
| `2021_revote.xlsx` | …/documents/2020-21_Budget_Re-Vote_Results_FINAL.xlsx |
| `2020_vote.xlsx`   | …/votingresults/2019-20BudgetVoteResults_POST_FFinal.xlsx |
| `2020_revote.xlsx` | …/votingresults/SCHOOLBUDGETRE-VOTE6-18-19.xlsx |
| `2019_vote.xls`    | …/documents/2018SchoolBudgetVoteResults_POST.xls |
| `2019_revote.xlsx` | …/votingresults/2018SchoolBudgetRevoteResults_Final.xlsx |
| `2018_vote.xls`    | …/documents/Copyof2017budgetvoteresults5-16-17.xls |
| `2017_vote.xls`    | …/documents/2016budgetvoteresultsmay17_POST_rjune29.xls |
| `2017_revote.xls`  | …/documents/2016BudgetRevotes_Post_Final_R.xls |
| `2016_vote.xls`    | …/votingresults/docs/2015budgetvoteresults.xls |
| `2016_revote.xls`  | …/votingresults/docs/2015-16budgetdefeat.xls |
| `2015_vote.xls`    | …/votingresults/docs/2014budgetvoteresults.xls |
| `2015_revote.xls`  | …/votingresults/docs/2014-15budgetdefeat.xls |
| `2014_vote.xls`    | …/votingresults/docs/2013budgetvoteresults.xls |
| `2014_revote.xls`  | …/votingresults/docs/2013-14budgetdefeat.xls |
| `2013_vote.xls`    | …/votingresults/docs/2012budgetvoteresults.xls |
| `2013_revote.xls`  | …/votingresults/docs/2012budgetdefeat-1.xls |

(All under `https://p12.nysed.gov/mgtserv/`. The full URL table lives in the
`REGISTRY` of `download_budget_votes.py`; re-run with `--force` to refresh.)

## Processing pipeline

1. **Download** — [`src/download_budget_votes.py`](../../../src/download_budget_votes.py)
   fetches each year's vote and re-vote workbook (browser user-agent; NYSED blocks
   the default urllib agent on some paths). Existing files are skipped unless
   `--force`.
2. **Build** — [`src/build_budget_votes.py`](../../../src/build_budget_votes.py)
   parses the 27 workbooks into one tidy panel,
   `data/processed/budget_votes.parquet`. NYSED changes the column headers, file
   format, and layout every year, so each sheet is reduced to a standard schema by
   **heuristic layout detection** (ported from the original R notebook): it infers
   the district-name column, the Yes/No count columns, and the 60%-supermajority
   flag column, then drops title/section/total rows. The build prints a per-file
   summary and a `% Yes` reconciliation check (where a "% Yes" column exists, the
   detected Yes/No counts rebuild it to 0.0000 median error).

**Schema** (`budget_votes.parquet`): `year` (vote calendar year), `kind`
(`vote`/`revote`), `district` (name, whitespace-squashed), `district_key`
(normalized name — upper-cased, punctuation removed, district-type suffix stripped
— used to join a first vote to its re-vote across NYSED's inconsistent name
forms), `yes`, `no`, and `above_cap` (True = the budget needed a 60%
supermajority, i.e. was above the tax cap).

### Known data-quality notes

- **No codes, no levy.** These files carry district *names* (not BEDS codes) and
  **no levy figures** — only the "60% required" flag. They are therefore **not
  joinable** to the project's `nysed_district_cd` crosswalk, and cap status comes
  entirely from the flag.
- **Name-form drift.** Older files (through ~2021) list districts in ALL CAPS;
  newer files use Title Case; the same district appears as e.g. "Corning-Painted
  Post Area SD" in one file and "...CSD" in another. The `district_key` normalizes
  this; re-vote linking is ~100% from 2020 on, and best-effort (a handful of
  misses per year) in the older files.
- **Year convention.** `year` is the vote's *calendar* year (the May 2024 vote is
  `2024`); the budget adopted funds the *following* school year. Do not
  double-count when merging with school-year-ending series.
- **2020 was atypical** (all-absentee voting, shifted June re-vote date).
- **Pre-2012 omitted** — no tax cap, so the above/below-cap split is undefined.

## Also here

`djb_revotes.xlsx` — a small **hand-curated** workbook (author: Don Boyd), not a
NYSED download; kept for reference.

## Provenance

This source and its parser were ported from a standalone R notebook
(`nys_contingency_budgets.qmd`, since retired) into the project's standard
`src/` download + build pipeline. The original heuristic layout detection is
preserved (faithful port) in `build_budget_votes.py`.
