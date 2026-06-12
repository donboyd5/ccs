# NYSED — school district budget vote & re-vote results

Statewide results of the annual May school-budget votes (and any June re-votes)
that New York districts hold to adopt their budgets, published by NYSED Office of
Educational Management Services.

| item | value |
|---|---|
| publisher | NYSED, Educational Management Services |
| source index | <https://www.p12.nysed.gov/mgtserv/> (budget vote results) |
| coverage | vote years **2013–2026** (+ re-votes most years) |
| grain | one row per district, per vote |

## Filenames — original → on-disk (this is a generated cache, not a hand-rename)

The on-disk names `<year>_<vote|revote>.<ext>` are **generated deterministically**
by the download step in [`nys_contingency_budgets.qmd`](../../../nys_contingency_budgets.qmd)
(`sprintf("%d_%s%s", year, kind, ext)`), which keeps the **original NYSED URL** for
every file in its `registry` table. So the mapping back to source is preserved in
code. The originals (note NYSED's wildly inconsistent naming):

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

(All under `https://p12.nysed.gov/mgtserv/`. Full table with `download: true` to
refresh is in the notebook's `registry` chunk.)

## Time convention

NYSED labels a file by the budget **span end** year (the May-2025 vote is in the
"2025-26" file). The notebook **relabels `year` to the vote's calendar year**
(`year = year - 1L`), so `2026_vote.xlsx` carries `year = 2025` downstream (its
embedded vote date is 2025-05-20). Watch this when joining to other tables.

## Also here

`djb_revotes.xlsx` — a small **hand-curated** workbook (author: Don Boyd), not a
NYSED download; used by the notebook's re-vote cross-checks.

## Processing

[`nys_contingency_budgets.qmd`](../../../nys_contingency_budgets.qmd) (R / `readxl`)
downloads, caches here, parses the inconsistent annual layouts, and analyzes
contingency budgets. Set its `download: true` param to refresh from NYSED.
