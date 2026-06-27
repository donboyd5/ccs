"""Download NYSED statewide school-district budget vote & re-vote workbooks.

NYSED posts two Excel files per year on its *Statewide School District Budget
Voting Results* page (https://p12.nysed.gov/mgtserv/votingresults/): the May
**vote** results (every district) and the June **revote** results (only the
districts that failed the first vote). This script saves them to
``data/raw/nysed_budget_votes/<budget_end_year>_<vote|revote>.<ext>`` under a
clean, sortable name. NYSED's original filenames are wildly inconsistent, so the
REGISTRY below *is* the provenance record (mirrored in that folder's
SOURCE.md). Re-run any time; existing files are skipped unless ``--force``.

**Year convention.** Files are named by the budget **span-end** year (NYSED's
"2024-25" file -> 2025), which is what the on-disk name and the REGISTRY key
use. The builder (``build_budget_votes.py``) then relabels to the vote's
*calendar* year (budget_end - 1, so the "2025" file holds the May-2024 vote).
2018 has no published revote (NYSED did not post one).

Usage:
    python src/download_budget_votes.py            # download missing files
    python src/download_budget_votes.py --force    # re-download all
"""

from __future__ import annotations

import sys
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_budget_votes"

#: (budget_end_year, kind, exact NYSED URL). budget_end_year is the second year
#: of NYSED's budget span (the "2024-25" file -> 2025); kind is "vote" (May) or
#: "revote" (June). The on-disk extension follows the source URL (older years
#: are legacy ``.xls``). Lifted verbatim from the original R notebook's registry.
REGISTRY: list[tuple[int, str, str]] = [
    (2026, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/2025-26-budget-vote-results.xlsx"),
    (2026, "revote", "https://p12.nysed.gov/mgtserv/votingresults/2025-26-budget-revote-results.xlsx"),
    (2025, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/2024-25-budget-vote-results-final.xlsx"),
    (2025, "revote", "https://p12.nysed.gov/mgtserv/votingresults/2024-25-budget-revote-results-final.xlsx"),
    (2024, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/2023-24_Budget_Vote_Results_FINAL.xlsx"),
    (2024, "revote", "https://p12.nysed.gov/mgtserv/votingresults/2023-24_Budget_Revote_Results_FINAL.xlsx"),
    (2023, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/docs/2022-23_Budget_Vote_Results_FINAL.xlsx"),
    (2023, "revote", "https://p12.nysed.gov/mgtserv/votingresults/docs/2022-23-budget-revote-results.xlsx"),
    (2022, "vote",   "https://p12.nysed.gov/mgtserv/documents/2021-22-budget-vote-results_final.xlsx"),
    (2022, "revote", "https://p12.nysed.gov/mgtserv/documents/2021-22-budget-revote-results_final.xlsx"),
    (2021, "vote",   "https://www.p12.nysed.gov/mgtserv/documents/2020-21-budget-vote-results_post.xlsx"),
    (2021, "revote", "https://www.p12.nysed.gov/mgtserv/documents/2020-21_Budget_Re-Vote_Results_FINAL.xlsx"),
    (2020, "vote",   "https://www.p12.nysed.gov/mgtserv/votingresults/2019-20BudgetVoteResults_POST_FFinal.xlsx"),
    (2020, "revote", "https://www.p12.nysed.gov/mgtserv/votingresults/SCHOOLBUDGETRE-VOTE6-18-19.xlsx"),
    (2019, "vote",   "https://p12.nysed.gov/mgtserv/documents/2018SchoolBudgetVoteResults_POST.xls"),
    (2019, "revote", "https://p12.nysed.gov/mgtserv/votingresults/2018SchoolBudgetRevoteResults_Final.xlsx"),
    (2018, "vote",   "https://p12.nysed.gov/mgtserv/documents/Copyof2017budgetvoteresults5-16-17.xls"),
    # 2018 revote: none published by NYSED.
    (2017, "vote",   "https://p12.nysed.gov/mgtserv/documents/2016budgetvoteresultsmay17_POST_rjune29.xls"),
    (2017, "revote", "https://p12.nysed.gov/mgtserv/documents/2016BudgetRevotes_Post_Final_R.xls"),
    (2016, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/docs/2015budgetvoteresults.xls"),
    (2016, "revote", "https://p12.nysed.gov/mgtserv/votingresults/docs/2015-16budgetdefeat.xls"),
    (2015, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/docs/2014budgetvoteresults.xls"),
    (2015, "revote", "https://p12.nysed.gov/mgtserv/votingresults/docs/2014-15budgetdefeat.xls"),
    (2014, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/docs/2013budgetvoteresults.xls"),
    (2014, "revote", "https://p12.nysed.gov/mgtserv/votingresults/docs/2013-14budgetdefeat.xls"),
    (2013, "vote",   "https://p12.nysed.gov/mgtserv/votingresults/docs/2012budgetvoteresults.xls"),
    (2013, "revote", "https://p12.nysed.gov/mgtserv/votingresults/docs/2012budgetdefeat-1.xls"),
]

# NYSED blocks the default urllib agent on some paths; present a browser UA.
_UA = "Mozilla/5.0 (ccs data pipeline; +https://github.com/donboyd5/ccs)"


def dest_for(year: int, kind: str, url: str) -> Path:
    """On-disk path ``<budget_end_year>_<kind>.<ext>``; ext follows the URL."""
    ext = ".xls" if url.lower().endswith(".xls") else ".xlsx"
    return OUT_DIR / f"{year}_{kind}{ext}"


def fetch_one(year: int, kind: str, url: str, *, force: bool) -> tuple[Path, int, str]:
    dest = dest_for(year, kind, url)
    if dest.exists() and not force:
        return dest, dest.stat().st_size, "skip (exists)"
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=120) as resp:
        data = resp.read()
    if len(data) < 1000:
        raise OSError(f"{url} returned only {len(data)} bytes (blocked/empty?)")
    dest.write_bytes(data)
    return dest, len(data), "downloaded"


def main() -> None:
    force = "--force" in sys.argv[1:]
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"budget votes -> {OUT_DIR.relative_to(PROJECT_ROOT)}")
    ok = 0
    for year, kind, url in REGISTRY:
        try:
            dest, size, status = fetch_one(year, kind, url, force=force)
            print(f"  {dest.name:<20} {size:>9,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001 - report and continue
            print(f"  {year}_{kind}: FAILED ({type(exc).__name__}: {exc})")
    print(f"{ok}/{len(REGISTRY)} files present.")


if __name__ == "__main__":
    main()
