"""Download NYS state school aid for the comparison districts (data.ny.gov).

The mergers chapter needs each comparison district's **Foundation Aid** — the base
against which **Reorganization Incentive Operating Aid (RIOA)** is calculated
after a merger — plus the two reorganization-incentive line items themselves
(which are $0 for districts that have not merged). State aid is also useful
context for the spending and tax chapters.

Source: New York's open-data portal, dataset **"State Aid to School Districts"**
(`9pb8-dg53`) — <https://data.ny.gov/Education/State-Aid-to-School-Districts/9pb8-dg53>.
Each row is one **aid category** for one **district** under one **enacted-budget
event** (a fiscal year), with the enacted amount (`school_year`) and the prior
year's amount (`base_year`). We pull all events and categories for the small set
of comparison districts only (the book's scope), via the Socrata API.

The state-aid `beds_code` (6-digit) equals the first six digits of the project's
`nysed_district_cd`, i.e. the `beds6` already in the district crosswalk — so the
output joins cleanly to the rest of the project.

Output (data/raw/, git-ignored, regenerable):
    state_aid/state_aid_comparison.csv

Usage:  python src/download_state_aid.py   [--force]
"""

from __future__ import annotations

import argparse
import csv
import json
import urllib.parse
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent

OUT_DIR = PROJECT_ROOT / "data" / "raw" / "state_aid"
OUT_PATH = OUT_DIR / "state_aid_comparison.csv"

API = "https://data.ny.gov/resource/9pb8-dg53.json"

#: The comparison districts (Cambridge + every Washington County K-12 district +
#: Schuylerville and Hoosick Falls). Values are beds6 = nysed_district_cd[:6].
#: Putnam (K-8, no high school) is included for completeness but is excluded
#: from K-12 comparisons elsewhere.
COMPARISON_BEDS6: dict[str, str] = {
    "641610": "Cambridge",
    "640101": "Argyle",
    "640502": "Fort Ann",
    "640601": "Fort Edward",
    "640701": "Granville",
    "640801": "Greenwich",
    "641001": "Hartford",
    "641301": "Hudson Falls",
    "641401": "Putnam",
    "641501": "Salem",
    "641701": "Whitehall",
    "521701": "Schuylerville",
    "490501": "Hoosick Falls",
}

_UA = ("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) "
       "Chrome/124.0 Safari/537.36")


def fetch(*, force: bool) -> list[dict]:
    codes = "', '".join(COMPARISON_BEDS6)
    where = f"beds_code IN ('{codes}')"
    params = urllib.parse.urlencode({"$where": where, "$limit": "50000",
                                     "$order": "beds_code,event,aid_category"})
    url = f"{API}?{params}"
    print(f"  fetching {len(COMPARISON_BEDS6)} districts' state aid from {API}")
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=180) as resp:
        rows = json.load(resp)
    print(f"  got {len(rows)} rows")
    return rows


def main(*, force: bool) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    rows = fetch(force=force)
    if not rows:
        raise RuntimeError("No rows returned from the state-aid API")
    cols = ["event", "beds_code", "county", "district", "aid_category",
            "base_year", "school_year", "change", "pct_change"]
    with open(OUT_PATH, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"Wrote {len(rows)} rows to {OUT_PATH.relative_to(PROJECT_ROOT)}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--force", action="store_true", help="re-download even if cached")
    args = ap.parse_args()
    main(force=args.force)
