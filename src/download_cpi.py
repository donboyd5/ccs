"""Download the monthly Consumer Price Index (CPI-U) from the Bureau of Labor Statistics.

The website shows dollar amounts adjusted for inflation. The price index used is
the **CPI-U, All items, U.S. city average, not seasonally adjusted**
(BLS series ``CUUR0000SA0``, 1982-84 = 100): the broadest standard measure of
consumer prices. It measures what households pay, not what school districts pay,
so it answers "what did this spending cost taxpayers in today's dollars", not
"how much more schooling did it buy". The index is a deliberate, replaceable
choice: only this script and ``build_price_index.py`` know which index is used.

Source: the BLS Public Data API, version 2 (no registration key needed for a
request this small): <https://www.bls.gov/developers/>. Unregistered requests
are limited to 10 years each, so the series is fetched in 10-year pieces.

Output (data/raw/bls_cpi/, git-ignored, regenerable):
    cpi_u_monthly.csv    one row per month: year, month, value, note

A month with no published value is kept with a blank ``value`` and BLS's
footnote in ``note`` (October 2025 was never published, because of the 2025
lapse in federal appropriations).

Usage:  python src/download_cpi.py
"""

from __future__ import annotations

import csv
import datetime as dt
import json
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "bls_cpi"
OUT_PATH = OUT_DIR / "cpi_u_monthly.csv"

API = "https://api.bls.gov/publicAPI/v2/timeseries/data/"
SERIES = "CUUR0000SA0"  # CPI-U, All items, U.S. city average, not seasonally adjusted

#: First calendar year fetched. School finance data start with the school year
#: ending 1995, which begins in July 1994.
FIRST_YEAR = 1994


def fetch(start: int, end: int) -> list[dict]:
    body = json.dumps(
        {"seriesid": [SERIES], "startyear": str(start), "endyear": str(end)}
    ).encode()
    req = urllib.request.Request(
        API, data=body, headers={"Content-Type": "application/json"}
    )
    with urllib.request.urlopen(req, timeout=120) as resp:
        payload = json.load(resp)
    if payload.get("status") != "REQUEST_SUCCEEDED":
        raise RuntimeError(f"BLS API: {payload.get('status')} {payload.get('message')}")
    series = payload["Results"]["series"]
    if not series or not series[0]["data"]:
        raise RuntimeError(f"BLS API returned no data for {start}-{end}")
    return series[0]["data"]


def main() -> None:
    this_year = dt.date.today().year
    rows: dict[tuple[int, int], tuple[str, str]] = {}
    for start in range(FIRST_YEAR, this_year + 1, 10):
        end = min(start + 9, this_year)
        print(f"fetching {SERIES} {start}-{end}")
        for r in fetch(start, end):
            if not r["period"].startswith("M") or r["period"] == "M13":
                continue  # M13 is the annual average
            value = r["value"].strip()
            note = "; ".join(f.get("text", "") for f in r.get("footnotes", []) if f)
            rows[(int(r["year"]), int(r["period"][1:]))] = (
                value if value not in ("-", "") else "",
                note,
            )

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    with open(OUT_PATH, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["year", "month", "value", "note"])
        for (year, month), (value, note) in sorted(rows.items()):
            w.writerow([year, month, value, note])

    keys = sorted(rows)
    missing = [k for k in keys if rows[k][0] == ""]
    print(
        f"wrote {len(keys)} months to {OUT_PATH.relative_to(PROJECT_ROOT)} "
        f"({keys[0][0]}-{keys[0][1]:02d} to {keys[-1][0]}-{keys[-1][1]:02d})"
    )
    for year, month in missing:
        print(f"  no published value: {year}-{month:02d} ({rows[(year, month)][1]})")


if __name__ == "__main__":
    main()
