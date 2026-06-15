"""Download NYSED Report Card (SRC) Access databases, statewide, by year.

Each year NYSED posts a single statewide **Report Card Database** — a zipped
MS-Access database (``SRC{YEAR}.accdb``) holding *every* report-card table for
that school year: grades 3-8 ELA/Math assessment results, Regents results,
accountability, enrollment, staff, graduation, etc. This script downloads the
annual zip for a 10-year window and saves it under a clean, sortable name in
``data/raw/nysed_report_card/zips/``.

The zips are large (~250-385 MB each, ~3.3 GB for 10 years) and git-ignored; the
``.accdb`` inside is larger still, so we keep only the **zip** as the raw artifact
and let ``build_assessments.py`` extract each ``.accdb`` to a temp dir on demand.

``year_end`` is the spring (school-year-end) year, matching the rest of the
project: NYSED's "2024-25" Report Card -> ``SRC2025.zip`` -> ``year_end = 2025``.
The REGISTRY below *is* the provenance record (mirrored in this source's
SOURCE.md).

Source page: https://data.nysed.gov/downloads.php  (section "Report Card Database")

Usage:
    python src/download_report_card.py             # download all missing years
    python src/download_report_card.py 2025 2024   # only these year_ends
    python src/download_report_card.py --force      # re-download everything
"""

from __future__ import annotations

import sys
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_report_card" / "zips"
BASE = "https://data.nysed.gov"

#: year_end (spring year) -> original NYSED URL of that year's Report Card zip.
#: NYSED moved the path from ``/files/reportcards/`` to ``/files/essa/`` starting
#: with SY 2017-18 (year_end 2018); both are recorded here verbatim.
REGISTRY: dict[int, str] = {
    2025: BASE + "/files/essa/24-25/SRC2025.zip",
    2024: BASE + "/files/essa/23-24/SRC2024.zip",
    2023: BASE + "/files/essa/22-23/SRC2023.zip",
    2022: BASE + "/files/essa/21-22/SRC2022.zip",
    2021: BASE + "/files/essa/20-21/SRC2021.zip",
    2020: BASE + "/files/essa/19-20/SRC2020.zip",
    2019: BASE + "/files/essa/18-19/SRC2019.zip",
    2018: BASE + "/files/essa/17-18/SRC2018.zip",
    2017: BASE + "/files/reportcards/16-17/SRC2017.zip",
    2016: BASE + "/files/reportcards/15-16/SRC2016.zip",
}

# NYSED serves these fine to a plain client, but present a browser-ish UA to be safe.
_UA = "Mozilla/5.0 (ccs data pipeline; +https://github.com/donboyd5/ccs)"


def dest_for(year_end: int) -> Path:
    """On-disk path ``zips/SRC{year_end}.zip`` (the original NYSED basename)."""
    return OUT_DIR / f"SRC{year_end}.zip"


def fetch_one(year_end: int, url: str, *, force: bool) -> tuple[Path, int, str]:
    dest = dest_for(year_end)
    if dest.exists() and not force:
        return dest, dest.stat().st_size, "skip (exists)"
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=600) as resp:
        data = resp.read()
    if len(data) < 100_000:  # a real SRC zip is hundreds of MB
        raise OSError(f"{url} returned only {len(data):,} bytes (blocked/empty?)")
    dest.write_bytes(data)
    return dest, len(data), "downloaded"


def main() -> None:
    argv = sys.argv[1:]
    force = "--force" in argv
    years = sorted((int(a) for a in argv if a.isdigit()), reverse=True) or sorted(
        REGISTRY, reverse=True
    )
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Report Card (SRC) -> {OUT_DIR.relative_to(PROJECT_ROOT)}")
    ok = 0
    for year_end in years:
        url = REGISTRY.get(year_end)
        if url is None:
            print(f"  {year_end}: no URL in REGISTRY, skipping")
            continue
        try:
            dest, size, status = fetch_one(year_end, url, force=force)
            print(f"  {dest.name:<14} {size:>13,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001 - report and continue
            print(f"  {year_end}: FAILED ({type(exc).__name__}: {exc})")
    print(f"{ok}/{len(years)} requested files present.")


if __name__ == "__main__":
    main()
