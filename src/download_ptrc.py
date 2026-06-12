"""Download NYSED Property Tax Report Card (PTRC) statewide workbooks.

Saves each year's main (initial-vote) workbook to
``data/raw/nysed_property_tax_report_card/<year_end>_ptrc.xlsx`` under a clean,
sortable name, where ``year_end`` is the second year of NYSED's budget-span label
(NYSED "2024-25" -> 2025). The original NYSED filenames are wildly inconsistent,
so the REGISTRY below *is* the provenance record (mirrored in that folder's
SOURCE.md). Re-run any time; existing files are skipped unless ``--force``.

Source page: https://www.p12.nysed.gov/mgtserv/propertytax/

Usage:
    python src/download_ptrc.py            # download missing years
    python src/download_ptrc.py --force    # re-download all
"""

from __future__ import annotations

import sys
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_property_tax_report_card"
BASE = "https://www.p12.nysed.gov/mgtserv/propertytax/"

#: year_end (second year of the NYSED budget span) -> original NYSED URL.
REGISTRY: dict[int, str] = {
    2027: BASE + "docs/2026-27-ptrc-final.xlsx",
    2026: BASE + "docs/2025-26-ptrc-20250509.xlsx",
    2025: BASE + "docs/2024-25-ptrc-20240510-.xlsx",
    2024: BASE + "docs/PTRC_23-24_FINAL.xlsx",
    2023: BASE + "docs/PTRC_2022-2023_FINAL.xlsx",
    2022: BASE + "docs/2021-22_PTRC.xlsx",
    2021: BASE + "docs/2020-21-ptrc-sams-data.xlsx",
    2020: BASE + "2019-20PTRCPost_R_ELW.xlsx",
    2019: BASE + "2018-19PTRC5_11_18_Post_Final_000.xlsx",
}

# NYSED blocks the default urllib agent on some paths; present a browser UA.
_UA = "Mozilla/5.0 (ccs data pipeline; +https://github.com/donboyd5/ccs)"


def fetch_one(year_end: int, url: str, *, force: bool) -> tuple[int, str]:
    dest = OUT_DIR / f"{year_end}_ptrc.xlsx"
    if dest.exists() and not force:
        return dest.stat().st_size, "skip (exists)"
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=120) as resp:
        data = resp.read()
    if len(data) < 1000:
        raise OSError(f"{url} returned only {len(data)} bytes (blocked/empty?)")
    dest.write_bytes(data)
    return len(data), "downloaded"


def main() -> None:
    force = "--force" in sys.argv[1:]
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"PTRC -> {OUT_DIR.relative_to(PROJECT_ROOT)}")
    ok = 0
    for year_end in sorted(REGISTRY, reverse=True):
        url = REGISTRY[year_end]
        try:
            size, status = fetch_one(year_end, url, force=force)
            print(f"  {year_end}_ptrc.xlsx  {size:>8,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001 - report and continue
            print(f"  {year_end}: FAILED ({type(exc).__name__}: {exc})")
    print(f"{ok}/{len(REGISTRY)} files present.")


if __name__ == "__main__":
    main()
