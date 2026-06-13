"""Download NYSED Property Tax Report Card (PTRC) statewide workbooks.

Saves each year's main (initial-vote) workbook to
``data/raw/nysed_property_tax_report_card/<year_end>_ptrc.<ext>`` under a clean,
sortable name, where ``year_end`` is the second year of NYSED's budget-span label
(NYSED "2024-25" -> 2025) and ``<ext>`` follows the source URL (NYSED posts the
older years as legacy ``.xls``). The original NYSED filenames are wildly
inconsistent, so the REGISTRY below *is* the provenance record (mirrored in that
folder's SOURCE.md). Re-run any time; existing files are skipped unless ``--force``.

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
#: The on-disk extension follows the source URL (older years are legacy ``.xls``).
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
    # --- historical files (added on feature/ptrc-history). .xlsx from 2014-15 on,
    # --- legacy .xls before that. Layouts predating the 2012 tax-cap law differ
    # --- (no levy-limit/exclusion columns), so not all of these feed the builder.
    2018: BASE + "Copyof2017-18_PTRC_FINAL_Posting_5-15-17_POSTONLY_R.xlsx",
    2017: BASE + "2016_17PTRC_Post_Final.xlsx",
    2016: BASE + "docs/2015-16_PTRC_Final_5_12_15.xlsx",
    2015: BASE + "docs/2014-15_PTRC_revised_5_14_14_post.xlsx",
    2014: BASE + "docs/2013-14_PTRC_5_10_13_post.xls",
    2013: BASE + "docs/2012-13_PTRC5_10_12_Full_Data_Post.xls",
    2012: BASE + "docs/2011-12PTRC_5_4_11_post.xls",
    2011: BASE + "docs/2010-11PTRC5-19-10_post_access.xls",
    2010: BASE + "docs/2009-10PTRC5_13_09_post.xls",
    2009: BASE + "docs/SM_PTRC_5_02_08_Post_r.xls",
    2008: BASE + "docs/PTRC_Web_5_12_07.xls",
    2007: BASE + "docs/PropTaxNumbers2006-6-13-06.xls",
    2006: BASE + "docs/2005PropertyTaxReport2.xls",
    2005: BASE + "docs/property-tax-report-card-count-2004-05.xls",
}

# NYSED blocks the default urllib agent on some paths; present a browser UA.
_UA = "Mozilla/5.0 (ccs data pipeline; +https://github.com/donboyd5/ccs)"


def dest_for(year_end: int, url: str) -> Path:
    """On-disk path ``<year_end>_ptrc.<ext>``; ext follows the source URL."""
    ext = ".xls" if url.lower().endswith(".xls") else ".xlsx"
    return OUT_DIR / f"{year_end}_ptrc{ext}"


def fetch_one(year_end: int, url: str, *, force: bool) -> tuple[Path, int, str]:
    dest = dest_for(year_end, url)
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
    print(f"PTRC -> {OUT_DIR.relative_to(PROJECT_ROOT)}")
    ok = 0
    for year_end in sorted(REGISTRY, reverse=True):
        url = REGISTRY[year_end]
        try:
            dest, size, status = fetch_one(year_end, url, force=force)
            print(f"  {dest.name:<16} {size:>9,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001 - report and continue
            print(f"  {year_end}: FAILED ({type(exc).__name__}: {exc})")
    print(f"{ok}/{len(REGISTRY)} files present.")


if __name__ == "__main__":
    main()
