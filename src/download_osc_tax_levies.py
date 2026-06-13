"""Download OSC real-property-tax levy / full-value / full-value-tax-rate tables
for **school districts** (Table 3), one workbook per year.

Source page (the "School Districts" file list):
    https://www.osc.ny.gov/local-government/data/real-property-tax-levies-taxable-full-value-and-full-value-tax-rates

Each annual workbook gives, for every NY school district, the district's tax levy,
taxable full value, and full-value tax rate **broken out by the city/town the
district overlaps** (the "component" detail), and (2019+) a district *Total* row.

This project pulls **2015-2025** (the consistent full-value-only era; see the
SOURCE.md in the output folder for the three format eras and the caveats). Files
are saved under clean, sortable names ``<year>_school_district_tax.<ext>`` where
``<year>`` is OSC's own label on the file. OSC's original filenames are recorded
in REGISTRY below (and mirrored in SOURCE.md) so the data stays reproducible.

We also archive the two OSC data-description PDFs and a snapshot of the landing
page HTML, purely for provenance.

Re-run any time; existing files are skipped unless ``--force``.

Usage:
    python src/download_osc_tax_levies.py           # download missing files
    python src/download_osc_tax_levies.py --force    # re-download everything
"""

from __future__ import annotations

import sys
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "osc_school_district_tax_levies"
BASE = "https://www.osc.ny.gov/files/local-government/data/"
LANDING_URL = (
    "https://www.osc.ny.gov/local-government/data/"
    "real-property-tax-levies-taxable-full-value-and-full-value-tax-rates"
)

#: OSC label year -> original OSC path (relative to BASE). On-disk name is always
#: ``<year>_school_district_tax.<ext>`` keeping the source extension (.xls/.xlsx).
#: 2015-2018 are the "tableN" detail-only era; 2019+ are the "school-districts"
#: era that adds ORPTS muni codes, a Library Levy column, and Total rows.
REGISTRY: dict[int, str] = {
    2015: "excel/2015table3.xls",
    2016: "excel/2016table3.xls",
    2017: "excel/2017table3.xlsx",
    2018: "excel/2018table3.xlsx",
    2019: "excel/2019-school-districts.xlsx",
    2020: "excel/2020-school-districts.xlsx",
    2021: "excel/2021-school-districts.xlsx",
    2022: "excel/2022-school-districts.xlsx",
    2023: "excel/2023-school-districts.xlsx",
    2024: "excel/2024-school-districts.xlsx",
    2025: "excel/2025-school-districts.xlsx",
}

#: Supporting provenance documents (original path -> on-disk name).
DOCS: dict[str, str] = {
    "pdf/datadescription2015.pdf": "datadescription2015.pdf",  # 2015+ layout
    "pdf/datadescription.pdf": "datadescription.pdf",          # general/older layout
}

# OSC serves these fine to urllib, but present a clear UA anyway.
_UA = "Mozilla/5.0 (ccs data pipeline; +https://github.com/donboyd5/ccs)"


def _fetch(url: str, dest: Path, *, force: bool, min_bytes: int = 1000) -> tuple[int, str]:
    if dest.exists() and not force:
        return dest.stat().st_size, "skip (exists)"
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=120) as resp:
        data = resp.read()
    if len(data) < min_bytes:
        raise OSError(f"{url} returned only {len(data)} bytes (blocked/empty?)")
    dest.write_bytes(data)
    return len(data), "downloaded"


def main() -> None:
    force = "--force" in sys.argv[1:]
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"OSC school-district tax levies -> {OUT_DIR.relative_to(PROJECT_ROOT)}")

    ok = 0
    print("data workbooks:")
    for year in sorted(REGISTRY):
        rel = REGISTRY[year]
        ext = rel.rsplit(".", 1)[-1]
        dest = OUT_DIR / f"{year}_school_district_tax.{ext}"
        try:
            size, status = _fetch(BASE + rel, dest, force=force)
            print(f"  {dest.name:<34} {size:>9,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001 - report and continue
            print(f"  {year}: FAILED ({type(exc).__name__}: {exc})")

    print("provenance documents:")
    for rel, name in DOCS.items():
        dest = OUT_DIR / name
        try:
            size, status = _fetch(BASE + rel, dest, force=force)
            print(f"  {dest.name:<34} {size:>9,} bytes  {status}")
            ok += 1
        except Exception as exc:  # noqa: BLE001
            print(f"  {name}: FAILED ({type(exc).__name__}: {exc})")

    # Landing-page HTML snapshot (records exactly which links we pulled, when).
    dest = OUT_DIR / "osc_landing_page.html"
    try:
        size, status = _fetch(LANDING_URL, dest, force=force, min_bytes=5000)
        print(f"  {dest.name:<34} {size:>9,} bytes  {status}")
        ok += 1
    except Exception as exc:  # noqa: BLE001
        print(f"  landing page: FAILED ({type(exc).__name__}: {exc})")

    n_expected = len(REGISTRY) + len(DOCS) + 1
    print(f"{ok}/{n_expected} files present in {OUT_DIR.relative_to(PROJECT_ROOT)}.")


if __name__ == "__main__":
    main()
