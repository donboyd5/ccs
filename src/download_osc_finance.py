"""Download the NYS Comptroller's school-district financial data (all years).

OSC publishes local-government financial data ("Level 3" detail: one row per
entity x account code x year) on its *Financial Data for Local Governments* page:

    https://wwe1.osc.state.ny.us/localgov/findata/financial-data-for-local-governments.cfm

For school districts the page offers a single bulk archive holding one CSV per
fiscal year, 1995 onward:

    https://wwe1.osc.state.ny.us/localgov/findata/level3zip/schooldistrict_all_years.zip

This script downloads that archive to ``data/raw/osc_school_finance/`` and
extracts the yearly CSVs next to it under OSC's own member names
(``<year>_SchoolDistrict.csv``). Every refresh of the archive is a new OSC
**snapshot** (each row carries a ``SNAPSHOT_DATE``), and OSC revises recent
years between snapshots, so a refresh replaces **all** of the yearly files, not
just the newest one. Both the archive and the CSVs are git-ignored.

Usage:
    python src/download_osc_finance.py            # download + extract
    python src/download_osc_finance.py --keep     # extract from the archive
                                                  # already on disk (no download)
"""

from __future__ import annotations

import re
import sys
import urllib.request
import zipfile
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "osc_school_finance"

URL = (
    "https://wwe1.osc.state.ny.us/localgov/findata/level3zip/"
    "schooldistrict_all_years.zip"
)
ZIP_PATH = OUT_DIR / "schooldistrict_all_years.zip"

#: Only members with exactly this shape are extracted (and only by base name),
#: so a malformed archive cannot write outside OUT_DIR.
MEMBER_RE = re.compile(r"^\d{4}_SchoolDistrict\.csv$")

_UA = "Mozilla/5.0 (ccs data pipeline)"


def download() -> None:
    print(f"fetching {URL}")
    req = urllib.request.Request(URL, headers={"User-Agent": _UA})
    tmp = ZIP_PATH.with_suffix(".zip.part")
    with urllib.request.urlopen(req, timeout=600) as resp, open(tmp, "wb") as out:
        while chunk := resp.read(1 << 20):
            out.write(chunk)
    if tmp.stat().st_size < 10_000_000:
        raise OSError(f"archive is only {tmp.stat().st_size:,} bytes (blocked/empty?)")
    tmp.replace(ZIP_PATH)
    print(f"  saved {ZIP_PATH.relative_to(PROJECT_ROOT)} ({ZIP_PATH.stat().st_size:,} bytes)")


def extract() -> list[str]:
    done: list[str] = []
    with zipfile.ZipFile(ZIP_PATH) as zf:
        for info in sorted(zf.infolist(), key=lambda i: i.filename):
            name = Path(info.filename).name
            if not MEMBER_RE.match(name):
                print(f"  skipped unexpected member: {info.filename!r}")
                continue
            with zf.open(info) as src, open(OUT_DIR / name, "wb") as dst:
                while chunk := src.read(1 << 20):
                    dst.write(chunk)
            done.append(name)
    return done


def snapshot_date(csv_path: Path) -> str:
    """SNAPSHOT_DATE of the first data row (it is constant within a file)."""
    with open(csv_path, encoding="utf-8", errors="replace") as f:
        header = f.readline().rstrip("\n").split(",")
        first = f.readline().rstrip("\n")
    if "SNAPSHOT_DATE" not in header or not first:
        return "n/a"
    return first.rsplit(",", 1)[-1]


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    if "--keep" not in sys.argv[1:]:
        download()
    elif not ZIP_PATH.exists():
        raise FileNotFoundError(f"--keep given but {ZIP_PATH} is not on disk")
    names = extract()
    print(f"extracted {len(names)} yearly files ({names[0][:4]}-{names[-1][:4]})")
    newest = OUT_DIR / names[-1]
    print(f"  newest file {newest.name}: snapshot {snapshot_date(newest)}, "
          f"{newest.stat().st_size:,} bytes")


if __name__ == "__main__":
    main()
