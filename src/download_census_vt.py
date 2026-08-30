"""Download Vermont county (Bennington, Rutland) population, 1970–2024.

The Cambridge comparisons book's demographics chapter plots regional population
history. New York county history comes from the sibling ``popfc`` forecast
project (its reconciled Census-PEP / NYSDOL series); Vermont counties are not in
popfc (it is NYS-only), so this script pulls the two neighboring Vermont
counties — **Bennington (50003)** and **Rutland (50021)** — from the U.S. Census
Bureau's Population Estimates Program (PEP) and its legacy files.

No single file spans 1970–2024, so we stitch four sources:

  - **1970–1999:** NBER's consolidated county intercensal file
    (``county_population.csv``), which merges the Census Bureau's legacy 1970s /
    1980s / 1990s county-intercensal releases into one CSV. The legacy releases
    themselves are awkward (fixed-width 1970s totals; 1980s annual county totals
    only inside a 34 MB age-sex-race file; 1990s totals published as PDF), and
    the machine-readable pieces are consistent with it. Values are intercensal
    estimates; **census-year columns (1970, 1980, 1990) are April-1 census
    counts, other years are July-1** — the legacy convention. 1970s values are
    rounded to hundreds, as published.
  - **2000–2009:** Census PEP 2000s intercensal — the
    ``2000/pep/int_population`` API endpoint (``DATE_`` 2–11 = 7/1/2000…7/1/2009).
    **Requires a Census API key** in the ``CENSUS_API_KEY`` env var (Census now
    rejects keyless API requests). If the key is absent, these years are skipped
    with a warning.
  - **2010–2019:** the key-free bulk file ``co-est2019-alldata.csv``
    (the 2010s vintage; ``POPESTIMATE2010…2019``).
  - **2020–2024:** the key-free bulk file ``co-est2024-alldata.csv``
    (the 2020s vintage, 2020-census-based; ``POPESTIMATE2020…2024``).

All years after 1999 are **July-1 resident population** (matching the July-1
basis of popfc's NY series, so the two are comparable in one indexed chart).
The 1999→2000 step embeds the 2000-census rebenchmark (a visible level shift,
not an error). The bulk/NBER files are large (all U.S. counties); we cache them
under ``data/raw/census_pep/_cache/`` (git-ignored) and extract only the two VT
counties. Output is a small long-format CSV:

    state_fips, county_fips, geoid, geography, year, population, source

where ``source`` records which file each row came from. Provenance is
documented in ``data/raw/census_pep/SOURCE.md``.

Usage:  CENSUS_API_KEY=...  python src/download_census_vt.py   [--force]
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import os
import urllib.request
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = PROJECT_ROOT / "data" / "raw" / "census_pep"
CACHE_DIR = OUT_DIR / "_cache"
OUT_PATH = OUT_DIR / "vt_county_population.csv"

#: Census now 302-redirects keyless API requests to a "missing key" page.
API_KEY = os.environ.get("CENSUS_API_KEY")

# Present a browser-like agent; some Census paths decline the urllib default.
_UA = ("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) "
       "Chrome/124.0 Safari/537.36")

#: The two Vermont counties adjacent to Washington County, NY.
VT_COUNTIES: dict[str, str] = {
    "003": "Bennington County",
    "021": "Rutland County",
}
STATE_FIPS = "50"  # Vermont

#: Key-free bulk PEP county-totals files (all U.S. counties).
BULK_2019 = (
    "https://www2.census.gov/programs-surveys/popest/datasets/"
    "2010-2019/counties/totals/co-est2019-alldata.csv"
)
BULK_2024 = (
    "https://www2.census.gov/programs-surveys/popest/datasets/"
    "2020-2024/counties/totals/co-est2024-alldata.csv"
)

#: NBER's consolidation of the Census Bureau's legacy 1970s/80s/90s county
#: intercensal releases (see module docstring for why not the legacy files).
NBER_POP = "https://data.nber.org/census/population/popest/county_population.csv"

#: Census PEP API (2000s intercensal); needs a key.
API_2000 = "https://api.census.gov/data/2000/pep/int_population"


def _download(url: str, dest: Path, *, force: bool) -> Path:
    """Download ``url`` to ``dest`` (skip if cached, unless ``force``)."""
    if dest.exists() and not force:
        print(f"  cached {dest.name}")
        return dest
    dest.parent.mkdir(parents=True, exist_ok=True)
    print(f"  fetching {url}")
    req = urllib.request.Request(url, headers={"User-Agent": _UA})
    with urllib.request.urlopen(req, timeout=180) as resp, open(dest, "wb") as out:
        out.write(resp.read())
    return dest


def _read_csv_latin1(path: Path) -> csv.DictReader:
    """Census bulk files are latin-1 encoded (curly apostrophes in CTYNAME)."""
    with open(path, encoding="latin-1", newline="") as f:
        return list(csv.DictReader(f))


def fetch_bulk_2010_2024(*, force: bool) -> list[dict]:
    """Bennington + Rutland July-1 population, 2010–2024, from the two bulk
    PEP files (2010–2019 from the 2019 vintage; 2020–2024 from the 2024
    vintage)."""
    rows: list[dict] = []
    # 2010–2019
    f19 = _download(BULK_2019, CACHE_DIR / "co-est2019-alldata.csv", force=force)
    for r in _read_csv_latin1(f19):
        if r["STATE"] == STATE_FIPS and r["COUNTY"] in VT_COUNTIES:
            name = VT_COUNTIES[r["COUNTY"]]
            for yr in range(2010, 2020):
                rows.append({
                    "state_fips": STATE_FIPS, "county_fips": r["COUNTY"],
                    "geoid": f"{STATE_FIPS}{r['COUNTY']}", "geography": name,
                    "year": yr, "population": int(r[f"POPESTIMATE{yr}"]),
                    "source": "census_pep_co-est2019-alldata",
                })
    # 2020–2024 (2020-census-based 2024 vintage)
    f24 = _download(BULK_2024, CACHE_DIR / "co-est2024-alldata.csv", force=force)
    for r in _read_csv_latin1(f24):
        if r["STATE"] == STATE_FIPS and r["COUNTY"] in VT_COUNTIES:
            name = VT_COUNTIES[r["COUNTY"]]
            for yr in range(2020, 2025):
                rows.append({
                    "state_fips": STATE_FIPS, "county_fips": r["COUNTY"],
                    "geoid": f"{STATE_FIPS}{r['COUNTY']}", "geography": name,
                    "year": yr, "population": int(r[f"POPESTIMATE{yr}"]),
                    "source": "census_pep_co-est2024-alldata",
                })
    return rows


def fetch_nber_1970_1999(*, force: bool) -> list[dict]:
    """Bennington + Rutland population, 1970–1999, from NBER's consolidation of
    the Census Bureau's legacy county-intercensal releases.

    Census-year columns (1970, 1980, 1990) are April-1 census counts; other
    years are July-1 intercensal estimates (the legacy convention). The file
    carries two rows per county (a combined-state and a plain-county row); the
    plain one (``fipsco`` == the 3-digit county code) is the one populated for
    these decades.
    """
    path = _download(NBER_POP, CACHE_DIR / "nber_county_population.csv", force=force)
    with open(path, encoding="latin-1", newline="") as f:
        rows: list[dict] = []
        for r in csv.DictReader(f):
            cf = r["county_fips"].zfill(3)   # NBER stores "3"/"21" unpadded
            if (r["state_fips"] == STATE_FIPS
                    and cf in VT_COUNTIES
                    and r["fipsco"] == cf):
                name = VT_COUNTIES[cf]
                for yr in range(1970, 2000):
                    rows.append({
                        "state_fips": STATE_FIPS,
                        "county_fips": cf,
                        "geoid": f"{STATE_FIPS}{cf}",
                        "geography": name,
                        "year": yr, "population": int(r[f"pop{yr}"]),
                        "source": "nber_census_intercensal_1970_1999",
                    })
        return rows


def fetch_api_2000_2009(*, force: bool) -> list[dict]:
    """Bennington + Rutland July-1 population, 2000–2009, from the Census PEP
    2000s intercensal API (``DATE_`` 2 = 7/1/2000 … 11 = 7/1/2009)."""
    if not API_KEY:
        print("  WARNING: CENSUS_API_KEY not set; skipping 2000–2009 "
              "(the Census API now requires a key). Output will cover 2010–2020 only.")
        return []
    cache = CACHE_DIR / "int_population_2000.json"
    if cache.exists() and not force:
        print(f"  cached {cache.name}")
        with open(cache, encoding="utf-8") as f:
            data = json.load(f)
    else:
        url = (f"{API_2000}?get=POP,DATE_,DATE_DESC"
               f"&for=county:{','.join(VT_COUNTIES)}&in=state:{STATE_FIPS}&key={API_KEY}")
        print(f"  fetching {API_2000}")
        req = urllib.request.Request(url, headers={"User-Agent": _UA})
        with urllib.request.urlopen(req, timeout=120) as resp:
            data = json.load(resp)
        cache.parent.mkdir(parents=True, exist_ok=True)
        with open(cache, "w", encoding="utf-8") as f:
            json.dump(data, f)
    header = data[0]
    ix = {k: i for i, k in enumerate(header)}  # POP, DATE_, DATE_DESC, state, county
    # DATE_ 2 = 7/1/2000, 3 = 7/1/2001, ... 11 = 7/1/2009  (1 = 4/1/2000 base)
    date_to_year = {d: 1998 + d for d in range(2, 12)}  # 2->2000 ... 11->2009
    rows: list[dict] = []
    for r in data[1:]:
        date_ = int(r[ix["DATE_"]])
        if date_ not in date_to_year:
            continue
        cfips = r[ix["county"]]
        rows.append({
            "state_fips": STATE_FIPS, "county_fips": cfips,
            "geoid": f"{STATE_FIPS}{cfips}", "geography": VT_COUNTIES[cfips],
            "year": date_to_year[date_], "population": int(r[ix["POP"]]),
            "source": "census_pep_int_population_2000s",
        })
    return rows


def build(*, force: bool) -> list[dict]:
    rows = (
        fetch_nber_1970_1999(force=force)
        + fetch_api_2000_2009(force=force)
        + fetch_bulk_2010_2024(force=force)
    )
    rows.sort(key=lambda r: (r["geoid"], r["year"]))
    return rows


def main(*, force: bool) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    rows = build(force=force)
    with open(OUT_PATH, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(
            f,
            fieldnames=["state_fips", "county_fips", "geoid", "geography",
                        "year", "population", "source"],
        )
        w.writeheader()
        w.writerows(rows)
    years = sorted({r["year"] for r in rows})
    counties = sorted({r["geography"] for r in rows})
    print(f"Wrote {len(rows)} rows to {OUT_PATH.relative_to(PROJECT_ROOT)}")
    print(f"  counties: {counties}")
    print(f"  years: {min(years)}–{max(years)} ({len(years)} years)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--force", action="store_true", help="re-download even if cached")
    args = parser.parse_args()
    main(force=args.force)
