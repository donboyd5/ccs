"""Cache NCES Common Core of Data (CCD) pupil-teacher data for NY districts.

The federal CCD reports **teacher full-time-equivalents (FTE)** and total
enrollment by school district (LEA). Dividing the two gives the standard,
nationally comparable pupil-teacher ratio -- a useful cross-check on the
NYSED headcount-based students-per-teacher ratio built by
``build_enrollment_teachers.py`` (which has no FTE).

Source: Urban Institute Education Data Portal (https://educationdata.urban.org),
which mirrors NCES CCD. We pull the LEA "directory" endpoint for New York
(fips=36) for each available year and keep enrollment + teacher FTE.

Year convention: CCD labels a school year by its **fall** term, so CCD year Y is
the Y/(Y+1) school year. We store ``year_end = ccd_year + 1`` to match the rest
of this project (NYSED school-year-ending), verified against the COVID
enrollment trough (CCD 2020 == 2020-21 == year_end 2021).

Output (data/enrollment_staff/, git-ignored and regenerable):
    ccd_pupil_teacher_ny.parquet   one row per LEA-year, all NY districts

Usage:  python src/build_ccd_pupil_teacher.py   (requires internet)
"""

from __future__ import annotations

import json
import urllib.request
from pathlib import Path

import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
OUT_PATH = PROJECT_ROOT / "data" / "enrollment_staff" / "ccd_pupil_teacher_ny.parquet"

# CCD fall-years to pull. 2015-2024 maps to year_end 2016-2025.
CCD_YEARS = range(2015, 2025)
API = "https://educationdata.urban.org/api/v1/school-districts/ccd/directory/{year}/?fips=36"


def fetch_year(ccd_year: int) -> list[dict]:
    url = API.format(year=ccd_year)
    with urllib.request.urlopen(url, timeout=60) as resp:
        return json.load(resp)["results"]


def build() -> pl.DataFrame:
    records: list[dict] = []
    for ccd_year in CCD_YEARS:
        try:
            rows = fetch_year(ccd_year)
        except Exception as exc:  # noqa: BLE001 - report and skip an unavailable year
            print(f"  CCD {ccd_year}: unavailable ({type(exc).__name__})")
            continue
        for r in rows:
            records.append(
                {
                    "leaid": r.get("leaid"),
                    "lea_name": r.get("lea_name"),
                    "county_name": r.get("county_name"),
                    "ccd_year": ccd_year,
                    "year_end": ccd_year + 1,  # fall-year -> school-year-ending
                    "enrollment": r.get("enrollment"),
                    "teachers_total_fte": r.get("teachers_total_fte"),
                }
            )
        print(f"  CCD {ccd_year} (year_end {ccd_year + 1}): {len(rows):>5,} LEAs")

    df = pl.DataFrame(records).with_columns(
        pl.when((pl.col("teachers_total_fte") > 0) & pl.col("enrollment").is_not_null())
        .then(pl.col("enrollment") / pl.col("teachers_total_fte"))
        .otherwise(None)
        .round(2)
        .alias("ccd_students_per_teacher")
    )
    return df


def main() -> None:
    print("Pulling NCES CCD (via Urban Institute) for New York ...")
    df = build()
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.write_parquet(OUT_PATH, compression="zstd")
    print(
        f"\nWrote {df.height:,} LEA-years to {OUT_PATH.relative_to(PROJECT_ROOT)} "
        f"(year_end {df['year_end'].min()}-{df['year_end'].max()})"
    )


if __name__ == "__main__":
    main()
