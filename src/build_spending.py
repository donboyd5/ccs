"""Build a per-pupil spending panel for the comparison districts.

Source: NYS Office of the State Comptroller (OSC) local-government financial
data (``data/school_finance_2013_2025.parquet``), joined to NYSED K-12
enrollment. OSC collects this from school districts' Annual Update Documents
using the NY Uniform System of Accounts; the fund is the leading letter of the
account code (A = General, F = Special Aid, C = School Lunch, H = Capital,
V = Debt Service, ...).

Three expenditure measures per district-year:
  * total_all_funds    -- every expenditure row, all funds
  * general_fund       -- the General Fund (A) only (the local budget-vote fund)
  * current_operating  -- General + Special Aid + School Lunch funds, excluding
                          capital outlay, debt service and interfund transfers;
                          approximates the NCES / Census F-33 "current spending"
                          basis used for cross-district comparison (the headline)

Per-pupil columns divide each measure by K-12 enrollment. ``year_end`` is the
fiscal year ending June 30, which equals OSC's ``CALENDAR_YEAR`` and the NYSED
school-year-ending label (no off-by-one). Per-pupil values are null before
2016, where the enrollment panel does not reach.

Output (data/finance/, git-ignored, regenerable):
    spending_per_pupil.parquet   one row per comparison district-year

Usage:  python src/build_spending.py
"""

from __future__ import annotations

import sys
from pathlib import Path

import polars as pl

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "books" / "cambridge-comparisons"))
import comparisons as cc  # noqa: E402  (after sys.path tweak)

OSC_PATH = REPO_ROOT / "data" / "school_finance_2013_2025.parquet"
OUT_PATH = REPO_ROOT / "data" / "finance" / "spending_per_pupil.parquet"

#: Operating funds kept for the "current operating" measure.
OPERATING_FUNDS = ["A", "F", "C"]  # General, Special Aid, School Lunch

#: Objects of expenditure excluded from "current operating": capital outlay,
#: debt service, and transfers between funds (which would double-count).
NONCURRENT = [
    "Equipment and Capital Outlay",
    "Purchase of Buses",
    "Debt Principal",
    "Debt Interest",
    "Transfer to Debt Service Fund",
    "Interfund Transfer",
    "Transfer to Special Aid Fund",
    "Transfer to School Food Service Fund",
]


def build() -> pl.DataFrame:
    osc_to_cd = {osc: cd for cd, osc in cc.OSC_CODE.items()}

    exp = (
        pl.read_parquet(OSC_PATH)
        .filter(
            (pl.col("ACCOUNT_CODE_SECTION") == "EXPENDITURE")
            & pl.col("MUNICIPAL_CODE").is_in(list(osc_to_cd))
        )
        .with_columns(pl.col("ACCOUNT_CODE").str.extract(r"^([A-Z]+)").alias("FUND"))
    )

    measures = (
        exp.group_by("MUNICIPAL_CODE", "CALENDAR_YEAR")
        .agg(
            pl.col("AMOUNT").sum().alias("total_all_funds"),
            pl.col("AMOUNT").filter(pl.col("FUND") == "A").sum().alias("general_fund"),
            pl.col("AMOUNT")
            .filter(
                pl.col("FUND").is_in(OPERATING_FUNDS)
                & ~pl.col("OBJECT_OF_EXPENDITURE").is_in(NONCURRENT)
            )
            .sum()
            .alias("current_operating"),
        )
        .with_columns(
            pl.col("MUNICIPAL_CODE").replace_strict(osc_to_cd).alias("district_cd"),
            pl.col("CALENDAR_YEAR").cast(pl.Int32).alias("year_end"),
        )
    )

    enr = cc.load_panel().select("district_cd", "year_end", "k12_enrollment")

    panel = (
        measures.join(enr, on=["district_cd", "year_end"], how="left")
        .with_columns(
            (pl.col("total_all_funds") / pl.col("k12_enrollment")).round(0).alias("total_pp"),
            (pl.col("general_fund") / pl.col("k12_enrollment")).round(0).alias("genfund_pp"),
            (pl.col("current_operating") / pl.col("k12_enrollment")).round(0).alias("current_pp"),
        )
        .select(
            "district_cd",
            "year_end",
            "k12_enrollment",
            "total_all_funds",
            "general_fund",
            "current_operating",
            "total_pp",
            "genfund_pp",
            "current_pp",
        )
        .sort("district_cd", "year_end")
    )
    return panel


def main() -> None:
    panel = build()
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    panel.write_parquet(OUT_PATH, compression="zstd")
    print(
        f"Wrote {panel.height} district-years to {OUT_PATH.relative_to(REPO_ROOT)} "
        f"({panel['year_end'].min()}-{panel['year_end'].max()})"
    )
    print(f"  districts: {panel['district_cd'].n_unique()} of {len(cc.OSC_CODE)}")


if __name__ == "__main__":
    main()
