"""Tidy NYS state school aid into a comparison-district panel.

Reads the raw download ([`download_state_aid.py`](download_state_aid.py)) and
produces one row per comparison district × enacted fiscal year × aid category,
joined to the project's ``nysed_district_cd`` via the 6-digit ``beds_code``
(which equals ``nysed_district_cd[:6]``).

Year convention: ``year_end`` is the calendar year the enacted school year ends
("2025-26 Enacted Budget" → ``year_end`` 2026), matching the rest of the project.
``amount`` is the enacted dollar figure for that year (the source's
``school_year`` column).

The mergers chapter uses **Foundation Aid** (the RIOA base) and the two
reorganization-incentive categories; all aid categories are retained so the panel
can serve the spending/tax chapters later.

Output (data/processed/, git-ignored, regenerable):
    state_aid.parquet

Usage:  python src/build_state_aid.py   (run download_state_aid.py first)
"""

from __future__ import annotations

import re
from pathlib import Path

import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_PATH = PROJECT_ROOT / "data" / "raw" / "state_aid" / "state_aid_comparison.csv"
XWALK_PATH = PROJECT_ROOT / "data" / "crosswalks" / "district_crosswalk.csv"
OUT_PATH = PROJECT_ROOT / "data" / "processed" / "state_aid.parquet"

_YEAR_RE = re.compile(r"(\d{4})-\d{2}")

#: Aid categories the mergers / spending chapters care about most. All categories
#: are kept in the output; this just documents the ones in active use.
KEY_CATEGORIES = [
    "Foundation Aid",
    "Operating Reorganization Incentive Aid",
    "Building Reorganization Incentive Aid",
    "High Tax Aid",
    "Total Formula-Based Aids",
]


def build() -> pl.DataFrame:
    raw = (
        pl.read_csv(RAW_PATH, infer_schema_length=10000)
        # event "2025-26 Enacted Budget" -> year_end 2026 (first year + 1)
        .with_columns(
            pl.col("event").str.extract(r"(\d{4})-\d{2}", 1).cast(pl.Int64).add(1).alias("year_end")
        )
    )

    xwalk = (
        pl.read_csv(XWALK_PATH, infer_schema_length=2000,
                    schema_overrides={"nysed_district_cd": pl.Utf8, "beds_entity_cd": pl.Utf8})
        .with_columns(pl.col("beds_entity_cd").str.slice(0, 6).alias("beds6"))
        .select("beds6", "nysed_district_cd")
    )

    tidy = (
        raw
        .with_columns(
            pl.col("beds_code").cast(pl.Utf8).alias("beds6"),
            pl.col("school_year").cast(pl.Float64, strict=False).alias("amount"),
        )
        .join(xwalk, on="beds6", how="left")
        .select(
            "nysed_district_cd", "beds6", "district", "county", "year_end",
            "aid_category", "amount",
        )
        .sort("district", "year_end", "aid_category")
    )

    unmatched = tidy.filter(pl.col("nysed_district_cd").is_null()).select("beds6", "district").unique()
    if unmatched.height:
        print(f"  WARNING: {unmatched.height} state-aid beds6 not in crosswalk:")
        print(unmatched.to_string())

    return tidy.filter(pl.col("nysed_district_cd").is_not_null())


def main() -> None:
    df = build()
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    df.write_parquet(OUT_PATH, compression="zstd")
    fa = df.filter(pl.col("aid_category") == "Foundation Aid")
    print(f"Wrote {df.height:,} rows to {OUT_PATH.relative_to(PROJECT_ROOT)} "
          f"({df['district'].n_unique()} districts, {df['year_end'].min()}-"
          f"{df['year_end'].max()}, {df['aid_category'].n_unique()} categories)")
    print(f"  Foundation Aid: {fa.height} district-years; Cambridge latest = "
          f"${fa.filter(pl.col('district')=='Cambridge')['amount'].max():,.0f}")


if __name__ == "__main__":
    main()
