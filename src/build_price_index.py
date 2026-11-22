"""Build the school-year price index used to put dollars in real terms.

Reads the monthly CPI-U (``data/raw/bls_cpi/cpi_u_monthly.csv``, from
``download_cpi.py``) and averages it over each **school year, July through
June**, so the index lines up with the fiscal year that school finance, tax and
budget data are reported for. ``year_end`` is the school year ending: 2025 is
July 2024 through June 2025.

A single missing month is filled by straight-line interpolation between the
months on either side and counted in ``months_interpolated`` (October 2025, in
the school year ending 2026, is the only one). A school year is written only if
all twelve months are then present, so the current, unfinished year is left out.

To change the price index, change the two scripts; nothing downstream names
CPI-U. Chapters call ``comparisons.to_real()``.

Output (data/processed/, git-ignored, regenerable):
    price_index_school_year.parquet
        year_end, index_name, index_value, months_interpolated

Usage:  python src/build_price_index.py
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

REPO_ROOT = Path(__file__).resolve().parents[1]
RAW_PATH = REPO_ROOT / "data" / "raw" / "bls_cpi" / "cpi_u_monthly.csv"
OUT_PATH = REPO_ROOT / "data" / "processed" / "price_index_school_year.parquet"

INDEX_NAME = "CPI-U, All items, U.S. city average, not seasonally adjusted"


def build() -> pl.DataFrame:
    monthly = (
        pl.read_csv(RAW_PATH, schema_overrides={"value": pl.Float64, "note": pl.String})
        .sort("year", "month")
        .with_columns(
            was_missing=pl.col("value").is_null(),
            # the school year a month belongs to: July-December count toward next year
            year_end=pl.when(pl.col("month") >= 7)
            .then(pl.col("year") + 1)
            .otherwise(pl.col("year")),
        )
    )
    # Only isolated gaps are filled: both neighbours must have a published value.
    fillable = (
        pl.col("was_missing")
        & pl.col("value").shift(1).is_not_null()
        & pl.col("value").shift(-1).is_not_null()
    )
    monthly = monthly.with_columns(
        value=pl.when(fillable)
        .then((pl.col("value").shift(1) + pl.col("value").shift(-1)) / 2)
        .otherwise(pl.col("value")),
        interpolated=fillable,
    )
    return (
        monthly.group_by("year_end")
        .agg(
            index_value=pl.col("value").mean(),
            months=pl.col("value").count(),
            months_interpolated=pl.col("interpolated").sum(),
        )
        .filter(pl.col("months") == 12)
        .with_columns(
            pl.col("year_end").cast(pl.Int32),
            pl.lit(INDEX_NAME).alias("index_name"),
            pl.col("index_value").round(3),
        )
        .select("year_end", "index_name", "index_value", "months_interpolated")
        .sort("year_end")
    )


def main() -> None:
    out = build()
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    out.write_parquet(OUT_PATH, compression="zstd")
    print(
        f"Wrote {out.height} school years to {OUT_PATH.relative_to(REPO_ROOT)} "
        f"({out['year_end'].min()}-{out['year_end'].max()})"
    )
    filled = out.filter(pl.col("months_interpolated") > 0)
    for row in filled.iter_rows(named=True):
        print(f"  {row['year_end']}: {row['months_interpolated']} month(s) interpolated")
    print(out.tail(4))


if __name__ == "__main__":
    main()
