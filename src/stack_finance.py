"""Stack NY school district finance CSVs (2013-2025) into a single parquet file.

The annual files in ``data/raw/osc_school_finance/`` cover every NY school district. Years 2013-2025
share a single schema (PERIOD_START/PERIOD_END/ACCOUNT_CODE_SECTION), so they
can be concatenated without harmonization. 2026 (a near-empty placeholder) and
the 1995-2012 files (a different, older schema) are intentionally excluded.

Usage:
    python src/stack_finance.py
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

# --- configuration ---------------------------------------------------------
PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
ACCOUNT_CODES_DIR = DATA_DIR / "raw" / "osc_school_finance"
OUT_PATH = DATA_DIR / "processed" / "school_finance.parquet"

YEARS = range(2013, 2026)  # 2013 through 2025 inclusive

# Codes are identifiers, not numbers: read as strings to preserve any leading
# zeros and avoid integer overflow surprises.
SCHEMA_OVERRIDES = {
    "MUNICIPAL_CODE": pl.String,
    "ACCOUNT_CODE": pl.String,
    "AMOUNT": pl.Float64,
}


def stack() -> pl.DataFrame:
    frames: list[pl.DataFrame] = []
    for year in YEARS:
        path = ACCOUNT_CODES_DIR / f"{year}_SchoolDistrict.csv"
        if not path.exists():
            raise FileNotFoundError(f"Expected file not found: {path}")
        df = pl.read_csv(path, schema_overrides=SCHEMA_OVERRIDES)
        # Provenance: tie every row back to its source year/file.
        df = df.with_columns(
            pl.lit(year, dtype=pl.Int32).alias("SOURCE_YEAR"),
            pl.lit(path.name).alias("SOURCE_FILE"),
        )
        print(f"  {path.name}: {df.height:>9,} rows, {df.width} cols")
        frames.append(df)

    # vertical_relaxed tolerates minor dtype drift across years.
    return pl.concat(frames, how="vertical_relaxed")


def main() -> None:
    print(f"Stacking finance CSVs for {YEARS.start}-{YEARS.stop - 1} ...")
    combined = stack()
    print(f"Combined: {combined.height:,} rows, {combined.width} cols")

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    combined.write_parquet(OUT_PATH, compression="zstd")
    size_mb = OUT_PATH.stat().st_size / 1e6
    print(f"Wrote {OUT_PATH.relative_to(PROJECT_ROOT)}  ({size_mb:.1f} MB)")


if __name__ == "__main__":
    main()
