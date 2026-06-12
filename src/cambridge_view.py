"""Build a Cambridge-only view from the stacked statewide finance parquet.

Filters the full 2013-2025 stack down to Cambridge Central School District
(municipal code 530790601000) and writes a small, focused parquet for the
ccs project. Run ``stack_finance.py`` first.

Usage:
    python src/cambridge_view.py
"""

from __future__ import annotations

from pathlib import Path

import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = PROJECT_ROOT / "data"
SRC_PATH = DATA_DIR / "processed" / "school_finance.parquet"
OUT_PATH = DATA_DIR / "processed" / "cambridge_finance.parquet"

CAMBRIDGE_CODE = "530790601000"


def main() -> None:
    if not SRC_PATH.exists():
        raise FileNotFoundError(f"Run stack_finance.py first; missing {SRC_PATH}")

    camb = (
        pl.scan_parquet(SRC_PATH)
        .filter(pl.col("MUNICIPAL_CODE") == CAMBRIDGE_CODE)
        .sort("CALENDAR_YEAR", "ACCOUNT_CODE")
        .collect()
    )
    camb.write_parquet(OUT_PATH, compression="zstd")
    print(f"Wrote {OUT_PATH.relative_to(PROJECT_ROOT)}: {camb.height:,} rows")

    # --- quick view -------------------------------------------------------
    with pl.Config(tbl_rows=20, tbl_cols=-1, fmt_str_lengths=40):
        print("\nRows per year:")
        print(camb.group_by("CALENDAR_YEAR").len().sort("CALENDAR_YEAR"))

        print("\nTotal AMOUNT by year and section (revenue vs expenditure):")
        print(
            camb.group_by("CALENDAR_YEAR", "ACCOUNT_CODE_SECTION")
            .agg(pl.col("AMOUNT").sum().alias("TOTAL"))
            .sort("CALENDAR_YEAR", "ACCOUNT_CODE_SECTION")
        )

        print("\nSample rows (2025):")
        print(
            camb.filter(pl.col("CALENDAR_YEAR") == 2025)
            .select(
                "ACCOUNT_CODE",
                "ACCOUNT_CODE_NARRATIVE",
                "ACCOUNT_CODE_SECTION",
                "LEVEL_1_CATEGORY",
                "AMOUNT",
            )
            .head(10)
        )


if __name__ == "__main__":
    main()
