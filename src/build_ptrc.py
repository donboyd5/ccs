"""Stack NYSED Property Tax Report Card (PTRC) workbooks into one parquet.

Reads the raw per-year workbooks downloaded by ``download_ptrc.py`` and produces
a clean, district-linkable panel: one row per district × ``year_end`` with the
**proposed (budget-year)** figures from each report card.

Each workbook's data sheet has a stable 35-column layout (verified 2019–2027):
columns come in prior-year / budget-year **pairs**, so we take the *second* of
each pair (the year the report card is about) plus the three percent-change
columns. We select by **position**, not header text, because the headers embed
shifting year labels and stray whitespace. ``year_end`` is the second year of
NYSED's budget span (file ``2024_ptrc.xlsx`` = budget for SY 2023-24 = 2024).

Districts are identified by a 6-digit BEDS code = the first 6 digits of the
project's 12-digit ``beds_entity_cd``; we join that to the district crosswalk to
attach the stable ``nysed_district_cd``.

Output (data/processed/, git-ignored, regenerable):
    property_tax_report_card.parquet

Usage:  python src/build_ptrc.py   (run download_ptrc.py first)
"""

from __future__ import annotations

import re
from pathlib import Path

import fastexcel
import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_property_tax_report_card"
XWALK_PATH = PROJECT_ROOT / "data" / "crosswalks" / "district_crosswalk.csv"
OUT_PATH = PROJECT_ROOT / "data" / "processed" / "property_tax_report_card.parquet"

#: (column index in the 35-col sheet, output name). Indices pick the budget-year
#: value from each prior/budget pair, plus the single percent-change columns.
COLS: list[tuple[int, str]] = [
    (3, "proposed_spending"),
    (4, "spending_pct_change"),
    (6, "proposed_tax_levy_to_support_budget"),
    (8, "levy_for_library_debt"),
    (10, "levy_for_nonexcludable_propositions"),
    (12, "tax_cap_reserve_used"),
    (14, "total_proposed_tax_levy"),
    (15, "tax_levy_pct_change"),
    (17, "permissible_exclusions"),
    (19, "tax_levy_limit_wo_exclusions"),
    (21, "proposed_tax_levy_wo_exclusions"),
    (23, "levy_vs_limit_wo_exclusions"),
    (25, "projected_enrollment"),
    (26, "enrollment_pct_change"),
    (28, "fund_balance_restricted"),
    (30, "fund_balance_assigned_appropriated"),
    (32, "fund_balance_unrestricted"),
    (34, "fund_balance_unrestricted_pct_of_budget"),
]

_YEAR_RE = re.compile(r"(\d{4})")


def read_year(path: Path) -> pl.DataFrame:
    year_end = int(_YEAR_RE.search(path.stem).group(1))
    df = fastexcel.read_excel(str(path)).load_sheet(0, header_row=0).to_polars()
    if df.width != 35 or not str(df.columns[0]).strip().upper().startswith("BEDS"):
        raise ValueError(f"{path.name}: unexpected layout ({df.width} cols, "
                         f"first header {df.columns[0]!r})")

    exprs = [
        pl.nth(0).cast(pl.Utf8).str.strip_chars().str.zfill(6).alias("beds6"),
        *[pl.nth(i).cast(pl.Float64, strict=False).alias(name) for i, name in COLS],
    ]
    out = (
        df.select(exprs)
        # keep real district rows: a 6-digit numeric BEDS code with a budget figure
        .filter(pl.col("beds6").str.contains(r"^\d{6}$") & pl.col("proposed_spending").is_not_null())
        .with_columns(pl.lit(year_end, dtype=pl.Int32).alias("year_end"))
    )
    return out


def build() -> pl.DataFrame:
    files = sorted(RAW_DIR.glob("*_ptrc.xlsx"))
    if not files:
        raise FileNotFoundError(f"No PTRC workbooks in {RAW_DIR}; run download_ptrc.py first")

    frames = [read_year(f) for f in files]
    stacked = pl.concat(frames, how="vertical")

    xwalk = (
        pl.read_csv(XWALK_PATH, infer_schema_length=2000,
                    schema_overrides={"nysed_district_cd": pl.Utf8, "beds_entity_cd": pl.Utf8})
        .with_columns(pl.col("beds_entity_cd").str.slice(0, 6).alias("beds6"))
        .select("beds6", "nysed_district_cd", "district_name", "county_name")
    )

    joined = stacked.join(xwalk, on="beds6", how="left")

    # Report any rows that did not link (so nothing is silently dropped).
    unmatched = joined.filter(pl.col("nysed_district_cd").is_null())
    if unmatched.height:
        by_year = (unmatched.group_by("year_end").len().sort("year_end")
                   .to_dict(as_series=False))
        print(f"  WARNING: {unmatched.height} PTRC rows did not link to a district:")
        for ye, n in zip(by_year["year_end"], by_year["len"]):
            names = (unmatched.filter(pl.col("year_end") == ye)
                     .select("beds6").to_series().to_list())
            print(f"    {ye}: {n}  beds6={names}")

    panel = (
        joined.filter(pl.col("nysed_district_cd").is_not_null())
        .select(
            "nysed_district_cd",
            pl.col("beds6").alias("beds_code"),
            "district_name",
            "county_name",
            "year_end",
            *[name for _, name in COLS],
        )
        .sort("nysed_district_cd", "year_end")
    )
    return panel


def main() -> None:
    panel = build()
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    panel.write_parquet(OUT_PATH, compression="zstd")
    print(
        f"Wrote {panel.height:,} district-years to "
        f"{OUT_PATH.relative_to(PROJECT_ROOT)} "
        f"({panel['year_end'].min()}-{panel['year_end'].max()}, "
        f"{panel['nysed_district_cd'].n_unique()} districts)"
    )


if __name__ == "__main__":
    main()
