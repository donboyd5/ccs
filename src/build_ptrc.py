"""Stack NYSED Property Tax Report Card (PTRC) workbooks into one parquet.

Reads the raw per-year workbooks downloaded by ``download_ptrc.py`` and produces
a clean, district-linkable panel: one row per district × ``year_end`` with the
**proposed (budget-year)** figures from each report card.

The modern workbooks share a stable 35-column layout (verified for year_end
2016–2027): columns come in prior-year / budget-year **pairs**, so we take the
*second* of each pair (the year the report card is about) plus the three
percent-change columns. We select by **position**, not header text, because the
headers embed shifting year labels and stray whitespace. ``year_end`` is the
second year of NYSED's budget span (file ``2024_ptrc.xlsx`` = budget for SY
2023-24 = 2024).

SY 2014-15 (year_end 2015) uses an earlier 29-column layout and has a dedicated
reader (``read_2015`` / ``COLS_2015``); 4 measures it lacks come through null.
Still-earlier workbooks (year_end ≤ 2014: narrower pre-tax-cap schemas, and the
2007-08…2012-13 ``.xls`` files crash the calamine reader) are **skipped with a
logged reason**, not silently dropped — see SOURCE.md.

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

#: SY 2014-15 (year_end 2015) predates the modern 35-column layout: it has 29
#: columns, the header on the 4th sheet row, and NO breakout of the levy into
#: "to support budget" / library-debt / nonexcludable-proposition / tax-cap-reserve
#: parts (only a single "Total Proposed Tax Levy"). We map the 14 measures that DO
#: exist by position — the budget-year value is the *second* of each prior/budget
#: pair, proven by the identity ``levy_vs_limit == proposed_levy_wo - levy_limit_wo``
#: holding for every district — and leave the 4 absent measures null. (column index
#: in the 29-col sheet, output name.)
COLS_2015: list[tuple[int, str]] = [
    (5, "proposed_spending"),
    (6, "spending_pct_change"),
    (8, "total_proposed_tax_levy"),
    (9, "tax_levy_pct_change"),
    (11, "permissible_exclusions"),
    (13, "proposed_tax_levy_wo_exclusions"),
    (15, "tax_levy_limit_wo_exclusions"),
    (17, "levy_vs_limit_wo_exclusions"),
    (19, "projected_enrollment"),
    (20, "enrollment_pct_change"),
    (22, "fund_balance_restricted"),
    (24, "fund_balance_assigned_appropriated"),
    (27, "fund_balance_unrestricted"),
    (28, "fund_balance_unrestricted_pct_of_budget"),
]

_YEAR_RE = re.compile(r"(\d{4})")


#: Header text of the first column across the conforming years. NYSED has used
#: "BEDS Code", "BEDSCODE" (2016-17/2017-18) and "SED CODE" (2015-16); all are the
#: 6-digit district code column. Used only as a sanity guard — values are read by
#: position, and rows are filtered to genuine 6-digit codes downstream.
_CODE_HEADERS = ("BEDS", "SED CODE")


def _assemble(df: pl.DataFrame, cols: list[tuple[int, str]], year_end: int) -> pl.DataFrame:
    """Select the budget-year columns named in ``cols`` by position, fill any
    canonical measure not in ``cols`` with null, and keep genuine district rows.
    Output schema is always: ``beds6``, the 18 canonical measures (in ``COLS``
    order), ``year_end`` — so frames from different layouts concat cleanly.
    """
    have = {name: idx for idx, name in cols}
    measures = [
        (pl.nth(have[name]).cast(pl.Float64, strict=False).alias(name)
         if name in have else pl.lit(None, dtype=pl.Float64).alias(name))
        for _, name in COLS
    ]
    return (
        df.select(
            pl.nth(0).cast(pl.Utf8).str.strip_chars().str.zfill(6).alias("beds6"),
            *measures,
        )
        # keep real district rows: a 6-digit numeric BEDS code with a budget figure
        .filter(pl.col("beds6").str.contains(r"^\d{6}$") & pl.col("proposed_spending").is_not_null())
        .with_columns(pl.lit(year_end, dtype=pl.Int32).alias("year_end"))
    )


def read_year(path: Path) -> pl.DataFrame | None:
    """Parse one modern-layout workbook into the panel, or return ``None`` if its
    layout does not match the 35-column schema (logged and skipped by ``build``).

    The 35-column layout covers year_end 2016 onward. SY 2014-15 (year_end 2015) has
    its own reader (``read_2015``); still earlier NYSED files use narrower layouts
    (and the 2007-08…2012-13 ``.xls`` files crash the calamine reader) and are left to
    per-year parsing if ever needed — see this folder's SOURCE.md.
    """
    year_end = int(_YEAR_RE.search(path.stem).group(1))
    df = fastexcel.read_excel(str(path)).load_sheet(0, header_row=0).to_polars()
    head0 = str(df.columns[0]).strip().upper()
    if df.width != 35 or not head0.startswith(_CODE_HEADERS):
        print(f"  SKIP {path.name}: non-conforming layout "
              f"({df.width} cols, first header {df.columns[0]!r})")
        return None
    return _assemble(df, COLS, year_end)


def read_2015(path: Path) -> pl.DataFrame | None:
    """Parse the SY 2014-15 (year_end 2015) workbook: a 29-column pre-breakout
    layout with the header on the 4th row. The BEDS-code row filter in ``_assemble``
    discards the title/header rows automatically; 4 measures absent from this layout
    (levy-to-support-budget, library-debt levy, nonexcludable-proposition levy,
    tax-cap-reserve used) come through as null. See ``COLS_2015``.
    """
    year_end = int(_YEAR_RE.search(path.stem).group(1))
    df = fastexcel.read_excel(str(path)).load_sheet(0, header_row=None).to_polars()
    if df.width != 29:
        print(f"  SKIP {path.name}: expected 29-col 2014-15 layout, got {df.width}")
        return None
    return _assemble(df, COLS_2015, year_end)


#: year_end -> dedicated reader for layouts that differ from the modern 35-column
#: schema. Everything not listed falls back to ``read_year``.
SPECIAL_READERS = {2015: read_2015}


def build() -> pl.DataFrame:
    files = sorted(RAW_DIR.glob("*_ptrc.xlsx"))
    if not files:
        raise FileNotFoundError(f"No PTRC workbooks in {RAW_DIR}; run download_ptrc.py first")

    frames = []
    for f in files:
        year_end = int(_YEAR_RE.search(f.stem).group(1))
        reader = SPECIAL_READERS.get(year_end, read_year)
        if (fr := reader(f)) is not None:
            frames.append(fr)
    if not frames:
        raise RuntimeError("No PTRC workbook matched a known layout")
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
