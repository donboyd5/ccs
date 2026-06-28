"""Build the demographics datasets for the Cambridge comparisons book.

Two small processed parquets feed the demographics chapter:

1. ``demographics_county_pop.parquet`` — July-1 resident population for the
   comparison counties, history + (NY) forecast:
     - **NY** (Washington, Warren, Saratoga, Rensselaer): ``popfc``'s reconciled
       Census-PEP/NYSDOL history (2000–2024) spliced to its baseline cohort-
       component forecast (2025–2050).
     - **VT** (Bennington, Rutland): Census PEP history only (2000–2020); no
       forecast (popfc is NYS-only).
   One row per county-year: ``geoid, geography, state, year, period
   (history/forecast), population``.

2. ``demographics_washington_components.parquet`` — Washington County annual
   components of change, 2020–2035 (rows = year, plus the component columns the
   chapter's table pivots on): ``births, deaths, domestic_mig, international_mig,
   net_mig, pop_change``. Per the user's choice (option a), the **domestic vs
   international migration split is shown 2020–2025** (Census PEP v2025 vintage,
   from ``washington_components.csv``); for **2026–2035** (forecast) only total
   ``net_mig`` is projected, so the two split columns are null there.

New York data is read **in place** from the sibling ``popfc`` project (its data
files are git-ignored here); set ``POPFC_DIR`` to override the default path. See
``data/raw/popfc/SOURCE.md`` and ``data/raw/census_pep/SOURCE.md``.

Usage:  python src/build_demographics.py
"""

from __future__ import annotations

import os
from pathlib import Path

import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
PROCESSED_DIR = PROJECT_ROOT / "data" / "processed"
VT_PATH = PROJECT_ROOT / "data" / "raw" / "census_pep" / "vt_county_population.csv"

#: The sibling popfc project (NYS-only forecasts). Override with POPFC_DIR.
POPFC_DIR = Path(os.environ.get(
    "POPFC_DIR", str(Path.home() / "Documents" / "python_projects" / "popfc")
))
POPFC_RECONCILED = POPFC_DIR / "data_interim" / "population_reconciled.parquet"
POPFC_FORECAST = POPFC_DIR / "data_final" / "county_forecast_totals.csv"
POPFC_YEARLY = POPFC_DIR / "data_final" / "county_yearly_components.csv"
POPFC_WASH = POPFC_DIR / "data_final" / "washington_components.csv"

#: The four NY comparison counties (Cambridge's county + neighbors).
NY_COUNTIES: dict[str, str] = {
    "36115": "Washington",   # Cambridge CSD is here — the focus county
    "36113": "Warren",
    "36091": "Saratoga",
    "36083": "Rensselaer",
}

OUT_POP = PROCESSED_DIR / "demographics_county_pop.parquet"
OUT_COMP = PROCESSED_DIR / "demographics_washington_components.parquet"


def _ny_population() -> pl.DataFrame:
    """NY county population: reconciled history (2000–2024) spliced to the
    baseline forecast (2025–2050).

    History comes from ``population_reconciled`` (Census PEP v2025 / NYSDOL, all
    four counties through 2024); the forecast comes from
    ``county_forecast_totals`` (the only all-county forecast file — note
    ``county_yearly_components`` is Washington-only). The forecast launches from
    2024, so we take history through 2024 and forecast from 2025; the small seam
    (reconciled 2024 vs the forecast's 2024 anchor differ by ~17 people) is
    invisible on an indexed chart.
    """
    hist = (
        pl.read_parquet(POPFC_RECONCILED)
        .filter(pl.col("geoid").is_in(NY_COUNTIES))
        .filter(pl.col("year") <= 2024)           # reconciled contributes 2000–2024
        .select(
            pl.col("geoid"),
            pl.col("geoid").replace_strict(NY_COUNTIES).alias("geography"),
            pl.lit("NY").alias("state"),
            pl.col("year"),
            pl.lit("history").alias("period"),
            pl.col("population").cast(pl.Float64),
        )
    )
    fc = (
        pl.read_csv(POPFC_FORECAST, schema_overrides={"geoid": pl.Utf8})
        .filter(pl.col("geoid").is_in(NY_COUNTIES))
        .filter(pl.col("scenario") == "baseline")
        .filter(pl.col("year") >= 2025)           # forecast contributes 2025–2050
        .select(
            pl.col("geoid"),
            pl.col("geoid").replace_strict(NY_COUNTIES).alias("geography"),
            pl.lit("NY").alias("state"),
            pl.col("year"),
            pl.lit("forecast").alias("period"),
            pl.col("population").cast(pl.Float64),
        )
    )
    return pl.concat([hist, fc], how="vertical")


def _vt_population() -> pl.DataFrame:
    """VT county population, history only (Census PEP, 2000–2020)."""
    return (
        pl.read_csv(VT_PATH, schema_overrides={"geoid": pl.Utf8})
        .select(
            pl.col("geoid"),
            pl.col("geography"),
            pl.lit("VT").alias("state"),
            pl.col("year"),
            pl.lit("history").alias("period"),
            pl.col("population").cast(pl.Float64),
        )
    )


def build_county_pop() -> pl.DataFrame:
    df = pl.concat([_ny_population(), _vt_population()], how="vertical")
    return df.sort("geoid", "year")


def build_washington_components() -> pl.DataFrame:
    """Washington County components of change, 2020–2035.

    2020–2025 (history): the Census PEP **v2025** vintage rows of
    ``washington_components.csv`` carry the full domestic/international split.
    2026–2035 (forecast): ``county_yearly_components.csv`` baseline forecast
    carries births/deaths/net_mig/pop_change only (the split columns are null).
    """
    # History 2020–2025 with the full migration split.
    hist = (
        pl.read_csv(POPFC_WASH)
        .filter(pl.col("vintage") == "v2025")
        .filter(pl.col("year").is_between(2020, 2025))
        .select(
            pl.col("year"),
            pl.lit("history").alias("period"),
            pl.col("births").cast(pl.Float64),
            pl.col("deaths").cast(pl.Float64),
            pl.col("domestic_mig").cast(pl.Float64),
            pl.col("international_mig").cast(pl.Float64),
            pl.col("net_mig").cast(pl.Float64),
            pl.col("pop_change").cast(pl.Float64),
        )
    )
    # Forecast 2026–2035 (net migration only; no domestic/international split).
    fc = (
        pl.read_csv(POPFC_YEARLY, schema_overrides={"geoid": pl.Utf8})
        .filter(pl.col("geoid") == "36115")
        .filter(pl.col("scenario") == "baseline")
        .filter(pl.col("year").is_between(2026, 2035))
        .select(
            pl.col("year"),
            pl.lit("forecast").alias("period"),
            pl.col("births").cast(pl.Float64),
            pl.col("deaths").cast(pl.Float64),
            pl.lit(None, dtype=pl.Float64).alias("domestic_mig"),
            pl.lit(None, dtype=pl.Float64).alias("international_mig"),
            pl.col("net_mig").cast(pl.Float64),
            pl.col("pop_change").cast(pl.Float64),
        )
    )
    return pl.concat([hist, fc], how="vertical").sort("year")


def main() -> None:
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)

    pop = build_county_pop()
    pop.write_parquet(OUT_POP, compression="zstd")
    by_state = (pop.group_by("state", "period").len().sort("state", "period")
                .to_dict(as_series=False))
    print(f"Wrote {pop.height} county-years to {OUT_POP.relative_to(PROJECT_ROOT)}")
    print(f"  counties: {sorted(pop['geography'].unique().to_list())}")
    print(f"  year range: {pop['year'].min()}–{pop['year'].max()}")
    for s, p, n in zip(by_state["state"], by_state["period"], by_state["len"]):
        print(f"    {s} {p}: {n} rows")

    comp = build_washington_components()
    comp.write_parquet(OUT_COMP, compression="zstd")
    split = comp.filter(pl.col("domestic_mig").is_not_null()).height
    print(f"\nWrote {comp.height} Washington component-years to "
          f"{OUT_COMP.relative_to(PROJECT_ROOT)} "
          f"({comp['year'].min()}–{comp['year'].max()}; "
          f"domestic/intl split on {split} history rows)")


if __name__ == "__main__":
    main()
