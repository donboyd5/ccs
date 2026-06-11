"""Build a district crosswalk for the ccs project.

One row per New York public school district, with the various identifiers that
different sources use, the district's long name, an editorial short name (only
for districts in the comparison groups, for now), and flags marking which
comparison group(s) each district belongs to.

Identifiers
-----------
- ``nysed_district_cd`` : NYSED 8-digit district code (the stable join key).
- ``beds_entity_cd``    : 12-digit BEDS entity code of the district-total row.
- ``nces_leaid``        : NCES Local Education Agency ID. **Not yet populated**
                          (not present in the NYSED source data); the column is
                          here so it can be filled from the NCES Common Core of
                          Data later without changing downstream code.

Short names and group membership are sourced from the book's config module
(``books/cambridge-comparisons/comparisons.py``) so there is a single source of
truth. Output: ``data/crosswalks/district_crosswalk.{csv,parquet}`` (the CSV is
tracked in git; the parquet is regenerable and ignored).

Run:  python src/build_crosswalk.py
"""

from __future__ import annotations

import sys
from pathlib import Path

import polars as pl

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "books" / "cambridge-comparisons"))
import comparisons as cc  # noqa: E402  (after sys.path tweak)

OUT_DIR = REPO_ROOT / "data" / "crosswalks"


def build() -> pl.DataFrame:
    panel = cc.load_panel()

    # Most-recent district-level attributes (names are already standardized to
    # the latest reported name, so taking the last row per district is safe).
    attrs = (
        panel.sort("year_end")
        .group_by("district_cd", maintain_order=True)
        .agg(
            pl.col("entity_cd").last().alias("beds_entity_cd"),
            pl.col("district_name").last(),
            pl.col("county_cd").last(),
            pl.col("county_name").last(),
            pl.col("needs_rc_category").last(),
            pl.col("needs_rc_description").last(),
        )
        .rename({"district_cd": "nysed_district_cd"})
    )

    graph_cds = set(cc.GRAPH_GROUP)
    table_cds = set(cc.TABLE_GROUP)

    xwalk = (
        attrs.with_columns(
            pl.col("nysed_district_cd")
            .replace_strict(cc.TABLE_GROUP, default=None)
            .alias("short_name"),
            pl.col("nysed_district_cd")
            .replace_strict(cc.NCES_LEAID, default=None)
            .alias("nces_leaid"),
            pl.col("nysed_district_cd")
            .replace_strict(cc.OSC_CODE, default=None)
            .alias("osc_municipal_code"),
            pl.col("nysed_district_cd").is_in(graph_cds).alias("in_graph_group"),
            pl.col("nysed_district_cd").is_in(table_cds).alias("in_table_group"),
        )
        .select(
            "short_name",
            "district_name",
            "county_name",
            "county_cd",
            "nysed_district_cd",
            "beds_entity_cd",
            "nces_leaid",
            "osc_municipal_code",
            "needs_rc_category",
            "needs_rc_description",
            "in_graph_group",
            "in_table_group",
        )
        .sort(["county_name", "district_name"])
    )
    return xwalk


def main() -> None:
    xwalk = build()
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    csv_path = OUT_DIR / "district_crosswalk.csv"
    pq_path = OUT_DIR / "district_crosswalk.parquet"
    xwalk.write_csv(csv_path)
    xwalk.write_parquet(pq_path)

    n_total = xwalk.height
    n_table = xwalk.filter(pl.col("in_table_group")).height
    n_graph = xwalk.filter(pl.col("in_graph_group")).height
    print(f"Wrote {n_total} districts to {csv_path.relative_to(REPO_ROOT)}")
    print(f"  comparison groups: {n_graph} graph, {n_table} table")
    n_nces = xwalk.filter(pl.col("nces_leaid").is_not_null()).height
    print(f"  NCES LEAIDs populated for {n_nces} comparison districts.")


if __name__ == "__main__":
    main()
