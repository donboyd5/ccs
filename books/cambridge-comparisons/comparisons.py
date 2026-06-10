"""Shared configuration and helpers for the *Cambridge in Comparison* book.

Every chapter imports this module:

    import comparisons as cc

It defines the comparison groups, a colour palette, a robust data loader (it
works regardless of which directory Quarto renders from), and a common
plotnine theme. Keeping this here means the chapters stay short and every
chart/table draws the comparison groups the same way.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl


# ---------------------------------------------------------------------------
# Locate the repo's data/ directory. Quarto may execute a .qmd from the book
# directory or the project root, so we walk up the tree until we find it
# rather than hard-coding a relative path.
# ---------------------------------------------------------------------------
def _find_repo_root() -> Path:
    start = Path(__file__).resolve()
    for base in (start, *start.parents):
        if (base / "data" / "enrollment_staff").is_dir():
            return base
    raise FileNotFoundError(
        f"Could not find data/enrollment_staff above {start}"
    )


REPO_ROOT = _find_repo_root()
DATA_DIR = REPO_ROOT / "data"
PANEL_PATH = DATA_DIR / "enrollment_staff" / "district_enrollment_teachers_panel.parquet"


# ---------------------------------------------------------------------------
# Comparison groups. Keys are NYSED 8-digit district codes; values are the
# short labels used in charts and tables. Adjust these in one place to
# re-scope every chapter.
# ---------------------------------------------------------------------------
FOCUS = "Cambridge"  # the district every comparison is built around

#: Small group for line charts (kept short so charts stay readable).
GRAPH_GROUP: dict[str, str] = {
    "64161004": "Cambridge",      # Washington
    "64080104": "Greenwich",      # Washington
    "64150104": "Salem",          # Washington
    "52170104": "Schuylerville",  # Saratoga
    "49050106": "Hoosick Falls",  # Rensselaer
}

#: All Washington County K-12 districts. Putnam CSD (64140104) is excluded: it
#: is a K-8 district that tuitions out its high-schoolers (0 students in grades
#: 9-12), so it is not comparable on K-12 measures.
WASHINGTON_K12: dict[str, str] = {
    "64010104": "Argyle",
    "64161004": "Cambridge",
    "64050204": "Fort Ann",
    "64060102": "Fort Edward",
    "64070104": "Granville",
    "64080104": "Greenwich",
    "64100104": "Hartford",
    "64130106": "Hudson Falls",
    "64150104": "Salem",
    "64170106": "Whitehall",
}

#: Larger group for tables: every Washington County K-12 district plus the two
#: graph-group districts that sit just outside the county.
TABLE_GROUP: dict[str, str] = {
    **WASHINGTON_K12,
    "52170104": "Schuylerville",  # Saratoga
    "49050106": "Hoosick Falls",  # Rensselaer
}

#: Colour palette for the graph group: Cambridge gets the one bold colour and
#: the rest are calmer hues so the focus district reads first.
PALETTE: dict[str, str] = {
    "Cambridge": "#C8102E",      # bold red
    "Greenwich": "#2C6E9B",
    "Salem": "#3F9C5A",
    "Schuylerville": "#8156A7",
    "Hoosick Falls": "#E08214",
}


def load_panel() -> pl.DataFrame:
    """Read the district enrollment/teacher panel (one row per district-year)."""
    return pl.read_parquet(PANEL_PATH)


def panel_for(group: dict[str, str], *, since: int | None = 2018) -> pl.DataFrame:
    """Panel rows for `group`, with a short ``district`` label and a ``region``
    column (Washington County vs. other).

    `since` defaults to 2018, the first school-year-ending for which NYSED
    teacher counts (and therefore the students-per-teacher ratio) exist. Pass
    ``since=2016`` for enrollment-only series, or ``since=None`` for all years.
    """
    df = load_panel().filter(pl.col("district_cd").is_in(list(group)))
    if since is not None:
        df = df.filter(pl.col("year_end") >= since)
    return df.with_columns(
        pl.col("district_cd").replace_strict(group).alias("district"),
        pl.when(pl.col("county_name") == "Washington")
        .then(pl.lit("Washington County"))
        .otherwise(pl.lit("Other comparison districts"))
        .alias("region"),
    )


def district_order(group: dict[str, str]) -> list[str]:
    """Focus district first, then the rest alphabetically (orders chart legends)."""
    rest = sorted(name for name in group.values() if name != FOCUS)
    return [FOCUS, *rest]


def theme_ccs():
    """A clean, shared plotnine theme for the book's charts."""
    from plotnine import element_blank, element_text, theme, theme_minimal

    return theme_minimal(base_size=12) + theme(
        figure_size=(8, 4.8),
        plot_title=element_text(weight="bold", size=14, ha="left"),
        plot_subtitle=element_text(size=11, color="#555555", ha="left"),
        legend_title=element_blank(),
        legend_position="right",
        panel_grid_minor=element_blank(),
    )
