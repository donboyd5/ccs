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
CCD_PATH = DATA_DIR / "enrollment_staff" / "ccd_pupil_teacher_ny.parquet"


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
#: has no high school of its own (0 students in grades 9-12), so it is not
#: comparable on K-12 measures.
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

#: NCES district IDs (LEAIDs) for the comparison districts, used to join the
#: federal Common Core of Data (CCD) FTE-based pupil-teacher ratio. Keys are the
#: NYSED district codes (the project's stable key). Verified by name, county and
#: grade range against the CCD directory; note Fort Edward is a Union Free (UFSD)
#: district, not a "Central" district, and our Salem is Washington County's, not
#: the same-named district in Westchester.
NCES_LEAID: dict[str, str] = {
    "64010104": "3603210",  # Argyle
    "64161004": "3606210",  # Cambridge
    "64050204": "3611280",  # Fort Ann
    "64060102": "3611310",  # Fort Edward (UFSD)
    "64070104": "3612450",  # Granville
    "64080104": "3612900",  # Greenwich
    "64100104": "3613830",  # Hartford
    "64130106": "3614970",  # Hudson Falls
    "64150104": "3625470",  # Salem
    "64170106": "3631290",  # Whitehall
    "52170104": "3626160",  # Schuylerville
    "49050106": "3614760",  # Hoosick Falls
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


def load_ccd() -> pl.DataFrame:
    """Read the cached NCES Common Core of Data pupil-teacher table.

    Built by ``src/build_ccd_pupil_teacher.py`` from the Urban Institute's
    mirror of NCES CCD. ``year_end`` is already shifted to the NYSED
    school-year-ending convention: CCD labels a year by its fall term, so
    ``year_end = ccd_year + 1`` (e.g. CCD 2024 is the 2024-25 school year).
    The FTE-based ratio is ``enrollment / teachers_total_fte``.
    """
    return pl.read_parquet(CCD_PATH)


def ccd_ratio_for(group: dict[str, str]) -> pl.DataFrame:
    """NCES/CCD FTE-based students-per-teacher for `group`, labelled like the
    NYSED panel: columns ``district``, ``region``, ``year_end`` and
    ``ccd_students_per_teacher`` (only districts with a known LEAID appear).
    """
    name_by_leaid = {
        NCES_LEAID[cd]: name for cd, name in group.items() if cd in NCES_LEAID
    }
    region_by_name = {
        name: ("Washington County" if cd in WASHINGTON_K12 else "Other comparison districts")
        for cd, name in group.items()
    }
    return (
        load_ccd()
        .filter(pl.col("leaid").is_in(list(name_by_leaid)))
        .with_columns(pl.col("leaid").replace_strict(name_by_leaid).alias("district"))
        .with_columns(pl.col("district").replace_strict(region_by_name).alias("region"))
        .select("district", "region", "year_end", "ccd_students_per_teacher")
        .sort("district", "year_end")
    )


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
