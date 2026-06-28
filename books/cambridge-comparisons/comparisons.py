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
        # data/crosswalks is tracked in git, so it is present in every clone
        # (unlike the git-ignored, regenerable data/processed/).
        if (base / "data" / "crosswalks").is_dir():
            return base
    raise FileNotFoundError(
        f"Could not find data/crosswalks above {start}"
    )


REPO_ROOT = _find_repo_root()
DATA_DIR = REPO_ROOT / "data"
PANEL_PATH = DATA_DIR / "processed" / "district_enrollment_teachers_panel.parquet"
CCD_PATH = DATA_DIR / "processed" / "ccd_pupil_teacher_ny.parquet"
SPENDING_PATH = DATA_DIR / "processed" / "spending_per_pupil.parquet"
CLASS_SIZE_PATH = DATA_DIR / "processed" / "class_size_by_district.parquet"
BUDGET_VOTES_PATH = DATA_DIR / "processed" / "budget_votes.parquet"
PTRC_PATH = DATA_DIR / "processed" / "property_tax_report_card.parquet"


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

#: NYS Comptroller (OSC) municipal codes for the comparison districts, used to
#: join OSC local-government financial data (school spending). Keys are NYSED
#: district codes. Matched by district name + county against the OSC data; OSC
#: codes are unrelated to NYSED/BEDS codes, so this map is the only join key.
OSC_CODE: dict[str, str] = {
    "64010104": "530703000100",  # Argyle
    "64161004": "530790601000",  # Cambridge
    "64050204": "530729500200",  # Fort Ann
    "64060102": "530929700100",  # Fort Edward (UFSD)
    "64070104": "530733900100",  # Granville
    "64080104": "530734900100",  # Greenwich
    "64100104": "530737800100",  # Hartford
    "64130106": "530643900100",  # Hudson Falls
    "64150104": "530773800100",  # Salem
    "64170106": "530690700100",  # Whitehall
    "52170104": "410774700100",  # Schuylerville
    "49050106": "380639800100",  # Hoosick Falls
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


def district_code(district: str) -> str:
    """Resolve a district to its 8-digit NYSED code (the project's stable key).

    Accepts either the code itself or a known short label from the comparison
    groups. The **code is canonical** — it is the join key used everywhere and
    is stable across a district's name changes; the label is only a convenience
    for readable one-off calls. Prefer passing the code in scripts.
    """
    known = {**TABLE_GROUP, **GRAPH_GROUP}  # code -> label
    if district in known:
        return district
    label_to_code = {label: cd for cd, label in known.items()}
    if district in label_to_code:
        return label_to_code[district]
    raise KeyError(
        f"Unknown district {district!r}: pass an 8-digit NYSED code or one of "
        f"{sorted(label_to_code)}"
    )


def staffing_summary(district: str, *, since: int = 2018) -> pl.DataFrame:
    """One-district staffing summary, shaped wide for a table.

    Rows are the three measures (K-12 enrollment, teachers, teachers per 100
    students); columns are each school-year-ending ``>= since`` plus the change
    and percent change from the first to the last of those years. ``district``
    may be a NYSED code (preferred) or a known short label (see
    :func:`district_code`). ``since`` defaults to 2018, the first year teacher
    counts exist.
    """
    code = district_code(district)
    s = (
        load_panel()
        .filter((pl.col("district_cd") == code) & (pl.col("year_end") >= since))
        .select("year_end", "k12_enrollment", "num_teachers")
        .with_columns(
            (pl.col("num_teachers") / pl.col("k12_enrollment") * 100).alias(
                "teachers_per_100_students"
            )
        )
        .sort("year_end")
    )
    labels = {
        "k12_enrollment": "K–12 enrollment",
        "num_teachers": "Teachers",
        "teachers_per_100_students": "Teachers per 100 students",
    }
    order = list(labels)
    first, last = str(s["year_end"].min()), str(s["year_end"].max())
    return (
        s.unpivot(index="year_end", on=order, variable_name="measure", value_name="value")
        .pivot(values="value", index="measure", on="year_end")
        .with_columns(
            (pl.col(last) - pl.col(first)).alias("chg"),
            ((pl.col(last) - pl.col(first)) / pl.col(first)).alias("pct_chg"),
            pl.col("measure").replace_strict({k: i for i, k in enumerate(order)}).alias("_ord"),
        )
        .sort("_ord")
        .with_columns(pl.col("measure").replace_strict(labels))
        .drop("_ord")
    )


def staffing_table(district: str, *, since: int = 2018):
    """A great-tables ``GT`` of :func:`staffing_summary` for one district.

    Styled like the book's other tables: a "School year ending" spanner over the
    year columns, a "Change" spanner over change / % change, the ratio row
    highlighted, and the standard NYSED source note. Counts render as whole
    numbers, the teachers-per-100 row to one decimal. ``district`` may be a
    NYSED code or a known short label.
    """
    from great_tables import GT, loc, md, style

    code = district_code(district)
    label = {**TABLE_GROUP, **GRAPH_GROUP}.get(code, code)
    wide = staffing_summary(district, since=since)
    year_cols = [c for c in wide.columns if c.isdigit()]
    first, last = year_cols[0], year_cols[-1]
    ratio_row = pl.col("measure") == "Teachers per 100 students"

    return (
        GT(wide, rowname_col="measure")
        .tab_header(
            title=f"{label}: enrollment, teachers, and staffing",
            subtitle=f"School-year-ending {first} through {last}",
        )
        .fmt_number(columns=year_cols, rows=~ratio_row, decimals=0)
        .fmt_number(columns=year_cols, rows=ratio_row, decimals=1)
        .fmt_number(columns="chg", rows=~ratio_row, decimals=0, force_sign=True)
        .fmt_number(columns="chg", rows=ratio_row, decimals=1, force_sign=True)
        .fmt_percent(columns="pct_chg", decimals=1, force_sign=True)
        .tab_spanner(label="School year ending", columns=year_cols)
        .tab_spanner(label=f"Change, {first} → {last}", columns=["chg", "pct_chg"])
        .cols_label(chg="Change", pct_chg="% change")
        .tab_style(
            style=[style.fill(color="#FCEFB4"), style.text(weight="bold")],
            locations=loc.stub(rows=ratio_row),
        )
        .tab_source_note(
            md(
                "**Source:** NYSED Enrollment database (K-12 enrollment) and "
                "Student &amp; Educator database (teacher counts). Teachers per 100 "
                "students = teacher headcount ÷ K-12 enrollment × 100. Year = "
                "school-year-ending (2025 = 2024–25)."
            )
        )
    )


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


def load_class_size() -> pl.DataFrame:
    """Read the district-level NYSED 'Average Class Size' extract — one row per
    district x year x reported class (e.g. Kindergarten, Grade 1, Mathematics
    (grade 5), Biology ...).

    Built by ``src/build_enrollment_teachers.py`` from the STUDED ``Average
    Class Size`` table. It is **roster/section-based** (students in a section ÷
    number of sections), reported via SIRS for school years 2018-19 onward
    (``year_end >= 2019``); it is **not** derived from teacher counts. NYSED
    publishes no single overall figure, so a per-district 'average class size'
    must be aggregated across the reported classes (see
    :func:`class_size_median_for`). See the source's ``SOURCE.md``.
    """
    return pl.read_parquet(CLASS_SIZE_PATH)


def class_size_for(
    group: dict[str, str],
    *,
    tier: str | None = None,
    subjects: list[str] | None = None,
    classes: list[str] | None = None,
) -> pl.DataFrame:
    """Median NYSED average class size across a chosen basket of classes, per
    district-year, for `group`.

    Narrow the basket with ``tier`` (``'elementary'`` | ``'grades_3_8'`` |
    ``'high_school'``), ``subjects`` (e.g. ``['ELA', 'Math']``), and/or
    ``classes`` (canonical names like ``'Algebra I'``). With no filters it
    medians across every reported class. The **median** (not mean) resists
    small-section artifacts (a 2-student physics class, a pandemic-year glitch).
    Columns: ``district``, ``region``, ``year_end``, ``class_size``,
    ``n_classes`` (how many classes the median is taken over).
    """
    region_by_cd = {
        cd: ("Washington County" if cd in WASHINGTON_K12 else "Other comparison districts")
        for cd in group
    }
    d = load_class_size().filter(pl.col("district_cd").is_in(list(group)))
    if tier is not None:
        d = d.filter(pl.col("class_tier") == tier)
    if subjects is not None:
        d = d.filter(pl.col("class_subject").is_in(subjects))
    if classes is not None:
        d = d.filter(pl.col("class_canonical").is_in(classes))
    return (
        # Collapse label variants of the same course first: in transition years a
        # district can report e.g. "Algebra I" and "Algebra I (Common Core)" (both
        # canonical "Algebra I") with different values. Average those to one value
        # per canonical class so a course isn't double-weighted, then median across
        # courses (n_classes = distinct canonical classes).
        d.group_by("district_cd", "year_end", "class_canonical")
        .agg(pl.col("average_class_size").mean().alias("class_avg"))
        .group_by("district_cd", "year_end")
        .agg(
            pl.col("class_avg").median().alias("class_size"),
            pl.len().alias("n_classes"),
        )
        .with_columns(
            pl.col("district_cd").replace_strict(group).alias("district"),
            pl.col("district_cd").replace_strict(region_by_cd).alias("region"),
        )
        .select("district", "region", "year_end", "class_size", "n_classes")
        .sort("district", "year_end")
    )


def class_size_median_for(group: dict[str, str]) -> pl.DataFrame:
    """Median class size across **all** reported classes (the broad view).

    Thin wrapper over :func:`class_size_for` with no basket filter; the value
    column is named ``median_class_size`` for the overview chart.
    """
    return class_size_for(group).rename({"class_size": "median_class_size"})


def load_spending() -> pl.DataFrame:
    """Read the cached per-pupil spending panel (one row per comparison
    district-year).

    Built by ``src/build_spending.py`` from the NYS Comptroller (OSC)
    local-government financial data, joined to NYSED K-12 enrollment. Carries
    three expenditure measures and their per-pupil values (see that script).
    ``year_end`` is the fiscal year ending June 30, which equals OSC's
    ``CALENDAR_YEAR`` and the NYSED school-year-ending label.
    """
    return pl.read_parquet(SPENDING_PATH)


def load_budget_votes() -> pl.DataFrame:
    """Read the NYSED budget vote & re-vote panel.

    Built by ``src/build_budget_votes.py`` from NYSED's statewide *School
    District Budget Voting Results* workbooks. One row per district × vote:
    ``year`` (the vote's *calendar* year — the May vote, NOT the budget-end
    year), ``kind`` (``"vote"`` = May, ``"revote"`` = June), ``district`` and
    ``district_key`` (a normalized name for joining vote↔revote across NYSED's
    inconsistent name forms), ``yes`` / ``no`` counts, and ``above_cap``
    (True = the budget needed a 60% supermajority to pass). These files carry
    district *names*, not codes, and **no levy data** — only the 60% flag — so
    the panel is not joinable to the ``nysed_district_cd`` crosswalk. See the
    source's ``SOURCE.md`` for provenance and the year convention.
    """
    return pl.read_parquet(BUDGET_VOTES_PATH)


def load_ptrc() -> pl.DataFrame:
    """Read the NYSED Property Tax Report Card panel (one row per district-year).

    Built by ``src/build_ptrc.py`` from NYSED's PTRC workbooks. Carries the
    **proposed** figures put before voters each May: proposed spending and tax
    levy, the tax-levy **limit** (the cap), **permissible exclusions**, the
    levy-vs-limit gap, fund balances, and projected enrollment. ``year_end`` is
    the budget-span end (the school year the budget funds).

    ⚠ The stored ``levy_vs_limit_wo_exclusions`` column's **sign flips at 2016**
    (see the source's ``SOURCE.md``): in 2015 it is ``proposed − limit``; from 2016
    it is ``limit − proposed``. Compute ``proposed_tax_levy_wo_exclusions −
    tax_levy_limit_wo_exclusions`` for a consistent over/under gap.
    """
    return pl.read_parquet(PTRC_PATH)


def ptrc_for(group: dict[str, str]) -> pl.DataFrame:
    """PTRC proposed-budget / tax-cap measures for `group`, labelled like the other
    panels (short ``district`` name + ``region``). Keeps the cap-relevant columns;
    callers compute the over/under gap as ``proposed_tax_levy_wo_exclusions −
    tax_levy_limit_wo_exclusions`` (positive = over the cap) because the stored
    ``levy_vs_limit`` sign is inconsistent across years.
    """
    region_by_cd = {
        cd: ("Washington County" if cd in WASHINGTON_K12 else "Other comparison districts")
        for cd in group
    }
    return (
        load_ptrc()
        .filter(pl.col("nysed_district_cd").is_in(list(group)))
        .with_columns(
            pl.col("nysed_district_cd").replace_strict(group).alias("district"),
            pl.col("nysed_district_cd").replace_strict(region_by_cd).alias("region"),
        )
        .sort("district", "year_end")
    )


def spending_for(group: dict[str, str]) -> pl.DataFrame:
    """Per-pupil spending for `group`, labelled like the other panels: a short
    ``district`` name and a ``region`` column, plus the spending measures.
    """
    region_by_cd = {
        cd: ("Washington County" if cd in WASHINGTON_K12 else "Other comparison districts")
        for cd in group
    }
    return (
        load_spending()
        .filter(pl.col("district_cd").is_in(list(group)))
        .with_columns(
            pl.col("district_cd").replace_strict(group).alias("district"),
            pl.col("district_cd").replace_strict(region_by_cd).alias("region"),
        )
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
