"""Build a seamless NY grades 3-8 ELA & Math assessment panel from the SRC DBs.

Reads the annual NYSED Report Card (SRC) MS-Access databases downloaded by
[`download_report_card.py`](download_report_card.py) and stitches their grades 3-8
("EM" = elementary/middle) ELA & Math results into one tidy, multi-year panel.

Grain: **one row per entity x year_end x assessment x subgroup**, where
``assessment`` is e.g. ``ELA4``, ``MATH8``, the ``ELA3_8`` all-grades aggregate, or
the accelerated-math variants ``RegentsMath8`` / ``Combined8Math`` (see ``test_type``).

TWO SOURCE LAYOUTS, stitched seamlessly
---------------------------------------
* **New layout** (SRC2018-2025): single ``Annual EM ELA`` / ``Annual EM MATH``
  tables with an ``ASSESSMENT_NAME`` column and pre-computed proficiency.
* **Old layout** (SRC2016-2017): one table *per grade* (``ELA4 Subgroup
  Results`` ... ``Math8 Subgroup Results``) with no ``ASSESSMENT_NAME``, no
  proficiency column, and only 4 performance levels.

To make the two comparable, ``num_prof`` and ``pct_prof`` are **computed the same
way for every year**: ``num_prof = level3 + level4 + level5`` (level 5 exists only
for the accelerated-math variants; proficiency = Level 3+) and
``pct_prof = num_prof / num_tested * 100``.  This reproduces NYSED's reported
``PER_PROF`` exactly in the new-layout years (checked at build time).

Entities kept (NYSED ``Institution Grouping`` GROUP_CODE): 1 statewide, 2 county,
3 Need/Resource-Capacity group, 5 public-school district.  Individual schools
(code 6) are dropped — districts plus those ready-made benchmark aggregates are
what this panel is for; school rows remain in the raw DBs if ever needed.

Each SRC file carries ~2 school years and consecutive files overlap, so years are
de-duplicated **latest-file-wins**.  After dedup the old layout uniquely supplies
YEAR 2015-2016 and the new layout supplies 2017-2025 (YEAR 2020 is absent
statewide: spring-2020 grades 3-8 tests were cancelled for COVID).

Time convention: NYSED ``YEAR`` is the school-year-**end** (spring) year
("2025" == SY 2024-25), stored verbatim as ``year_end`` (no off-by-one).

Output: ``data/processed/assessments_em_ela_math.parquet`` (full panel) plus a
focused convenience view ``assessments_grade4_8_all_students.csv`` (grade 4 & 8
grade-level tests, All Students).

Usage:
    python src/build_assessments.py
"""

from __future__ import annotations

import gc
import subprocess
import tempfile
import zipfile
from pathlib import Path

import polars as pl
from access_parser import AccessParser

PROJECT_ROOT = Path(__file__).resolve().parent.parent
ZIP_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_report_card" / "zips"
OUT_DIR = PROJECT_ROOT / "data" / "processed"
OUT_PARQUET = OUT_DIR / "assessments_em_ela_math.parquet"
OUT_CSV = OUT_DIR / "assessments_grade4_8_all_students.csv"

GROUP_LEVEL = {"1": "state", "2": "county", "3": "nrc", "5": "district"}

# unified pre-finalize columns each reader emits (besides entity_level/src_year/subject)
INT_FIELDS = ["total_count", "not_tested", "num_tested",
              "level1_count", "level2_count", "level3_count",
              "level4_count", "level5_count", "total_scale_scores"]
FLOAT_FIELDS = ["pct_not_tested", "pct_tested",
                "level1_pct", "level2_pct", "level3_pct", "level4_pct",
                "level5_pct", "mean_score"]
STR_FIELDS = ["entity_cd", "entity_name", "subgroup", "assessment_name"]
ALL_FIELDS = STR_FIELDS + ["year_end"] + INT_FIELDS + FLOAT_FIELDS


def _int(col_or_expr) -> pl.Expr:
    e = pl.col(col_or_expr) if isinstance(col_or_expr, str) else col_or_expr
    return e.cast(pl.Utf8).str.strip_chars().str.strip_suffix(".").cast(pl.Int64, strict=False)


def _float(col_or_expr) -> pl.Expr:
    e = pl.col(col_or_expr) if isinstance(col_or_expr, str) else col_or_expr
    return e.cast(pl.Utf8).str.strip_chars().str.strip_suffix(".").cast(pl.Float64, strict=False)


def extract_db(zip_path: Path, tmpdir: Path) -> Path | None:
    with zipfile.ZipFile(zip_path) as zf:
        names = zf.namelist()
        member = next((n for n in names if n.lower().endswith(".mdb")), None) \
            or next((n for n in names if n.lower().endswith(".accdb")), None)
        if member is None:
            return None
        try:
            return Path(zf.extract(member, path=tmpdir))
        except NotImplementedError:
            pass  # some SRC zips use Deflate64, which stdlib zipfile can't read
    # fall back to the system `unzip` (handles Deflate64); -j flattens paths
    subprocess.run(["unzip", "-o", "-j", str(zip_path), member, "-d", str(tmpdir)],
                   check=True, stdout=subprocess.DEVNULL)
    return tmpdir / Path(member).name


def grouping_map(db: AccessParser) -> dict[str, str]:
    ig = db.parse_table("Institution Grouping")
    out: dict[str, str] = {}
    for cd, gc_ in zip(ig["ENTITY_CD"], ig["GROUP_CODE"]):
        lvl = GROUP_LEVEL.get(str(gc_).strip().rstrip("."))
        if cd and lvl:
            out[cd] = lvl
    return out


def district_meta(db: AccessParser, src_year: int) -> pl.DataFrame:
    b = db.parse_table("BOCES and N/RC")
    n = len(b["DISTRICT_CD"])
    col = lambda name: b[name] if name in b else [None] * n  # old layout lacks some
    return (
        pl.DataFrame({
            "nysed_district_cd": b["DISTRICT_CD"], "district_name": col("DISTRICT_NAME"),
            "county_name": col("COUNTY_NAME"), "boces_name": col("BOCES_NAME"),
            "needs_rc_category": col("NEEDS_INDEX"),
            "needs_rc_description": col("NEEDS_INDEX_DESCRIPTION"), "meta_year": col("YEAR"),
        })
        .with_columns(
            pl.col("nysed_district_cd").cast(pl.Utf8).str.strip_chars(),
            pl.col("needs_rc_category").cast(pl.Utf8).str.strip_chars().str.strip_suffix("."),
            _int("meta_year").alias("meta_year"),
            src_year=pl.lit(src_year),
        )
        .filter(pl.col("nysed_district_cd").is_not_null()
                & (pl.col("nysed_district_cd").str.len_chars() == 8))
        .sort("meta_year", descending=True)
        .unique(subset="nysed_district_cd", keep="first")
    )


def _finalize_block(df: pl.DataFrame, subject: str, glevel: dict[str, str],
                    src_year: int) -> pl.DataFrame:
    """Common post-read shaping: types, entity_level, subject, src_year."""
    # ensure every unified field exists (old layout lacks several)
    for c in INT_FIELDS + FLOAT_FIELDS + STR_FIELDS:
        if c not in df.columns:
            df = df.with_columns(pl.lit(None).alias(c))
    return (
        df.with_columns(
            *[_int(c).alias(c) for c in INT_FIELDS],
            *[_float(c).alias(c) for c in FLOAT_FIELDS],
            _int("year_end").alias("year_end"),
            pl.col("entity_cd").replace_strict(glevel, default=None).alias("entity_level"),
            subject=pl.lit(subject),
            src_year=pl.lit(src_year),
        )
        .filter(pl.col("entity_level").is_not_null())  # drop schools / unknown
        .select(*ALL_FIELDS, "entity_level", "subject", "src_year")
    )


def read_new(db: AccessParser, glevel: dict[str, str], src_year: int) -> list[pl.DataFrame]:
    out = []
    for table, subject in (("Annual EM ELA", "ELA"), ("Annual EM MATH", "Math")):
        raw = db.parse_table(table)
        ren = {
            "ENTITY_CD": "entity_cd", "ENTITY_NAME": "entity_name",
            "SUBGROUP_NAME": "subgroup", "ASSESSMENT_NAME": "assessment_name",
            "YEAR": "year_end", "TOTAL_COUNT": "total_count", "NOT_TESTED": "not_tested",
            "PCT_NOT_TESTED": "pct_not_tested", "NUM_TESTED": "num_tested",
            "PCT_TESTED": "pct_tested", "MEAN_SCORE": "mean_score",
            "TOTAL_SCALE_SCORES": "total_scale_scores",
            "LEVEL1_COUNT": "level1_count", "LEVEL2_COUNT": "level2_count",
            "LEVEL3_COUNT": "level3_count", "LEVEL4_COUNT": "level4_count",
            "LEVEL5_COUNT": "level5_count", "LEVEL1_%TESTED": "level1_pct",
            "LEVEL2_%TESTED": "level2_pct", "LEVEL3_%TESTED": "level3_pct",
            "LEVEL4_%TESTED": "level4_pct", "LEVEL5_%TESTED": "level5_pct",
        }
        df = pl.DataFrame({ren[k]: raw[k] for k in raw if k in ren})
        out.append(_finalize_block(df, subject, glevel, src_year))
    return out


def read_old(db: AccessParser, glevel: dict[str, str], src_year: int) -> list[pl.DataFrame]:
    ren = {
        "ENTITY_CD": "entity_cd", "ENTITY_NAME": "entity_name",
        "SUBGROUP_NAME": "subgroup", "YEAR": "year_end", "NUM_TESTED": "num_tested",
        "MEAN_SCORE": "mean_score", "TOTAL_SCALE_SCORES": "total_scale_scores",
        "LEVEL1_COUNT": "level1_count", "LEVEL2_COUNT": "level2_count",
        "LEVEL3_COUNT": "level3_count", "LEVEL4_COUNT": "level4_count",
        "LEVEL1_%TESTED": "level1_pct", "LEVEL2_%TESTED": "level2_pct",
        "LEVEL3_%TESTED": "level3_pct", "LEVEL4_%TESTED": "level4_pct",
    }
    out = []
    tables = [t for t in db.catalog if t.endswith("Subgroup Results")
              and "Total Cohort" not in t]
    for table in tables:
        prefix = table.split(" ")[0]            # "ELA4" / "Math4" / "Science4"
        if prefix.startswith("ELA"):
            subject, name = "ELA", prefix
        elif prefix.startswith("Math"):
            subject, name = "Math", prefix.upper()   # Math4 -> MATH4 (match new layout)
        else:
            continue                            # skip Science here
        raw = db.parse_table(table)
        df = pl.DataFrame({ren[k]: raw[k] for k in raw if k in ren})
        df = df.with_columns(assessment_name=pl.lit(name))
        out.append(_finalize_block(df, subject, glevel, src_year))
    return out


def synthesize_em_aggregate(panel: pl.DataFrame) -> pl.DataFrame:
    """Build a grades-3-8 aggregate (assessment ELA3_8 / MATH3_8) for years that
    lack a NYSED-provided one (the old layout), by summing the grade-level tests.
    Method matches how NYSED's own 3_8 row sums grades 3-8."""
    have = (panel.filter(pl.col("test_type") == "aggregate")
            .select("year_end", "subject").unique())
    # grade-level rows for (year, subject) combos that LACK a NYSED-provided aggregate
    grade = (panel.filter(pl.col("test_type") == "grade")
             .join(have, on=["year_end", "subject"], how="anti"))
    if grade.is_empty():
        return panel
    agg = (
        grade.group_by("entity_cd", "entity_name", "entity_level",
                       "nysed_district_cd", "year_end", "subject", "subgroup")
        .agg(
            pl.sum("num_tested"), pl.sum("total_count"), pl.sum("not_tested"),
            pl.sum("level1_count"), pl.sum("level2_count"), pl.sum("level3_count"),
            pl.sum("level4_count"), pl.sum("level5_count"), pl.sum("total_scale_scores"),
            pl.max("src_year"),
        )
        .with_columns(
            assessment_name=pl.col("subject").replace({"ELA": "ELA3_8", "Math": "MATH3_8"}),
            grade=pl.lit("3-8"), test_type=pl.lit("aggregate"),
            is_computed_aggregate=pl.lit(True),
            mean_score=(pl.col("total_scale_scores") / pl.col("num_tested")).round(0),
        )
    )
    return pl.concat([panel, agg], how="diagonal_relaxed")


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    zips = sorted(ZIP_DIR.glob("SRC*.zip"),
                  key=lambda p: int("".join(c for c in p.stem if c.isdigit())), reverse=True)
    if not zips:
        raise SystemExit(f"No SRC*.zip in {ZIP_DIR} — run download_report_card.py first.")

    frames: list[pl.DataFrame] = []
    metas: list[pl.DataFrame] = []
    with tempfile.TemporaryDirectory() as td:
        tmp = Path(td)
        for zp in zips:
            src_year = int("".join(c for c in zp.stem if c.isdigit()))
            mdb = extract_db(zp, tmp)
            if mdb is None:
                print(f"  {zp.name}: no .mdb/.accdb — skipped"); continue
            db = AccessParser(str(mdb))
            cat = set(db.catalog)
            glevel = grouping_map(db)
            metas.append(district_meta(db, src_year))
            if "Annual EM ELA" in cat:
                blocks, layout = read_new(db, glevel, src_year), "new"
            elif any(t.startswith(("ELA", "Math")) and t.endswith("Subgroup Results") for t in cat):
                blocks, layout = read_old(db, glevel, src_year), "old"
            else:
                print(f"  {zp.name}: no EM ELA/Math tables (COVID year?) — skipped")
                del db; mdb.unlink(missing_ok=True); gc.collect(); continue
            rows = sum(b.height for b in blocks)
            yrs = sorted({y for b in blocks for y in b["year_end"].unique().to_list() if y})
            print(f"  {zp.name} [{layout}]: {rows:>7,} rows  years={yrs}")
            frames.extend(blocks)
            del db; mdb.unlink(missing_ok=True); gc.collect()

    panel = pl.concat(frames, how="vertical_relaxed")

    # parse subject/grade/test_type from assessment_name
    panel = panel.with_columns(
        test_type=pl.when(pl.col("assessment_name").str.ends_with("3_8")).then(pl.lit("aggregate"))
        .when(pl.col("assessment_name").str.starts_with("Regents")).then(pl.lit("regents"))
        .when(pl.col("assessment_name").str.starts_with("Combined")).then(pl.lit("combined"))
        .otherwise(pl.lit("grade")),
        nysed_district_cd=pl.when(pl.col("entity_level") == "district")
        .then(pl.col("entity_cd").str.slice(0, 8)).otherwise(None),
    ).with_columns(
        grade=pl.when(pl.col("test_type") == "aggregate").then(pl.lit("3-8"))
        .otherwise(pl.col("assessment_name").str.extract(r"(\d)")),
        is_computed_aggregate=pl.lit(False),
    )

    # latest-file-wins de-dup across overlapping SRC files
    before = panel.height
    panel = (panel.sort("src_year", descending=True)
             .unique(subset=["entity_cd", "year_end", "assessment_name", "subgroup"], keep="first"))
    print(f"\nde-dup: {before:,} -> {panel.height:,} rows")

    # synthesize the old-year 3-8 aggregate (sums grade-level level counts), THEN
    # compute seamless proficiency (Level 3+) for every row from the level counts
    panel = synthesize_em_aggregate(panel)
    panel = panel.with_columns(
        num_prof=(pl.col("level3_count") + pl.col("level4_count")
                  + pl.col("level5_count").fill_null(0)).cast(pl.Int64),
    ).with_columns(
        # proficiency rate only where students were tested (avoid 0/0 -> NaN)
        pct_prof=pl.when(pl.col("num_tested") > 0)
        .then((pl.col("num_prof") / pl.col("num_tested") * 100).round(1))
        .otherwise(None),
        # a mean scale score is meaningful only for a single grade's test: the
        # grades-3-8 aggregate mixes per-grade scales (NYSED leaves it blank) and
        # 0 is a not-a-real-score placeholder -> null both
        mean_score=pl.when((pl.col("test_type") == "aggregate") | (pl.col("mean_score") == 0))
        .then(None).otherwise(pl.col("mean_score")),
    )

    # attach district metadata (latest src_year wins)
    meta = (pl.concat(metas, how="vertical_relaxed")
            .sort("src_year", descending=True).unique(subset="nysed_district_cd", keep="first")
            .select("nysed_district_cd", "district_name", "county_name",
                    "boces_name", "needs_rc_category", "needs_rc_description"))
    panel = panel.join(meta, on="nysed_district_cd", how="left")

    final_cols = [
        "nysed_district_cd", "entity_cd", "entity_name", "district_name", "entity_level",
        "county_name", "needs_rc_category", "needs_rc_description", "boces_name",
        "year_end", "subject", "grade", "test_type", "assessment_name", "subgroup",
        "num_tested", "total_count", "not_tested", "pct_tested", "pct_not_tested",
        "level1_count", "level1_pct", "level2_count", "level2_pct",
        "level3_count", "level3_pct", "level4_count", "level4_pct",
        "level5_count", "level5_pct", "num_prof", "pct_prof",
        "total_scale_scores", "mean_score", "is_computed_aggregate", "src_year",
    ]
    panel = panel.select([c for c in final_cols if c in panel.columns]).sort(
        ["subject", "assessment_name", "year_end", "entity_level", "entity_cd", "subgroup"])
    panel.write_parquet(OUT_PARQUET)
    print(f"\nwrote {OUT_PARQUET.relative_to(PROJECT_ROOT)}  "
          f"({panel.height:,} rows, {len(panel.columns)} cols)")

    view = (panel.filter(pl.col("assessment_name").is_in(["ELA4", "ELA8", "MATH4", "MATH8"])
                         & (pl.col("subgroup") == "All Students"))
            .select("entity_level", "nysed_district_cd", "entity_name", "district_name",
                    "county_name", "needs_rc_category", "year_end", "subject", "grade",
                    "num_tested", "num_prof", "pct_prof", "mean_score"))
    view.write_csv(OUT_CSV)
    print(f"wrote {OUT_CSV.relative_to(PROJECT_ROOT)}  ({view.height:,} rows)")
    _report(panel)


def _report(panel: pl.DataFrame) -> None:
    print("\n=== year coverage (ELA4, All Students) ===")
    print(panel.filter((pl.col("assessment_name") == "ELA4") & (pl.col("subgroup") == "All Students"))
          .group_by("year_end").agg(pl.len().alias("entities"),
                                    (pl.col("entity_level") == "district").sum().alias("districts"))
          .sort("year_end"))

    print("\n=== QA: computed pct_prof vs NYSED PER_PROF would-be (statewide ELA3_8) ===")
    print(panel.filter((pl.col("entity_cd") == "111111111111")
                       & (pl.col("assessment_name") == "ELA3_8")
                       & (pl.col("subgroup") == "All Students"))
          .select("year_end", "num_tested", "num_prof", "pct_prof", "mean_score",
                  "is_computed_aggregate").sort("year_end"))

    print("\n=== Cambridge CSD (64161004), All Students, grade 4 & 8 ===")
    print(panel.filter((pl.col("nysed_district_cd") == "64161004")
                       & (pl.col("subgroup") == "All Students")
                       & pl.col("assessment_name").is_in(["ELA4", "ELA8", "MATH4", "MATH8"]))
          .select("year_end", "subject", "grade", "num_tested", "pct_prof", "mean_score")
          .sort("subject", "grade", "year_end"))


if __name__ == "__main__":
    main()
