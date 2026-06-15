"""Build district-level K-12 enrollment and teacher time series for NY State.

Source: NYSED public data downloads (https://data.nysed.gov/downloads.php)

  * Enrollment Database ("BEDS Day Enrollment" table) -> K-12 enrollment.
    Eight annual MS-Access files (school years ending 2018-2025); each file
    carries THREE years of data, so the union spans school years 2015-16
    through 2024-25 (YEAR field = school-year-end: 2024 == 2023-24).
  * Student & Educator Database / "STUDED" ("Staff" table) -> teacher counts.
    Eight annual files; union spans school years 2017-18 through 2024-25.

Both databases share a stable schema across all years, so the series are
directly comparable within each dataset (see README.md for caveats).

District identification (per the NYSED ENTITY_CD layout, 12 digits):
    digits 1-2  = county        digits 7-8   = LEA type (86 == charter)
    digits 3-4  = city/town      digits 9-12  = building (0000 == DISTRICT)
We keep only district-total rows (digits 9-12 == "0000"), dropping building
rows and the statewide / county / Need-Resource-Capacity aggregate rows
(which use a leading "0000" or "11" entity code).

Outputs (data/processed/):
    enrollment_k12_by_district.parquet          tidy enrollment + grade detail
    teachers_by_district.parquet                tidy staff counts
    district_enrollment_teachers_panel.parquet  combined district-year panel
    district_enrollment_teachers_panel.csv      same, as CSV
    washington_area_enrollment_teachers.csv      Washington Co. + neighbors view

Usage:
    python src/build_enrollment_teachers.py
"""

from __future__ import annotations

import re
from pathlib import Path

import polars as pl
from access_parser import AccessParser

# --- configuration ---------------------------------------------------------
PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_enrollment_staff"
OUT_DIR = PROJECT_ROOT / "data" / "processed"
ENROLL_DIR = RAW_DIR / "enrollment"
STUDED_DIR = RAW_DIR / "studed"

# Washington County (64) plus its NY neighbors for the convenience view.
# Cambridge CSD straddles Washington (64) and Rensselaer (49).
COUNTY_NAME = {
    "01": "Albany", "02": "Allegany", "03": "Broome", "04": "Cattaraugus",
    "05": "Cayuga", "06": "Chautauqua", "07": "Chemung", "08": "Chenango",
    "09": "Clinton", "10": "Columbia", "11": "Cortland", "12": "Delaware",
    "13": "Dutchess", "14": "Erie", "15": "Essex", "16": "Franklin",
    "17": "Fulton", "18": "Genesee", "19": "Greene", "20": "Hamilton",
    "21": "Herkimer", "22": "Jefferson", "23": "Lewis", "24": "Livingston",
    "25": "Madison", "26": "Monroe", "27": "Montgomery", "28": "Nassau",
    "31": "New York", "32": "Bronx", "33": "Kings", "34": "Queens",
    "35": "Richmond", "40": "Niagara", "41": "Oneida", "42": "Onondaga",
    "43": "Ontario", "44": "Orange", "45": "Orleans", "46": "Oswego",
    "47": "Otsego", "48": "Putnam", "49": "Rensselaer", "50": "Rockland",
    "51": "St. Lawrence", "52": "Saratoga", "53": "Schenectady",
    "54": "Schoharie", "55": "Schuyler", "56": "Seneca", "57": "Steuben",
    "58": "Suffolk", "59": "Sullivan", "60": "Tioga", "61": "Tompkins",
    "62": "Ulster", "63": "Warren", "64": "Washington", "65": "Wayne",
    "66": "Westchester", "67": "Wyoming", "68": "Yates",
}
WASHINGTON_AREA_COUNTIES = {"64", "63", "52", "49", "15"}  # Wash, Warren, Sara, Rens, Essex

# Need/Resource Capacity categories (NYSED ENROLL ReadMe).
NEEDS_RC_LABEL = {
    "1": "High Need: NYC",
    "2": "High Need: Large City (Buffalo/Rochester/Syracuse/Yonkers)",
    "3": "High Need: Urban-Suburban",
    "4": "High Need: Rural",
    "5": "Average Need",
    "6": "Low Need",
    "7": "Charter School",
}

YEAR_RE = re.compile(r"(\d{4})")


# --- low-level helpers -----------------------------------------------------
def mdb_table(path: Path, table: str) -> pl.DataFrame:
    """Read one Access table as an all-string polars DataFrame.

    access-parser returns a {column: [values]} dict with mixed python types
    and a trailing-dot text artifact (e.g. "703."). We normalize every value
    to str here and clean numerics downstream.
    """
    raw = AccessParser(str(path)).parse_table(table)
    cols = list(raw.keys())
    n = max(len(v) for v in raw.values())
    data = {}
    for c in cols:
        vals = raw[c]
        if len(vals) < n:  # pad ragged columns (deleted-row artifact)
            vals = vals + [None] * (n - len(vals))
        data[c] = [None if v is None else str(v) for v in vals]
    return pl.DataFrame(data)


def num(col: str) -> pl.Expr:
    """Clean a NYSED text-stored number ("703.", "", None) into Float64."""
    return (
        pl.col(col)
        .str.strip_chars()
        .str.strip_chars_end(".")          # "703." -> "703", "2024." -> "2024"
        .replace("", None)
        .cast(pl.Float64, strict=False)
        .alias(col)
    )


def year_end_from_filename(path: Path) -> int:
    return int(YEAR_RE.search(path.stem).group(1))


def is_district() -> pl.Expr:
    """Keep district-total rows; drop buildings and statewide/county/NRC aggregates."""
    cd = pl.col("ENTITY_CD")
    return (
        cd.str.slice(8, 4).eq("0000")      # digits 9-12 == 0000  -> district
        & ~cd.str.starts_with("0000")      # drop statewide & county aggregates
        & cd.ne("111111111111")            # drop the statewide total row (exact)
    )


# --- builders --------------------------------------------------------------
def build_enrollment() -> tuple[pl.DataFrame, pl.DataFrame]:
    """Return (district K-12 enrollment, statewide reconciliation by year)."""
    grade_cols = ["KHALF", "KFULL", *[str(g) for g in range(1, 13)], "UGE", "UGS",
                  "PK", "PKHALF", "PKFULL"]
    frames = []
    for path in sorted(ENROLL_DIR.glob("ENROLL_*.mdb")):
        df = mdb_table(path, "BEDS Day Enrollment")
        df = df.with_columns(pl.lit(year_end_from_filename(path)).alias("source_year"))
        frames.append(df)
        print(f"  enroll {path.name}: {df.height:>7,} rows")
    allrows = pl.concat(frames, how="vertical_relaxed")

    cleaned = allrows.with_columns(
        num("YEAR").cast(pl.Int32).alias("year_end")
    ).with_columns([num(c) for c in ["K12", *grade_cols]])

    # Each (entity, year) appears in up to 3 source files. Flag any revisions,
    # then keep the value from the most recent file (single source of truth).
    dups = (
        cleaned.filter(is_district())
        .group_by("ENTITY_CD", "year_end")
        .agg(pl.col("K12").n_unique().alias("nuniq"))
        .filter(pl.col("nuniq") > 1)
        .height
    )
    print(f"  enrollment (district,year) cells with revised K12 across files: {dups}")
    dedup = (
        cleaned.sort("source_year", descending=True)
        .unique(subset=["ENTITY_CD", "year_end"], keep="first", maintain_order=True)
    )

    # Reconciliation: statewide row vs sum of districts vs charters (charters
    # have no district-aggregate row, so they are absent from the district sum).
    recon = (
        dedup.group_by("year_end")
        .agg(
            pl.col("K12").filter(pl.col("ENTITY_CD") == "111111111111").sum().alias("statewide_k12"),
            pl.col("K12").filter(is_district()).sum().alias("district_sum_k12"),
            pl.col("K12").filter(pl.col("ENTITY_CD").str.slice(6, 2) == "86").sum().alias("charter_k12"),
        )
        .sort("year_end")
    )

    df = (
        dedup.filter(is_district())
        .select(
            pl.col("ENTITY_CD").alias("entity_cd"),
            pl.col("ENTITY_CD").str.slice(0, 8).alias("district_cd"),
            pl.col("ENTITY_NAME").str.strip_chars().alias("district_name"),
            "year_end",
            pl.col("K12").alias("k12_enrollment"),
            *grade_cols,
            pl.col("source_year").alias("enr_source_year"),
        )
        .sort("entity_cd", "year_end")
    )
    return df, recon


def build_teachers() -> pl.DataFrame:
    keep_num = ["NUM_TEACH", "NUM_PRINC", "NUM_COUNSELORS", "NUM_SOCIAL",
                "PER_ATTEND", "PER_TURN_ALL", "PER_TURN_FIVE_YRS"]
    frames = []
    for path in sorted(STUDED_DIR.glob("STUDED_*.mdb")):
        df = mdb_table(path, "Staff")
        df = df.with_columns(pl.lit(year_end_from_filename(path)).alias("source_year"))
        frames.append(df)
        print(f"  staff  {path.name}: {df.height:>7,} rows")
    allrows = pl.concat(frames, how="vertical_relaxed")

    df = (
        allrows.with_columns(num("YEAR").cast(pl.Int32).alias("year_end"))
        .with_columns([num(c) for c in keep_num])
        .filter(is_district())
    )

    dups = (
        df.group_by("ENTITY_CD", "year_end")
        .agg(pl.col("NUM_TEACH").n_unique().alias("nuniq"))
        .filter(pl.col("nuniq") > 1)
        .height
    )
    print(f"  teachers (district,year) cells with revised NUM_TEACH across files: {dups}")

    df = (
        df.sort("source_year", descending=True)
        .unique(subset=["ENTITY_CD", "year_end"], keep="first", maintain_order=True)
        .select(
            pl.col("ENTITY_CD").alias("entity_cd"),
            pl.col("DISTRICT_CD").str.strip_chars().alias("district_cd"),
            pl.col("DISTRICT_NAME").str.strip_chars().alias("district_name"),
            "year_end",
            pl.col("NUM_TEACH").alias("num_teachers"),
            pl.col("NUM_PRINC").alias("num_principals"),
            pl.col("NUM_COUNSELORS").alias("num_counselors"),
            pl.col("NUM_SOCIAL").alias("num_social_workers"),
            pl.col("PER_ATTEND").alias("pct_teacher_attendance"),
            pl.col("PER_TURN_ALL").alias("pct_teacher_turnover"),
            pl.col("source_year").alias("staff_source_year"),
        )
        .sort("entity_cd", "year_end")
    )
    return df


def build_class_size() -> pl.DataFrame:
    """District-level NYSED 'Average Class Size' — one row per district x year x
    reported class.

    Roster/section-based (students enrolled in specified sections / number of
    sections), reported through SIRS beginning school year 2018-19 (year_end
    2019); it is NOT derived from teacher counts (headcount or FTE). NYSED gives
    no single overall number — only specific classes (K-2 and assessment-aligned
    courses) — so any per-district 'average class size' is an aggregate computed
    downstream. Only rows NYSED flags DATA_REPORTED='Y' with a value are kept.
    The pre-2018-19 era used a different (teacher-form) method and is excluded.
    """
    keep = ["ENTITY_CD", "ENTITY_NAME", "YEAR", "CLASS_DESCRIPTION",
            "AVERAGE_CLASS_SIZE", "DATA_REPORTED"]
    frames = []
    for path in sorted(STUDED_DIR.glob("STUDED_*.mdb")):
        df = mdb_table(path, "Average Class Size")
        # Pre-2018-19 files store this table in a WIDE layout (COMMON_BRANCH,
        # GRADE_8_MATH, ... ; years 2016-2018) under the old teacher-form method.
        # That era is non-comparable to the modern SIRS roster method, so skip it
        # with a logged reason rather than aborting (the modern long-format files
        # carry all of 2019-2025 anyway).
        if "CLASS_DESCRIPTION" not in df.columns:
            print(f"  classsize {path.name}: skipped (pre-2019 wide layout)")
            continue
        df = df.select(keep).with_columns(
            pl.lit(year_end_from_filename(path)).alias("source_year")
        )
        frames.append(df)
        print(f"  classsize {path.name}: {df.height:>7,} rows")
    allrows = pl.concat(frames, how="vertical_relaxed")

    df = (
        allrows.with_columns(
            num("YEAR").cast(pl.Int32).alias("year_end"),
            num("AVERAGE_CLASS_SIZE").alias("average_class_size"),
        )
        .filter(is_district())
        .filter(pl.col("DATA_REPORTED") == "Y")
        .filter(pl.col("average_class_size").is_not_null())
        .filter(pl.col("year_end") >= 2019)  # comparable SIRS-based era only
    )
    return (
        df.sort("source_year", descending=True)
        .unique(subset=["ENTITY_CD", "year_end", "CLASS_DESCRIPTION"],
                keep="first", maintain_order=True)
        .select(
            pl.col("ENTITY_CD").alias("entity_cd"),
            pl.col("ENTITY_CD").str.slice(0, 8).alias("district_cd"),
            pl.col("ENTITY_NAME").str.strip_chars().alias("district_name"),
            "year_end",
            pl.col("CLASS_DESCRIPTION").str.strip_chars().alias("class_description"),
            "average_class_size",
            pl.col("source_year").alias("acs_source_year"),
        )
        .sort("district_cd", "year_end", "class_description")
    )


def build_crosswalk() -> pl.DataFrame:
    """District -> county / BOCES / Need-Resource-Capacity.

    The enrollment 'BOCES and N/RC' table is keyed at the SCHOOL level; county,
    BOCES and N/RC are constant within a district, so we union all years and
    keep one row per DISTRICT_CD (latest file wins). The column set drifts
    across years, so we select a common slice from each file before stacking.
    """
    frames = []
    for path in sorted(ENROLL_DIR.glob("ENROLL_*.mdb")):
        df = mdb_table(path, "BOCES and N/RC")
        df = df.select(
            "DISTRICT_CD", "COUNTY_CD", "COUNTY_NAME", "BOCES_NAME", "NEEDS_INDEX",
        ).with_columns(pl.lit(year_end_from_filename(path)).alias("source_year"))
        frames.append(df)
    allrows = pl.concat(frames, how="vertical_relaxed")
    xwalk = (
        allrows.sort("source_year", descending=True)
        .unique(subset=["DISTRICT_CD"], keep="first")
        .select(
            pl.col("DISTRICT_CD").str.strip_chars().alias("district_cd"),
            pl.col("COUNTY_CD").str.strip_chars().str.zfill(2).alias("county_cd"),
            pl.col("COUNTY_NAME").str.strip_chars().str.to_titlecase().alias("county_name"),
            pl.col("BOCES_NAME").str.strip_chars().alias("boces_name"),
            pl.col("NEEDS_INDEX").str.strip_chars().alias("needs_rc_category"),
        )
        .with_columns(
            pl.col("needs_rc_category")
            .replace_strict(NEEDS_RC_LABEL, default=None)
            .alias("needs_rc_description"),
        )
    )
    return xwalk


# --- assembly + validation -------------------------------------------------
def main() -> None:
    print("Reading enrollment databases ...")
    enr, enr_recon = build_enrollment()
    print("Reading staff databases ...")
    tch = build_teachers()
    print("Reading class-size table ...")
    acs = build_class_size()
    print("Building district crosswalk ...")
    xwalk = build_crosswalk()

    # Combined district-year panel: outer join keeps enrollment-only years
    # (2016, 2017) and any staff-only rows. entity_cd is the shared district key.
    panel = (
        enr.join(
            tch.drop("district_cd").rename({"district_name": "district_name_t"}),
            on=["entity_cd", "year_end"],
            how="full",
            coalesce=True,
        )
        .with_columns(
            # Enrollment spans 2016-2025; teachers 2018-2025. Coalesce the name so
            # enrollment-only years (2016-17) and any staff-only districts keep one.
            pl.coalesce("district_name", "district_name_t").alias("district_name"),
            pl.coalesce("district_cd", pl.col("entity_cd").str.slice(0, 8)).alias("district_cd"),
        )
        .join(xwalk, on="district_cd", how="left")
        .with_columns(
            # county fallback from entity code when a district is absent from BOCES
            pl.coalesce("county_cd", pl.col("entity_cd").str.slice(0, 2)).alias("county_cd"),
        )
        .with_columns(
            pl.coalesce(
                "county_name",
                pl.col("county_cd").replace_strict(COUNTY_NAME, default=None),
            ).alias("county_name"),
        )
        .with_columns(
            pl.when(pl.col("num_teachers") > 0)
            .then(pl.col("k12_enrollment") / pl.col("num_teachers"))
            .otherwise(None)
            .round(2)
            .alias("k12_students_per_teacher"),
            pl.col("county_cd").is_in(WASHINGTON_AREA_COUNTIES).alias("washington_area"),
        )
        .select(
            "district_cd", "entity_cd", "district_name", "county_cd", "county_name",
            "needs_rc_category", "needs_rc_description", "boces_name", "washington_area",
            "year_end", "k12_enrollment", "num_teachers", "k12_students_per_teacher",
            "num_principals", "num_counselors", "num_social_workers",
            "pct_teacher_attendance", "pct_teacher_turnover",
        )
        .sort("county_cd", "district_name", "year_end")
    )

    # A district's reported name can change across years (e.g. "CAMBRIDGE CENTRAL
    # SCHOOL DISTRICT" -> "CAMBRIDGE CSD"). Label every year with the most recent
    # name so each district appears once in a grouped time series (district_cd is
    # the stable key regardless).
    current_name = (
        panel.filter(pl.col("district_name").is_not_null())
        .sort("year_end")
        .group_by("district_cd")
        .agg(pl.col("district_name").last().alias("current_name"))
    )
    panel = (
        panel.join(current_name, on="district_cd", how="left")
        .with_columns(pl.coalesce("current_name", "district_name").alias("district_name"))
        .drop("current_name")
        .sort("county_cd", "district_name", "year_end")
    )

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    enr.write_parquet(OUT_DIR / "enrollment_k12_by_district.parquet", compression="zstd")
    tch.write_parquet(OUT_DIR / "teachers_by_district.parquet", compression="zstd")
    acs.write_parquet(OUT_DIR / "class_size_by_district.parquet", compression="zstd")
    panel.write_parquet(OUT_DIR / "district_enrollment_teachers_panel.parquet", compression="zstd")
    panel.write_csv(OUT_DIR / "district_enrollment_teachers_panel.csv")

    wash = panel.filter(pl.col("washington_area"))
    wash.write_csv(OUT_DIR / "washington_area_enrollment_teachers.csv")

    # ---------------- validation report ----------------
    print("\n" + "=" * 64 + "\nVALIDATION\n" + "=" * 64)
    print(f"enrollment rows: {enr.height:,}  ({enr['year_end'].min()}-{enr['year_end'].max()})")
    print(f"teacher rows:    {tch.height:,}  ({tch['year_end'].min()}-{tch['year_end'].max()})")
    print(f"classsize rows:  {acs.height:,}  ({acs['year_end'].min()}-{acs['year_end'].max()}), "
          f"{acs['class_description'].n_unique()} class types, {acs['district_cd'].n_unique():,} districts")
    print(f"panel rows:      {panel.height:,}   districts: {panel['district_cd'].n_unique():,}")

    print("\nStatewide K-12 enrollment & teachers by year (district sums):")
    by_year = (
        panel.group_by("year_end")
        .agg(
            pl.col("k12_enrollment").sum().alias("k12"),
            pl.col("num_teachers").sum().alias("teachers"),
            pl.col("k12_enrollment").is_not_null().sum().alias("n_enr"),
            pl.col("num_teachers").is_not_null().sum().alias("n_tch"),
        )
        .sort("year_end")
    )
    with pl.Config(tbl_rows=20, thousands_separator=True):
        print(by_year)

    # K12 == sum of component grades incl. ungraded (definition check)
    comp = pl.sum_horizontal(
        "KHALF", "KFULL", *[str(g) for g in range(1, 13)], "UGE", "UGS"
    )
    chk = enr.with_columns((pl.col("k12_enrollment") - comp).abs().alias("d"))
    print(f"\nK12 vs sum(K..12,UGE,UGS) max abs diff: {chk['d'].max()} (expect 0)")

    # Reconciliation: district sum + charters should equal NYSED statewide total.
    recon = enr_recon.with_columns(
        (pl.col("statewide_k12") - pl.col("district_sum_k12") - pl.col("charter_k12"))
        .alias("residual")
    )
    print("\nEnrollment reconciliation (statewide = districts + charters + residual):")
    with pl.Config(tbl_rows=20, thousands_separator=True):
        print(recon)

    print("\nWashington-area districts (latest year):")
    latest = wash["year_end"].max()
    with pl.Config(tbl_rows=60, fmt_str_lengths=40):
        print(
            wash.filter(pl.col("year_end") == latest)
            .select("county_name", "district_name", "needs_rc_category",
                    "k12_enrollment", "num_teachers", "k12_students_per_teacher")
            .sort("county_name", "district_name")
        )

    print("\nCambridge CSD time series:")
    with pl.Config(tbl_rows=20):
        print(
            panel.filter(pl.col("district_name").str.contains("(?i)cambridge"))
            .select("district_name", "year_end", "k12_enrollment",
                    "num_teachers", "k12_students_per_teacher")
            .sort("district_name", "year_end")
        )

    print("\nWrote outputs to", OUT_DIR.relative_to(PROJECT_ROOT))


if __name__ == "__main__":
    main()
