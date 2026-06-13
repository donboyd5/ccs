"""Build a tidy school-district property-tax panel from the OSC Table-3 workbooks.

Reads the per-year workbooks downloaded by ``download_osc_tax_levies.py`` (under
``data/raw/osc_school_district_tax_levies/``) and produces one long, district-
linkable table:

    one row per  year_end x school district x city/town    (record_type='detail')
    plus one     year_end x school district                (record_type='district_total')

OSC's workbooks drift across three layout eras (column names, header-row
position, sheet name, and which identifier columns exist). We detect the header
row, map each year's headers to a canonical schema by text, and forward-fill the
district-identity columns (the 2020 file leaves them blank on every row but the
first of each district block; 2021+ leave them blank only on the Total rows).

Identifiers
-----------
* ``osc_sd_muni_code`` -- OSC's 12-digit ORPTS "RPT Authority" code, the stable
  per-district id. Present 2016+; for 2015 (name only) we backfill it from a
  name->code lookup built from the 2016 file.
* ``nysed_district_cd`` -- the project key, attached via a generated crosswalk
  (``data/crosswalks/osc_rpt_district_crosswalk.csv``) built by normalized-name
  match to ``district_crosswalk.csv``. Districts that don't resolve (e.g. NYC,
  which OSC reports as one row but NYSED splits by borough) keep the OSC code and
  a null ``nysed_district_cd`` -- reported, never silently dropped.

Totals
------
District totals are **computed as the sum of the district's detail rows** for
every year (tax_levy, taxable_full_value, library_levy summed; full_value_tax_rate
recomputed as 1000 * levy / full_value). For 2021-2025 OSC also publishes its own
Total rows; ``validate_totals`` checks the computed totals against OSC's published
totals (they should agree to within rounding) and the build refuses to proceed if
any district is off by more than TOTAL_TOLERANCE.

Time convention
---------------
``year_end`` = OSC's file-year label. Verified against the PTRC panel: OSC's
*actual* district levy tracks the PTRC *proposed* levy for the same ``year_end``
to ~0.1% (e.g. Cambridge OSC 2025 = $9,819,773 vs PTRC year_end 2025 =
$9,829,260). So OSC year N == school year ending June 30 of year N; no off-by-one.

Output (data/processed/, git-ignored, regenerable):
    osc_school_district_tax_levies.parquet
Also (re)writes the tracked crosswalk:
    data/crosswalks/osc_rpt_district_crosswalk.csv

Usage:  python src/build_osc_tax_levies.py   (run download_osc_tax_levies.py first)
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import fastexcel
import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw" / "osc_school_district_tax_levies"
XWALK_IN = PROJECT_ROOT / "data" / "crosswalks" / "district_crosswalk.csv"
XWALK_OUT = PROJECT_ROOT / "data" / "crosswalks" / "osc_rpt_district_crosswalk.csv"
OUT_PATH = PROJECT_ROOT / "data" / "processed" / "osc_school_district_tax_levies.parquet"

YEARS = range(2015, 2026)  # 2015-2025 inclusive (the full-value-only era)

#: computed district total vs OSC-published total: max tolerated relative error
TOTAL_TOLERANCE = 1e-4  # 0.01%

# --- header text -> canonical column name ---------------------------------
# Longest matching prefix wins (so "School District Municipal Code" beats nothing
# and "Taxable Full Value" beats the bare "Taxable" fallback).
CANON: dict[str, list[str]] = {
    "osc_sd_muni_code":     ["School District Municipal Code", "SD Muni Code"],
    "school_name":          ["School Name", "School District Name"],
    "primary_county":       ["Primary County of School"],
    "city_town_muni_code":  ["City/Town Muni Code", "Muni Code - RPT Location"],
    "city_town_name":       ["City or Town Name", "City/Town Name", "Company Name"],
    "muni_class":           ["Class", "Sub Government Type"],
    "county_of_city_town":  ["County of City/Town"],
    "pct_school_in_town":   ["Percent of School in City/Town"],
    "pct_town_in_school":   ["Percent of City/Town in School"],
    "tax_levy":             ["School District Tax Levy", "Tax Levy"],
    "taxable_full_value":   ["Taxable Full Value", "Taxable  Full Value", "Taxable"],
    "full_value_tax_rate":  ["Full Value Tax Rate"],
    "library_levy":         ["Library Levy"],
}
IDENTITY_COLS = ["osc_sd_muni_code", "school_name", "primary_county"]
NUMERIC_COLS = ["pct_school_in_town", "pct_town_in_school", "tax_levy",
                "taxable_full_value", "full_value_tax_rate", "library_levy"]
_HEADER_HINTS = ("School District Municipal Code", "SD Muni Code",
                 "School District Name", "School Name", "Tax Levy", "Taxable",
                 "Full Value Tax Rate", "Percent of")


def _map_header(text: str) -> str | None:
    text = text.replace("\n", " ").strip()
    best, best_len = None, -1
    for canon, prefixes in CANON.items():
        for p in prefixes:
            if text.startswith(p) and len(p) > best_len:
                best, best_len = canon, len(p)
    return best


def _find_header_row(df: pl.DataFrame) -> int:
    for i in range(min(15, df.height)):
        row = [str(x) for x in df.row(i)]
        hits = sum(1 for v in row if any(v.strip().startswith(h) for h in _HEADER_HINTS))
        if hits >= 4:
            return i
    raise ValueError("could not locate a header row in the first 15 rows")


def read_year(path: Path) -> pl.DataFrame:
    """Return one year's rows in the canonical schema, identity cols forward-filled."""
    year = int(path.name[:4])
    wb = fastexcel.read_excel(str(path))
    raw = wb.load_sheet(wb.sheet_names[0], header_row=None).to_polars()
    hr = _find_header_row(raw)
    header = [str(x) for x in raw.row(hr)]
    rename = {raw.columns[j]: _map_header(header[j])
              for j in range(len(header)) if _map_header(header[j])}
    body = raw.slice(hr + 1).rename(rename).select(list(rename.values()))

    # add any canonical columns this era lacks, so all years share one schema
    for col in CANON:
        if col not in body.columns:
            body = body.with_columns(pl.lit(None, dtype=pl.Utf8).alias(col))
    body = body.select(list(CANON.keys()))

    # cast id/text columns to clean strings; numerics to float
    body = body.with_columns(
        *[pl.col(c).cast(pl.Utf8).str.strip_chars().replace("", None)
          for c in CANON if c not in NUMERIC_COLS],
        *[pl.col(c).cast(pl.Float64, strict=False) for c in NUMERIC_COLS],
    )
    # The 12-digit ORPTS codes lose their leading zero when Excel stores them as a
    # number (counties 01-09, e.g. Broome 031007300100 -> "31007300100"). Strip any
    # ".0" float tail and zero-pad back to 12 so codes stay stable & joinable.
    body = body.with_columns(
        *[pl.col(c).str.replace(r"\.0$", "").str.zfill(12)
          for c in ("osc_sd_muni_code", "city_town_muni_code")]
    )
    # forward-fill the district identity down the file (fixes 2020's blank blocks,
    # and gives 2021+ Total rows their district's code). File order is preserved.
    body = body.with_columns(pl.col(IDENTITY_COLS).forward_fill())
    return body.with_columns(
        pl.lit(year, dtype=pl.Int32).alias("year_end"),
        pl.lit(path.name).alias("source_file"),
    )


def split_detail_total(body: pl.DataFrame) -> tuple[pl.DataFrame, pl.DataFrame]:
    """Detail rows have a city/town; OSC's published Total rows end in 'Total'."""
    is_total = pl.col("school_name").str.strip_chars().str.ends_with("Total")
    detail = body.filter(pl.col("city_town_name").is_not_null() & ~is_total)
    osc_totals = (
        body.filter(is_total)
        .with_columns(pl.col("school_name").str.replace(r"\s*Total\s*$", ""))
    )
    return detail, osc_totals


def backfill_2015_codes(detail: pl.DataFrame) -> pl.DataFrame:
    """2015 has no muni code; backfill osc_sd_muni_code by district name from 2016."""
    lookup = (
        detail.filter((pl.col("year_end") == 2016) & pl.col("osc_sd_muni_code").is_not_null())
        .select(pl.col("school_name").alias("_nm"), "osc_sd_muni_code")
        .unique()
    )
    fixed = (
        detail.join(lookup, left_on="school_name", right_on="_nm", how="left", suffix="_bf")
        .with_columns(pl.col("osc_sd_muni_code").fill_null(pl.col("osc_sd_muni_code_bf")))
        .drop("osc_sd_muni_code_bf")
    )
    miss = fixed.filter((pl.col("year_end") == 2015) & pl.col("osc_sd_muni_code").is_null())
    if miss.height:
        names = miss.select("school_name").unique().to_series().to_list()
        print(f"  NOTE: {len(names)} 2015 district(s) had no 2016 name match (no OSC code): {names}")
    return fixed


def compute_totals(detail: pl.DataFrame) -> pl.DataFrame:
    """District totals = sum of detail parts; rate recomputed from the totals."""
    return (
        detail.group_by("year_end", "osc_sd_muni_code")
        .agg(
            pl.col("school_name").drop_nulls().first(),
            pl.col("primary_county").drop_nulls().first(),
            pl.col("source_file").first(),
            pl.col("tax_levy").sum(),
            pl.col("taxable_full_value").sum(),
            pl.col("library_levy").sum(),
        )
        .with_columns(
            (1000.0 * pl.col("tax_levy") / pl.col("taxable_full_value")).alias("full_value_tax_rate"),
            pl.lit("district_total").alias("record_type"),
            pl.lit(1.0).alias("pct_school_in_town"),
        )
    )


def validate_totals(computed: pl.DataFrame, osc_totals: pl.DataFrame) -> None:
    """Test that computed (sum-of-detail) totals match OSC's published Total rows."""
    if osc_totals.is_empty():
        print("  validate_totals: no OSC-published Total rows to check (none before 2021)")
        return
    chk = (
        computed.join(
            osc_totals.select("year_end", "osc_sd_muni_code",
                              pl.col("tax_levy").alias("osc_levy"),
                              pl.col("taxable_full_value").alias("osc_fv")),
            on=["year_end", "osc_sd_muni_code"], how="inner",
        )
        .with_columns(
            ((pl.col("tax_levy") - pl.col("osc_levy")).abs()
             / pl.col("osc_levy").abs().clip(lower_bound=1)).alias("levy_relerr"),
            ((pl.col("taxable_full_value") - pl.col("osc_fv")).abs()
             / pl.col("osc_fv").abs().clip(lower_bound=1)).alias("fv_relerr"),
        )
    )
    worst_levy = chk.select(pl.col("levy_relerr").max()).item() or 0.0
    worst_fv = chk.select(pl.col("fv_relerr").max()).item() or 0.0
    n_exact = chk.filter((pl.col("levy_relerr") == 0) & (pl.col("fv_relerr") == 0)).height
    print(f"  validate_totals: {chk.height} districts x years checked vs OSC totals; "
          f"{n_exact} exact; worst levy err={worst_levy:.2e}, worst FV err={worst_fv:.2e}")
    bad = chk.filter((pl.col("levy_relerr") > TOTAL_TOLERANCE) | (pl.col("fv_relerr") > TOTAL_TOLERANCE))
    if bad.height:
        print(f"  WARNING: {bad.height} district-years exceed tolerance {TOTAL_TOLERANCE:g}:")
        print(bad.select("year_end", "school_name", "tax_levy", "osc_levy",
                         "levy_relerr", "fv_relerr").sort("levy_relerr", descending=True).head(15))
        raise SystemExit("Detail-vs-total check FAILED; not writing output.")


# --- crosswalk to nysed_district_cd ---------------------------------------
def _norm_name(expr: pl.Expr) -> pl.Expr:
    """Normalize a district name to the crosswalk's spelling conventions."""
    e = expr.str.to_uppercase().str.replace_all(r"\(.*?\)", " ")  # drop "(LAKE SHORE)" etc.
    for long, short in [
        ("UNION FREE SCHOOL DISTRICT", "UFSD"),
        ("CENTRAL HIGH SCHOOL DISTRICT", "CHSD"),
        ("CENTRAL SCHOOL DISTRICT", "CSD"),
        ("COMMON SCHOOL DISTRICT", "COMN SD"),  # crosswalk abbreviates "COMN"
        ("CITY SCHOOL DISTRICT", "CITY SD"),
        ("FREE SCHOOL DISTRICT", "FSD"),
        ("SCHOOL DISTRICT", "SD"),
    ]:
        e = e.str.replace_all(long, short, literal=True)
    e = (e.str.replace_all(r"\bSAINT\b", "ST")
          .str.replace_all(r"\bMOUNT\b", "MT")
          .str.replace_all(r"\bINSTITUTE\b", "INST")
          .str.replace_all(r"\bAND\b", " "))  # "& "/"AND" are noise vs the crosswalk
    e = e.str.replace_all(r"[^A-Z0-9 ]", " ").str.replace_all(r"\s+", " ").str.strip_chars()
    return e


#: type/qualifier tokens dropped to form a county-scoped "core place name" key
_CORE_DROP = ["UFSD", "CHSD", "CSD", "CITY SD", "COMN SD", "FSD", "SD", "AT",
              "ACADEMY", "INST", "UNION", "FREE", "COMMON", "COMN", "CENTRAL",
              "CITY", "SCHOOL", "DISTRICT", "HIGH"]


def _core_name(expr: pl.Expr) -> pl.Expr:
    """Strip all district-type/qualifier words, leaving just the place name(s)."""
    e = _norm_name(expr)
    for tok in _CORE_DROP:
        e = e.str.replace_all(rf"\b{tok}\b", " ")
    return e.str.replace_all(r"\s+", " ").str.strip_chars()


#: Hand-verified osc_sd_muni_code -> nysed_district_cd for districts the automatic
#: name match can't resolve, each confirmed against district_crosswalk.csv. Reasons:
#: NYSED lists the district under an alias, or the crosswalk's district_name is
#: truncated to ~35 chars (so the parenthetical/long name is cut off), or an
#: abbreviation differs (NY vs New York). Two districts remain intentionally
#: unmatched: New York City (NYSED splits it into 30+ borough districts) and South
#: Mountain-Hickory Common (a non-operating district absent from the crosswalk).
MANUAL_OVERRIDES: dict[str, str] = {
    "150789600200": "15180104",  # Boquet Valley CSD (OSC adds "at Elizabethtown-Lewis-Westport")
    "320633200100": "43090106",  # Gorham-Middlesex CSD (Marcus Whitman) -- truncated in crosswalk
    "390638300100": "50020106",  # Haverstraw-Stony Point CSD (North Rockland) -- truncated
    "330739200100": "44090104",  # Highland Falls-Fort Montgomery -> HIGHLAND FALLS CSD
    "320750400100": "43110104",  # Manchester-Shortsville CSD (Red Jacket) -- truncated
    "270773600300": "27120104",  # Oppenheim-Ephratah-St. Johnsville CSD -- truncated ("...CS")
    "060610400100": "06020106",  # Southwestern CSD at Jamestown (vs Pine Valley, same county)
    "300957400400": "41150402",  # New York Mills UFSD -> crosswalk "NY MILLS UFSD" (abbrev.)
    "550634200100": "66040103",  # UFSD of the Tarrytowns -> crosswalk "UFSD-TARRYTOWNS"
}


def build_crosswalk(detail: pl.DataFrame) -> pl.DataFrame:
    """Map every OSC district to nysed_district_cd by normalized name.

    Keyed on the stable ``osc_sd_muni_code`` -- OSC's district *name* drifts across
    years (2016-2020 spell out "Central"/"Union Free"; 2023-2025 truncate to a bare
    "School District"), so per code we keep the **longest (most descriptive) name**,
    which is the one that normalizes to the crosswalk's CSD/UFSD/etc. forms.
    """
    osc = (
        detail.drop_nulls("osc_sd_muni_code")
        .select("osc_sd_muni_code", "school_name", "primary_county")
        .with_columns(pl.col("school_name").str.len_chars().alias("_nlen"))
        .sort("_nlen", descending=True)
        .unique(subset="osc_sd_muni_code", keep="first", maintain_order=True)
        .drop("_nlen").drop_nulls("school_name")
        .with_columns(
            _norm_name(pl.col("school_name")).alias("_key"),
            _core_name(pl.col("school_name")).alias("_core"),
            pl.col("primary_county").str.to_uppercase().str.strip_chars().alias("_county"),
        )
    )
    cw = (
        pl.read_csv(XWALK_IN, infer_schema_length=2000,
                    schema_overrides={"nysed_district_cd": pl.Utf8, "beds_entity_cd": pl.Utf8})
        .select("nysed_district_cd", "beds_entity_cd", "district_name", "county_name",
                "osc_municipal_code")
        .with_columns(
            _norm_name(pl.col("district_name")).alias("_key"),
            _core_name(pl.col("district_name")).alias("_core"),
            pl.col("county_name").str.to_uppercase().str.strip_chars().alias("_county"),
        )
    )

    def _unique_map(df: pl.DataFrame, keys: list[str]) -> pl.DataFrame:
        """key(s) -> nysed_district_cd, keeping only keys that map to exactly one."""
        g = df.group_by(keys).agg(
            pl.col("nysed_district_cd").unique().alias("_n"),
            pl.col("district_name").first().alias("_xn"))
        return (g.filter(pl.col("_n").list.len() == 1)
                 .with_columns(pl.col("_n").list.first().alias("nysed_district_cd")).drop("_n"))

    by_name = _unique_map(cw, ["_key"]).select(
        "_key", "nysed_district_cd", pl.col("_xn").alias("xwalk_name"))
    by_core = (_unique_map(cw.filter(pl.col("_core") != ""), ["_core", "_county"])
               .select("_core", "_county",
                       pl.col("nysed_district_cd").alias("_nysed_core"),
                       pl.col("_xn").alias("_xwalk_core")))

    out = (
        osc.join(by_name, on="_key", how="left")
        .join(by_core, on=["_core", "_county"], how="left")
        .with_columns(
            pl.when(pl.col("nysed_district_cd").is_not_null()).then(pl.lit("name"))
            .when(pl.col("_nysed_core").is_not_null()).then(pl.lit("name+county"))
            .otherwise(pl.lit("unmatched")).alias("match_method"),
            pl.coalesce("nysed_district_cd", "_nysed_core").alias("nysed_district_cd"),
            pl.coalesce("xwalk_name", "_xwalk_core").alias("xwalk_name"),
        )
        .select("osc_sd_muni_code", "school_name", "primary_county",
                "nysed_district_cd", "xwalk_name", "match_method")
        .sort("school_name")
    )
    # apply the hand-verified overrides (fills the residual the matcher can't resolve)
    ov = pl.DataFrame(
        {"osc_sd_muni_code": list(MANUAL_OVERRIDES), "_ov": list(MANUAL_OVERRIDES.values())},
        schema={"osc_sd_muni_code": pl.Utf8, "_ov": pl.Utf8})
    out = (
        out.join(ov, on="osc_sd_muni_code", how="left")
        .with_columns(
            pl.when(pl.col("_ov").is_not_null()).then(pl.lit("manual"))
            .otherwise(pl.col("match_method")).alias("match_method"),
            pl.coalesce("_ov", "nysed_district_cd").alias("nysed_district_cd"),
        )
        .drop("_ov")
    )
    # sanity: agree with the hand-entered osc_municipal_code anchors
    anchors = cw.filter(pl.col("osc_municipal_code").is_not_null()).select(
        pl.col("osc_municipal_code").cast(pl.Utf8).str.zfill(12).alias("osc_sd_muni_code"),
        pl.col("nysed_district_cd").alias("anchor_nysed"))
    disagree = (out.join(anchors, on="osc_sd_muni_code", how="inner")
                .filter(pl.col("nysed_district_cd") != pl.col("anchor_nysed")))
    by_method = dict(out.group_by("match_method").len().iter_rows())
    n_matched = out.filter(pl.col("match_method") != "unmatched").height
    print(f"  crosswalk: {n_matched}/{out.height} OSC districts -> nysed_district_cd "
          f"(name={by_method.get('name', 0)}, name+county={by_method.get('name+county', 0)}, "
          f"manual={by_method.get('manual', 0)}, unmatched={by_method.get('unmatched', 0)}); "
          f"{anchors.height} hand-coded anchors, {disagree.height} disagreements")
    if disagree.height:
        print("  WARNING: anchor disagreements:")
        print(disagree.select("school_name", "osc_sd_muni_code", "nysed_district_cd", "anchor_nysed"))
    unmatched = out.filter(pl.col("match_method") == "unmatched")
    if unmatched.height:
        print(f"  unmatched OSC districts ({unmatched.height}): "
              f"{unmatched.select('school_name').to_series().to_list()}")
    return out


def main() -> None:
    files = sorted(RAW_DIR.glob("*_school_district_tax.*"))
    files = [f for f in files if int(f.name[:4]) in YEARS]
    if not files:
        raise FileNotFoundError(f"No OSC workbooks in {RAW_DIR}; run download_osc_tax_levies.py first")

    print(f"Reading {len(files)} OSC workbooks ({YEARS.start}-{YEARS.stop - 1}) ...")
    bodies = [read_year(f) for f in files]
    all_rows = pl.concat(bodies, how="vertical")
    detail, osc_totals = split_detail_total(all_rows)
    detail = backfill_2015_codes(detail).with_columns(pl.lit("detail").alias("record_type"))
    print(f"  detail rows: {detail.height:,} | OSC-published total rows: {osc_totals.height:,}")

    computed = compute_totals(detail)
    print(f"  computed district totals: {computed.height:,}")
    validate_totals(computed, osc_totals)

    xwalk = build_crosswalk(detail)
    XWALK_OUT.parent.mkdir(parents=True, exist_ok=True)
    xwalk.write_csv(XWALK_OUT)
    print(f"  wrote {XWALK_OUT.relative_to(PROJECT_ROOT)} ({xwalk.height} districts)")

    code_to_nysed = xwalk.filter(pl.col("nysed_district_cd").is_not_null()).select(
        "osc_sd_muni_code", "nysed_district_cd").unique()

    # which year_ends have an OSC-published total (for the total_source flag)
    osc_total_years = set(osc_totals.select("year_end").unique().to_series().to_list())

    final_cols = ["year_end", "record_type", "nysed_district_cd", "osc_sd_muni_code",
                  "school_name", "primary_county", "city_town_muni_code", "city_town_name",
                  "muni_class", "county_of_city_town", "pct_school_in_town", "pct_town_in_school",
                  "tax_levy", "taxable_full_value", "full_value_tax_rate", "library_levy",
                  "total_source", "source_file"]

    detail = detail.with_columns(pl.lit(None, dtype=pl.Utf8).alias("total_source"))
    totals = computed.with_columns(
        pl.when(pl.col("year_end").is_in(list(osc_total_years)))
        .then(pl.lit("computed_equals_osc"))
        .otherwise(pl.lit("computed_no_osc_published")).alias("total_source"),
        *[pl.lit(None, dtype=pl.Utf8).alias(c) for c in
          ["city_town_muni_code", "city_town_name", "muni_class", "county_of_city_town"]],
        pl.lit(None, dtype=pl.Float64).alias("pct_town_in_school"),
    )
    panel = (
        pl.concat([detail.select(set(final_cols) & set(detail.columns)),
                   totals.select(set(final_cols) & set(totals.columns))],
                  how="diagonal")
        .join(code_to_nysed, on="osc_sd_muni_code", how="left")
        .select(final_cols)
        .sort("year_end", "school_name", "record_type", "city_town_name", nulls_last=True)
    )

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    panel.write_parquet(OUT_PATH, compression="zstd")
    n_unlinked = panel.filter(pl.col("nysed_district_cd").is_null()).select(
        "school_name").unique().height
    print(f"Wrote {panel.height:,} rows to {OUT_PATH.relative_to(PROJECT_ROOT)} "
          f"({panel['year_end'].min()}-{panel['year_end'].max()}, "
          f"{panel['osc_sd_muni_code'].n_unique()} districts; "
          f"{n_unlinked} district name(s) without nysed_district_cd)")


if __name__ == "__main__":
    main()
