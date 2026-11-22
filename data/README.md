# `data/` — organization & provenance

This project keeps a hard line between **raw** source data (exactly as obtained
from an agency) and **processed** data (anything a script in [`src/`](../src/)
derives from it). Nothing in `processed/` is edited by hand; delete it and it
rebuilds from `raw/` + the scripts.

```
data/
├── raw/                     # source data, as downloaded — one folder per source
│   ├── osc_school_finance/            NYS Comptroller school-district finance (CSV)
│   ├── osc_school_district_tax_levies/ NYS Comptroller tax levies & full-value rates, by district×town (Excel)
│   ├── nysed_enrollment_staff/        NYSED enrollment + staff (MS-Access)
│   ├── nysed_report_card/             NYSED Report Card DB — grades 3-8 ELA/Math assessments (MS-Access)
│   ├── nysed_budget_votes/            NYSED budget vote / re-vote results (Excel)
│   ├── nysed_property_tax_report_card/ NYSED Property Tax Report Card (Excel)
│   ├── state_aid/                     NYS state aid by district and category (data.ny.gov)
│   ├── census_pep/                    Census population estimates, two Vermont counties
│   ├── popfc/                         pointer to the sibling popfc forecast project (SOURCE.md only)
│   ├── bls_cpi/                       BLS Consumer Price Index (CPI-U), monthly — the inflation adjustment
│   └── fac_annual_reports/            Cambridge CSD audited financial reports (PDF)
├── processed/               # derived datasets, regenerable via src/ (Parquet/CSV)
└── crosswalks/              # curated, hand-maintained id maps (CSV, tracked)
```

## Rules

1. **Raw goes in `raw/<source>/`.** Every raw folder carries a tracked
   **`SOURCE.md`** giving the publisher, source URL, coverage, time convention,
   the original-filename → on-disk mapping (raw files are often renamed to a clean
   convention), and how to re-download. Where a file is too big for GitHub it is
   git-ignored, but its `SOURCE.md` (and any download script) is tracked so the
   data can always be re-fetched.
2. **Processed goes in `processed/`** and is git-ignored — it is reproducible
   output, not source. Build it with the scripts in [`src/`](../src/).
3. **Crosswalks** (`crosswalks/`) are curated by hand and **tracked** (the CSV is
   the source of truth; the parallel `.parquet` is ignored).

## What builds what

| script | reads | writes (in `processed/`) |
|---|---|---|
| `build_enrollment_teachers.py` | `raw/nysed_enrollment_staff/` | `district_enrollment_teachers_panel.{parquet,csv}`, `enrollment_k12_by_district.parquet`, `teachers_by_district.parquet`, `washington_area_enrollment_teachers.csv` |
| `build_ccd_pupil_teacher.py` | NCES/CCD via Urban Institute API | `ccd_pupil_teacher_ny.parquet` |
| `download_osc_finance.py` / `stack_finance.py` | `raw/osc_school_finance/` | `school_finance.parquet` |
| `cambridge_view.py` | `processed/school_finance.parquet` | `cambridge_finance.parquet` |
| `build_spending.py` | `processed/school_finance.parquet` + enrollment | `spending_per_pupil.parquet` |
| `download_report_card.py` / `build_assessments.py` | `raw/nysed_report_card/zips/` | `assessments_em_ela_math.parquet` (+ `assessments_grade4_8_all_students.csv`) |
| `download_ptrc.py` / `build_ptrc.py` | `raw/nysed_property_tax_report_card/` | `property_tax_report_card.parquet` |
| `download_osc_tax_levies.py` / `build_osc_tax_levies.py` | `raw/osc_school_district_tax_levies/` | `osc_school_district_tax_levies.parquet` (+ tracked `crosswalks/osc_rpt_district_crosswalk.csv`) |
| `download_budget_votes.py` / `build_budget_votes.py` | `raw/nysed_budget_votes/` | `budget_votes.parquet` |
| `download_state_aid.py` / `build_state_aid.py` | `raw/state_aid/` | `state_aid.parquet` |
| `download_census_vt.py` / `build_demographics.py` | `raw/census_pep/` + the sibling `popfc` project | `demographics_county_pop.parquet`, `demographics_washington_components.parquet` |
| `download_cpi.py` / `build_price_index.py` | `raw/bls_cpi/` | `price_index_school_year.parquet` (used by `comparisons.to_real()` to adjust dollars for inflation) |
| `build_crosswalk.py` | enrollment panel + `comparisons.py` maps | `crosswalks/district_crosswalk.{csv,parquet}` |

The reader-facing **index of all data sources** is in the website's Sources
appendix: [`books/cambridge-comparisons/appendix/sources.qmd`](../books/cambridge-comparisons/appendix/sources.qmd).

## Refreshing the data (runbook)

Last full refresh: **2026-11-19** (see each `SOURCE.md` "Update log" and
[`meta/decisions.md`](../meta/decisions.md)). Run from the repo root with the
project environment (`.venv/bin/python`). Order matters where noted.

1. **Enrollment** — download the new `ENROLLMENT_<year>.zip` from
   <https://data.nysed.gov/downloads.php> into `raw/nysed_enrollment_staff/enrollment/`,
   extract its `.mdb` as `ENROLL_<year>.mdb` (likewise `STUDED_<year>.mdb` in
   `studed/` when the staff file is posted), then `python src/build_enrollment_teachers.py`.
2. **District crosswalk** — `python src/build_crosswalk.py` (after step 1: it reads
   the enrollment panel, and later builds read it). Check the diff of
   `crosswalks/district_crosswalk.csv` for new or merged districts.
3. **Budget votes** — add the year's two URLs to `REGISTRY` in
   `download_budget_votes.py`; run it, then `build_budget_votes.py`.
4. **Tax levies** — add the year to `REGISTRY` in `download_osc_tax_levies.py` and
   extend `YEARS` in `build_osc_tax_levies.py`; run both.
5. **Property Tax Report Card** — add the year to `REGISTRY` in `download_ptrc.py`;
   run it, then `build_ptrc.py`.
6. **School finance** — `python src/download_osc_finance.py` (replaces every yearly
   file with OSC's current snapshot), extend `YEARS` in `stack_finance.py`, then
   `stack_finance.py`, `cambridge_view.py`, `build_spending.py`. The newest year
   is preliminary.
7. **Demographics** — set `VINTAGE_2020S` in `download_census_vt.py` (needs
   `CENSUS_API_KEY`), run it, then `build_demographics.py` (needs the sibling
   `popfc` project at `~/Documents/code_projects/popfc`, or `POPFC_DIR`).
8. **State aid** — `download_state_aid.py`, then `build_state_aid.py`.
9. **Report card (test results)** — add the year to `REGISTRY` in
   `download_report_card.py`; `python src/download_report_card.py <year>` (add
   `--force` to replace a reposted file), then `build_assessments.py` (slow: it
   reads every multi-gigabyte database).
10. **Price index** — `download_cpi.py`, then `build_price_index.py`. When the
    chapters move to a newer latest year, raise `REAL_DOLLAR_YEAR` in
    `books/cambridge-comparisons/comparisons.py` so adjusted amounts are shown in
    that year's dollars, and reread every chapter that states adjusted amounts.

**After every rebuild, compare with the previous build** before trusting it:
earlier years should be unchanged unless the publisher revised them, and any
revision should be explained in that source's `SOURCE.md`.

**Re-rendering the book.** Quarto must use the project environment:
`QUARTO_PYTHON=$PWD/.venv/bin/python`. With `freeze: auto`, a whole-book
`quarto render` does **not** re-run a chapter whose `.qmd` is unchanged, even if
its data changed; render the chapter file directly (`quarto render
front/enrollment.qmd`) to force it. Chapter prose and chart titles that name
years are written by hand and do not update themselves.

## Join keys

`nysed_district_cd` (8-digit) is the project's stable key. Each source's native id
(`MUNICIPAL_CODE` for OSC, `leaid` for NCES, `beds_entity_cd` for NYSED PTRC/votes)
maps to it through [`crosswalks/district_crosswalk.csv`](crosswalks/district_crosswalk.csv).
