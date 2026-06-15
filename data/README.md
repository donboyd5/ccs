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
| `stack_finance.py` | `raw/osc_school_finance/` | `school_finance.parquet` |
| `cambridge_view.py` | `processed/school_finance.parquet` | `cambridge_finance.parquet` |
| `build_spending.py` | `processed/school_finance.parquet` + enrollment | `spending_per_pupil.parquet` |
| `download_report_card.py` / `build_assessments.py` | `raw/nysed_report_card/zips/` | `assessments_em_ela_math.parquet` (+ `assessments_grade4_8_all_students.csv`) |
| `download_ptrc.py` / `build_ptrc.py` | `raw/nysed_property_tax_report_card/` | `property_tax_report_card.parquet` |
| `download_osc_tax_levies.py` / `build_osc_tax_levies.py` | `raw/osc_school_district_tax_levies/` | `osc_school_district_tax_levies.parquet` (+ tracked `crosswalks/osc_rpt_district_crosswalk.csv`) |
| `build_crosswalk.py` | enrollment panel + `comparisons.py` maps | `crosswalks/district_crosswalk.{csv,parquet}` |

The reader-facing **index of all data sources** is in the website's Sources
appendix: [`books/cambridge-comparisons/sources.qmd`](../books/cambridge-comparisons/sources.qmd).

## Join keys

`nysed_district_cd` (8-digit) is the project's stable key. Each source's native id
(`MUNICIPAL_CODE` for OSC, `leaid` for NCES, `beds_entity_cd` for NYSED PTRC/votes)
maps to it through [`crosswalks/district_crosswalk.csv`](crosswalks/district_crosswalk.csv).
