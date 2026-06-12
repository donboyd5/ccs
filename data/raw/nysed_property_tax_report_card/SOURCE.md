# NYSED — Property Tax Report Card (PTRC)

Each New York school district files a **Property Tax Report Card** when it puts its
budget to a public vote. It reports the **proposed budget**, the **proposed and
prior-year tax levy**, the district's **tax levy limit**, **projected enrollment**
and its percent change, the **CPI** factor, and **fund-balance / reserve**
amounts. NYSED posts a **statewide, all-districts** workbook each year.

| item | value |
|---|---|
| publisher | NYSED, Educational Management Services (data filed via SAMS) |
| source page | <https://www.p12.nysed.gov/mgtserv/propertytax/> |
| coverage | statewide files posted annually; this project pulls **2019-20 → 2026-27** |
| grain | one row per district per year (the main vote file) |

## Downloads (original NYSED URL → on-disk name)

Files are downloaded by [`src/download_ptrc.py`](../../../src/download_ptrc.py) and
saved under clean, sortable names `<year_end>_ptrc.xlsx` (`year_end` = the **second
year of the budget span**; NYSED "2024-25" → `2025_ptrc.xlsx`). The original NYSED
filenames are wildly inconsistent, so this mapping is the provenance record:

| year_end | on-disk | original NYSED file (under `…/mgtserv/propertytax/`) |
|---:|---|---|
| 2027 | `2027_ptrc.xlsx` | `docs/2026-27-ptrc-final.xlsx` |
| 2026 | `2026_ptrc.xlsx` | `docs/2025-26-ptrc-20250509.xlsx` |
| 2025 | `2025_ptrc.xlsx` | `docs/2024-25-ptrc-20240510-.xlsx` |
| 2024 | `2024_ptrc.xlsx` | `docs/PTRC_23-24_FINAL.xlsx` |
| 2023 | `2023_ptrc.xlsx` | `docs/PTRC_2022-2023_FINAL.xlsx` |
| 2022 | `2022_ptrc.xlsx` | `docs/2021-22_PTRC.xlsx` |
| 2021 | `2021_ptrc.xlsx` | `docs/2020-21-ptrc-sams-data.xlsx` |
| 2020 | `2020_ptrc.xlsx` | `2019-20PTRCPost_R_ELW.xlsx` |

(NYSED posts historical files back to 2004-05 and separate **re-vote** and
**schedule-of-reserves** workbooks; add them here if/when pulled. Older layouts
differ and may need per-year parsing.)

## Time convention

The PTRC is **forward-looking**: the workbook NYSED labels "2024-25" is the budget
**proposed in spring 2024 for school year 2024-25**. We store `year_end` = the
ending year of that span (**2025**), so it lines up with `k12_enrollment`,
`school_finance`, and budget-vote `year` for the **same school year** — but note
PTRC figures are *proposed/budgeted*, whereas OSC finance is *actual*.

## Linking

The vote file identifies districts by **BEDS code** and name. Join to the rest of
the project via [`data/crosswalks/district_crosswalk.csv`](../../crosswalks/district_crosswalk.csv)
(`beds_entity_cd` / `nysed_district_cd`).

## Processed output

[`src/build_ptrc.py`](../../../src/build_ptrc.py) → `data/processed/property_tax_report_card.parquet`
— **6,015 district-years, year_end 2019–2027, 675 districts**. The main data sheet
has a stable **35-column** layout across all years (prior-year / budget-year
**pairs**); the builder selects the **budget-year** value of each pair **by column
position** (header text carries shifting year labels and stray spaces) and joins
the 6-digit BEDS code to `data/crosswalks/district_crosswalk.csv` to attach
`nysed_district_cd`.

Columns: `nysed_district_cd`, `beds_code`, `district_name`, `county_name`,
`year_end`, then the budget-year measures — `proposed_spending`,
`spending_pct_change`, `proposed_tax_levy_to_support_budget`,
`levy_for_library_debt`, `levy_for_nonexcludable_propositions`,
`tax_cap_reserve_used`, `total_proposed_tax_levy`, `tax_levy_pct_change`,
`permissible_exclusions`, `tax_levy_limit_wo_exclusions`,
`proposed_tax_levy_wo_exclusions`, `levy_vs_limit_wo_exclusions`,
`projected_enrollment`, `enrollment_pct_change`, `fund_balance_restricted`,
`fund_balance_assigned_appropriated`, `fund_balance_unrestricted`,
`fund_balance_unrestricted_pct_of_budget`. Dollar amounts are **nominal**;
`*_pct_*` are percents; `projected_enrollment` is the report's **forecast** for the
budget year (it can differ from later *actual* K-12 enrollment).

**Known non-link:** `ROCKLAND CSD` (Sullivan Co., BEDS `591303`) appears only in the
2026-27 and 2025-26 cards — a newly reorganized district not yet in the enrollment
panel (which ends at `year_end` 2025), so it is reported by the builder and
excluded from the linked output until enrollment coverage catches up.
