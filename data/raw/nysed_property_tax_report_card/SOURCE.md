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
| coverage | NYSED posts statewide files **2004-05 → 2026-27**; this project downloads all of them, but only **SY 2014-15 → 2026-27** (`year_end` 2015–2027) feeds the linkable panel — see [Downloads](#downloads-original-nysed-url--on-disk-name) and [Processed output](#processed-output) |
| grain | one row per district per year (the main vote file) |

## Downloads (original NYSED URL → on-disk name)

Files are downloaded by [`src/download_ptrc.py`](../../../src/download_ptrc.py) and
saved under clean, sortable names `<year_end>_ptrc.<ext>` (`year_end` = the **second
year of the budget span**; NYSED "2024-25" → `2025_ptrc.xlsx`). The on-disk
extension follows the source: NYSED posts **SY 2014-15 and later as `.xlsx`**,
**2013-14 and earlier as legacy `.xls`**. The original NYSED filenames are wildly
inconsistent, so this mapping is the provenance record (the `REGISTRY` dict in
`download_ptrc.py` is the machine-readable copy).

**Feed the linkable panel** (`year_end` 2015–2027). 2016–2027 share the modern
35-column layout; **2015 (SY 2014-15)** uses an earlier 29-column layout read by a
dedicated parser (see [Processed output](#processed-output)):

| year_end | SY | on-disk | original NYSED file (under `…/mgtserv/propertytax/`) |
|---:|---|---|---|
| 2027 | 2026-27 | `2027_ptrc.xlsx` | `docs/2026-27-ptrc-final.xlsx` |
| 2026 | 2025-26 | `2026_ptrc.xlsx` | `docs/2025-26-ptrc-20250509.xlsx` |
| 2025 | 2024-25 | `2025_ptrc.xlsx` | `docs/2024-25-ptrc-20240510-.xlsx` |
| 2024 | 2023-24 | `2024_ptrc.xlsx` | `docs/PTRC_23-24_FINAL.xlsx` |
| 2023 | 2022-23 | `2023_ptrc.xlsx` | `docs/PTRC_2022-2023_FINAL.xlsx` |
| 2022 | 2021-22 | `2022_ptrc.xlsx` | `docs/2021-22_PTRC.xlsx` |
| 2021 | 2020-21 | `2021_ptrc.xlsx` | `docs/2020-21-ptrc-sams-data.xlsx` |
| 2020 | 2019-20 | `2020_ptrc.xlsx` | `2019-20PTRCPost_R_ELW.xlsx` |
| 2019 | 2018-19 | `2019_ptrc.xlsx` | `2018-19PTRC5_11_18_Post_Final_000.xlsx` |
| 2018 | 2017-18 | `2018_ptrc.xlsx` | `Copyof2017-18_PTRC_FINAL_Posting_5-15-17_POSTONLY_R.xlsx` |
| 2017 | 2016-17 | `2017_ptrc.xlsx` | `2016_17PTRC_Post_Final.xlsx` |
| 2016 | 2015-16 | `2016_ptrc.xlsx` | `docs/2015-16_PTRC_Final_5_12_15.xlsx` |
| 2015 | 2014-15 | `2015_ptrc.xlsx` | `docs/2014-15_PTRC_revised_5_14_14_post.xlsx` |

**Downloaded but NOT in the panel** — older / differently-shaped layouts.
`build_ptrc.py` skips any workbook it cannot map to the panel schema and logs why:

| year_end | SY | on-disk | original NYSED file | why excluded |
|---:|---|---|---|---|
| 2014 | 2013-14 | `2014_ptrc.xls` | `docs/2013-14_PTRC_5_10_13_post.xls` | 27-col `.xls` layout |
| 2013 | 2012-13 | `2013_ptrc.xls` | `docs/2012-13_PTRC5_10_12_Full_Data_Post.xls` | 30-col `.xls` layout |
| 2012 | 2011-12 | `2012_ptrc.xls` | `docs/2011-12PTRC_5_4_11_post.xls` | crashes calamine `.xls` reader |
| 2011 | 2010-11 | `2011_ptrc.xls` | `docs/2010-11PTRC5-19-10_post_access.xls` | crashes calamine `.xls` reader |
| 2010 | 2009-10 | `2010_ptrc.xls` | `docs/2009-10PTRC5_13_09_post.xls` | crashes calamine `.xls` reader |
| 2009 | 2008-09 | `2009_ptrc.xls` | `docs/SM_PTRC_5_02_08_Post_r.xls` | crashes calamine `.xls` reader |
| 2008 | 2007-08 | `2008_ptrc.xls` | `docs/PTRC_Web_5_12_07.xls` | crashes calamine `.xls` reader |
| 2007 | 2006-07 | `2007_ptrc.xls` | `docs/PropTaxNumbers2006-6-13-06.xls` | crashes calamine `.xls` reader |
| 2006 | 2005-06 | `2006_ptrc.xls` | `docs/2005PropertyTaxReport2.xls` | 11-col pre-tax-cap schema |
| 2005 | 2004-05 | `2005_ptrc.xls` | `docs/property-tax-report-card-count-2004-05.xls` | 11-col pre-tax-cap schema |

The tax-levy-limit and exclusion columns reflect the **2012 property-tax-cap law**;
reports for SY 2013-14 and earlier use narrower schemas without them, and each year's
column arrangement drifts. The 2007-08 … 2012-13 `.xls` files additionally **crash**
the `calamine` reader behind `fastexcel`, so reading them at all would need a
different `.xls` engine (e.g. `xlrd`/`pandas`). Extending the panel before
`year_end` 2015 therefore requires **per-year parsing** and is left undone — the raw
files are kept so that work can start from disk without re-fetching.

NYSED also posts separate **re-vote** workbooks (from SY 2020-21) and
**schedule-of-reserves** workbooks (from SY 2018-19); these are **not pulled** yet.

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
— **8,691 district-years, year_end 2015–2027, 675 districts**. For 2016–2027 the
builder reads the modern **35-column** layout (prior-year / budget-year **pairs**),
selecting the **budget-year** value of each pair **by column position** (header text
carries shifting year labels and stray spaces). **SY 2014-15 (year_end 2015)** is an
earlier **29-column** layout (header on the 4th row, no levy breakout) read by a
dedicated parser (`read_2015` / `COLS_2015`); its budget-year column map is verified
by the identity `levy_vs_limit == proposed_levy_wo - levy_limit_wo` holding for every
district. The 6-digit BEDS code is joined to
`data/crosswalks/district_crosswalk.csv` to attach `nysed_district_cd`. Workbooks
that match no known layout are **skipped with a logged reason** (see the "Downloaded
but NOT in the panel" table above), never silently dropped.

**Null in 2014-15 (year_end 2015):** that report predates NYSED's split of the levy
into components, so four measures are **null** for every 2015 row —
`proposed_tax_levy_to_support_budget`, `levy_for_library_debt`,
`levy_for_nonexcludable_propositions`, and `tax_cap_reserve_used`. The single
`total_proposed_tax_levy` is populated and comparable across all years.

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

## Continuity & known data quirks

The historical extension was checked at both splices — the **2018→2019 seam**
(2016–2018 additions vs. the original 2019+ build) and the **2015→2016 seam**
(the separate 29-col 2014-15 reader vs. the 35-col years). Every major measure is
continuous at both: the median year-over-year change at each seam sits within the
band of neighboring transitions (e.g. proposed spending ≈ +1.9%/+2.7%, total
proposed levy ≈ +1.7%/+2.1%, levy limit ≈ +1.8%/+2.3%, projected enrollment ≈
−1.1%/−0.8%), so no concept or measure definition appears to change across either
splice. (Spot-check: Cambridge CSD proposed spending runs $19.40M → $20.06M →
$20.58M → $20.79M → $21.64M over 2015→2019, smooth.) The one mildly larger seam
move is `fund_balance_assigned_appropriated` at 2015→2016 (≈ −6.3% median vs. ~0
later) — this is the *appropriated* fund balance districts actively set each year,
so it reflects budgeting behavior, not a definitional break; its level series is
continuous.

**Null vs. zero in three sparse columns.** `levy_for_library_debt`,
`levy_for_nonexcludable_propositions`, and `tax_cap_reserve_used` are non-zero for
only ~1–4% of districts in any year. NYSED encodes "none" as `0` in most years but
leaves the cell **blank in `year_end` 2019 and 2021** (and partially in 2021). The
real (non-zero) incidence is stable across all years — only the encoding of "none"
differs — so **treat null ≡ 0** for these three columns. This is a pre-existing NYSED
quirk, not an artifact of the historical extension.
