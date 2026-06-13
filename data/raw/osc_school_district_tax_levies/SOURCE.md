# NYS Comptroller (OSC) — school-district real property tax levies, full value & full-value tax rates

Annual **"Table 3"** workbooks giving, for **every New York school district**, the
district's **tax levy**, **taxable full value**, and **full-value tax rate**,
broken out by the **city/town each district overlaps** (the "component" detail),
plus — in recent years — a district **Total** row. From the NYS Office of the
State Comptroller (OSC), Division of Local Government and School Accountability.

| item | value |
|---|---|
| publisher | NYS Office of the State Comptroller (OSC) |
| source page | <https://www.osc.ny.gov/local-government/data/real-property-tax-levies-taxable-full-value-and-full-value-tax-rates> |
| underlying source | counties' Tax Data Verification Forms / districts' Constitutional Tax Limit forms, compiled by OSC |
| coverage on the page | school-district workbooks 2008–2025 (Excel) and 2001–2007 (PDF) |
| **this project uses** | **2015–2025** (the consistent full-value-only era) |
| grain | one row per district × city/town (**detail**) + one per district (**district_total**) |

## Files (original OSC name → on-disk name)

Downloaded by [`src/download_osc_tax_levies.py`](../../../src/download_osc_tax_levies.py),
saved under the clean, sortable convention `<year>_school_district_tax.<ext>`
(extension kept from the source). `<year>` is OSC's own file-year label.

| year | original OSC file (under `…/data/excel/`) | on-disk |
|---:|---|---|
| 2015 | `2015table3.xls`  | `2015_school_district_tax.xls`  |
| 2016 | `2016table3.xls`  | `2016_school_district_tax.xls`  |
| 2017 | `2017table3.xlsx` | `2017_school_district_tax.xlsx` |
| 2018 | `2018table3.xlsx` | `2018_school_district_tax.xlsx` |
| 2019 | `2019-school-districts.xlsx` | `2019_school_district_tax.xlsx` |
| 2020 | `2020-school-districts.xlsx` | `2020_school_district_tax.xlsx` |
| 2021 | `2021-school-districts.xlsx` | `2021_school_district_tax.xlsx` |
| 2022 | `2022-school-districts.xlsx` | `2022_school_district_tax.xlsx` |
| 2023 | `2023-school-districts.xlsx` | `2023_school_district_tax.xlsx` |
| 2024 | `2024-school-districts.xlsx` | `2024_school_district_tax.xlsx` |
| 2025 | `2025-school-districts.xlsx` | `2025_school_district_tax.xlsx` |

Also archived for provenance (same folder):
`datadescription2015.pdf` and `datadescription.pdf` (OSC data descriptions) and
`osc_landing_page.html` (a snapshot of the file-list page, recording exactly which
links were pulled). To re-download everything: `python src/download_osc_tax_levies.py`
(add `--force` to overwrite).

## Three format eras (handled by the builder, not by hand)

OSC's layout drifts; the builder detects the header row and maps each year's
headers to one canonical schema by text.

| era | files | district id column | town muni code | Library Levy col | district Total rows |
|---|---|---|---|---|---|
| 2015 | `2015table3.xls` | **name only** | no | no | no |
| 2016–2019 | `…table3` / `…school-districts` | `School District Municipal Code` | no | 2019 only | no |
| 2020–2025 | `…-school-districts.xlsx` (sheet `Combined`/`Sheet1`) | code (2023–25 label it `SD Muni Code - RPT Authority`) | yes | yes | **2021–2025 only** |

In the **2020** file the district code/name appear only on the *first* row of each
district block (merged cells); the builder forward-fills them. Header rows sit at
varying offsets (1–5) under title/note/date rows.

## Time convention — `year_end` = OSC file-year (verified, no off-by-one)

We label rows by `year_end` = OSC's file-year. Confirmed empirically against the
PTRC panel: OSC's *actual* district levy tracks PTRC's *proposed* levy for the
**same** `year_end` to ~0.1% (Cambridge OSC 2025 = $9,819,773 vs PTRC `year_end`
2025 = $9,829,260; the pattern holds every year). So OSC year *N* = school year
ending June 30 of *N*, matching `osc_school_finance` and the rest of the project.

## Identifiers & linking

* **`osc_sd_muni_code`** — OSC's 12-digit ORPTS "RPT Authority" code, the stable
  per-district id (same code system as `osc_school_finance`'s `MUNICIPAL_CODE`;
  Cambridge = `530790601000`). Present 2016+; **backfilled for 2015** from a
  name→code lookup built from the 2016 file. Codes for counties 01–09 are
  **zero-padded to 12** (Excel drops the leading zero when it stores them as a
  number).
* **`nysed_district_cd`** — the project key, attached via the generated crosswalk
  [`data/crosswalks/osc_rpt_district_crosswalk.csv`](../../crosswalks/osc_rpt_district_crosswalk.csv).
  Built by `src/build_osc_tax_levies.py` (normalized-name match to
  `district_crosswalk.csv`, then a county-scoped fallback, then 9 hand-verified
  `MANUAL_OVERRIDES`). **678 of 680** OSC districts link; the 12 pre-existing
  `osc_municipal_code` anchors all agree. `match_method` records how each linked.
  **Two intentionally unmatched** (reported, never dropped): *New York City*
  (NYSED splits it into 30+ borough districts; OSC reports one row) and *South
  Mountain-Hickory Common* (a non-operating district absent from the crosswalk).

## Detail vs. totals (and the closeness test)

`district_total` rows are **computed as the sum of each district's detail rows**
(`tax_levy`, `taxable_full_value`, `library_levy` summed; `full_value_tax_rate`
recomputed = 1000 × levy ÷ full value). For **2021–2025** OSC also publishes its
own Total rows; the builder's `validate_totals` checks the computed totals against
them and **refuses to write output** if any district is off by > 0.01%. Result:
3,389 district-years checked, worst levy error 2.3e-16 — i.e., the sums of the
detail parts equal OSC's reported totals to floating-point precision.
`total_source` flags each total as `computed_equals_osc` (2021–2025) or
`computed_no_osc_published` (2015–2020, OSC published no total).

## Known quirks & provenance gaps (surfaced, not hidden)

- **⚠ No official data dictionary for the school-district table.** The archived
  OSC `datadescription*.pdf` files document the **county/city/town/village**
  tables only (they say school-district rates are "located on our website"). The
  school-district column meanings here are read from the headers and the analogous
  town/city definitions (full value = taxable AV ÷ State equalization rate;
  full-value tax rate = levy per $1,000 of taxable full value).
- **2019 detail rates are district-wide, not town-specific.** In the 2019 file the
  `full_value_tax_rate` is the district's overall rate repeated on every town row
  (Cambridge: all towns = 17.070248), so it doesn't equal 1000 × levy ÷ full value
  per town (459 town-slices statewide differ noticeably). All other years report a
  town-specific rate. **District totals are unaffected** (computed from levy and
  full value). OSC's published values are preserved verbatim.
- **Definitional notes carried in the OSC files:** "Beginning in 2019, levy for
  New York City school district purposes are included in the New York City tax
  levy. Beginning in 2018 Library Levy is reported separately from Tax Levy."
- A handful of detail rows have zero/blank levy or full value (genuine in the
  source); the per-row rate check tolerates these.

## Processed output

[`src/build_osc_tax_levies.py`](../../../src/build_osc_tax_levies.py) →
`data/processed/osc_school_district_tax_levies.parquet` — **39,476 rows**, 2015–2025,
680 districts (32,014 detail + 7,462 district_total). Rebuild:
`python src/build_osc_tax_levies.py` (run the download script first).
