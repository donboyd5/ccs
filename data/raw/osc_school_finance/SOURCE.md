# NYS Comptroller (OSC) — local-government financial data, school districts

Annual revenue and expenditure detail for **every New York school district**,
by account code, from the NYS Office of the State Comptroller (OSC). OSC compiles
this from the **Annual Financial Report (AFR / "AUD")** every local government and
school district files under the NY Uniform System of Accounts.

| item | value |
|---|---|
| publisher | NYS Office of the State Comptroller (OSC) |
| underlying filing | school districts' Annual Update Document (AUD) |
| coverage | one CSV per year, `1995`–`2026` (32 files) |
| grain | one row per district × account code × year, with an `AMOUNT` |
| this project uses | **2013–2025** (see "Schema break" below) |

## Files

`raw/osc_school_finance/<year>_SchoolDistrict.csv` — one file per fiscal year.
These names are a **project convention** (`<year>_SchoolDistrict.csv`), not OSC's
original download filenames.

Key columns (2013+ schema): `MUNICIPAL_CODE` (OSC entity code — *not* a BEDS/NYSED
code), `CALENDAR_YEAR` (fiscal year ending June 30), `ACCOUNT_CODE`,
`ACCOUNT_CODE_SECTION` (`REVENUE` / `EXPENDITURE`), `ACCOUNT_CODE_NARRATIVE`,
`LEVEL_1_CATEGORY`, `OBJECT_OF_EXPENDITURE`, `AMOUNT`. The **fund** is the leading
letter of `ACCOUNT_CODE` (A = General, F = Special Aid, C = School Lunch,
H = Capital, V = Debt Service, …).

## Time convention

`CALENDAR_YEAR` is the **fiscal year ending June 30**, which equals the NYSED
school-year-ending label used elsewhere in this project — **no off-by-one**
(OSC 2025 = SY 2024-25 = `year_end` 2025).

## Schema break

The **2013–2025** files share one schema and stack cleanly (this is what the
project uses). **1995–2012** use an older, different layout, and **2026** is a
near-empty placeholder; both are excluded by [`src/stack_finance.py`](../../../src/stack_finance.py).

## Linking

`MUNICIPAL_CODE` is OSC-specific and unrelated to NYSED/BEDS codes. The map from
OSC code → NYSED district code for the comparison districts lives in
[`comparisons.py`](../../../books/cambridge-comparisons/comparisons.py) (`OSC_CODE`)
and in [`data/crosswalks/district_crosswalk.csv`](../../crosswalks/district_crosswalk.csv)
(`osc_municipal_code`).

## Processed outputs

[`src/stack_finance.py`](../../../src/stack_finance.py) → `data/processed/school_finance.parquet`
(2013–2025, all districts). [`src/cambridge_view.py`](../../../src/cambridge_view.py)
→ `data/processed/cambridge_finance.parquet` (Cambridge only).
[`src/build_spending.py`](../../../src/build_spending.py) → `data/processed/spending_per_pupil.parquet`.

## ⚠ Provenance gap to confirm

The **exact OSC download URL / method and download date were not recorded** when
these CSVs were first obtained. They are consistent with OSC's *Local Government
Financial Data* (annual financial-data spreadsheets, by class of government, and
**Open Book New York**, <https://www.osc.ny.gov/local-government/data> /
<https://openbooknewyork.com>). **TODO:** confirm and record the precise source
(URL or data.ny.gov dataset id) and refresh date here.
