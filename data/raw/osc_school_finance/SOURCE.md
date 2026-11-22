# NYS Comptroller (OSC) — local-government financial data, school districts

Annual revenue and expenditure detail for **every New York school district**,
by account code, from the NYS Office of the State Comptroller (OSC). OSC compiles
this from the **Annual Financial Report (AFR / "AUD")** every local government and
school district files under the NY Uniform System of Accounts.

| item | value |
|---|---|
| publisher | NYS Office of the State Comptroller (OSC) |
| underlying filing | school districts' Annual Update Document (AUD) |
| source page | <https://wwe1.osc.state.ny.us/localgov/findata/financial-data-for-local-governments.cfm> ("Financial Data for Local Governments") |
| bulk file | <https://wwe1.osc.state.ny.us/localgov/findata/level3zip/schooldistrict_all_years.zip> (one CSV per fiscal year inside) |
| coverage | one CSV per year, `1995`–`2026` (32 files) |
| snapshot on disk | OSC `SNAPSHOT_DATE` **2026-10-30**, downloaded 2026-11-19 |
| grain | one row per district × account code × year, with an `AMOUNT` |
| this project uses | **2013–2026** (see "Schema break" below) |

## Files

`raw/osc_school_finance/<year>_SchoolDistrict.csv` — one file per fiscal year,
extracted from OSC's bulk archive `schooldistrict_all_years.zip` (kept alongside)
by [`src/download_osc_finance.py`](../../../src/download_osc_finance.py). The
file names are OSC's own (the archive's member names).

Key columns (2013+ schema): `MUNICIPAL_CODE` (OSC entity code — *not* a BEDS/NYSED
code), `CALENDAR_YEAR` (fiscal year ending June 30), `ACCOUNT_CODE`,
`ACCOUNT_CODE_SECTION` (`REVENUE` / `EXPENDITURE`, plus balance-sheet rows `GL`
and fund-balance rows `FBNP`), `ACCOUNT_CODE_NARRATIVE`,
`LEVEL_1_CATEGORY`, `OBJECT_OF_EXPENDITURE`, `AMOUNT`, `SNAPSHOT_DATE`. The **fund** is the leading
letter of `ACCOUNT_CODE` (A = General, F = Special Aid, C = School Lunch,
H = Capital, V = Debt Service, …).

## Time convention

`CALENDAR_YEAR` is the **fiscal year ending June 30**, which equals the NYSED
school-year-ending label used elsewhere in this project — **no off-by-one**
(OSC 2025 = SY 2024-25 = `year_end` 2025).

## Schema break

The **2013–2026** files share one schema and stack cleanly (this is what the
project uses). **1995–2012** use an older, different layout and are excluded by
[`src/stack_finance.py`](../../../src/stack_finance.py).

## Linking

`MUNICIPAL_CODE` is OSC-specific and unrelated to NYSED/BEDS codes. The map from
OSC code → NYSED district code for the comparison districts lives in
[`comparisons.py`](../../../books/cambridge-comparisons/comparisons.py) (`OSC_CODE`)
and in [`data/crosswalks/district_crosswalk.csv`](../../crosswalks/district_crosswalk.csv)
(`osc_municipal_code`).

## Processed outputs

[`src/stack_finance.py`](../../../src/stack_finance.py) → `data/processed/school_finance.parquet`
(2013–2026, all districts). [`src/cambridge_view.py`](../../../src/cambridge_view.py)
→ `data/processed/cambridge_finance.parquet` (Cambridge only).
[`src/build_spending.py`](../../../src/build_spending.py) → `data/processed/spending_per_pupil.parquet`.

## Snapshots and revisions — the newest year is preliminary

Every refresh of OSC's archive is a new **snapshot** (`SNAPSHOT_DATE` on every
row), and OSC revises filed data between snapshots. Comparing the previous
snapshot on disk (2026-05-29) with the current one (2026-10-30):

- **1995–2012:** byte-for-byte identical.
- **2013–2024:** identical except one district's name spelling ("Gates-Chili" →
  "Gates Chili").
- **2025:** amounts changed for **32 of 692 districts** (61 more rows). None of the
  12 comparison districts changed in any dollar amount that feeds
  `spending_per_pupil.parquet`.
- **2026 (FY 2025-26):** new — 690 districts, 186,778 rows. This is OSC's **first
  release** of the year and should be treated as **preliminary**; expect the same
  kind of revision 2025 received.

### Cambridge FY 2025-26 — why all-funds totals jump (checked 2026-11-19)

Cambridge's all-funds expenditure rises from $29.1M (2025) to $34.6M (2026) and
all-funds revenue from $27.8M to $38.6M. The cause is a **capital project**, not
operations:

| fund | expenditure 2025 | expenditure 2026 | revenue 2025 | revenue 2026 |
|---|---:|---:|---:|---:|
| A — General | $25.32M | $24.23M | $24.94M | $25.98M |
| F — Special Aid | $1.19M | $0.93M | $1.19M | $0.93M |
| C — School Lunch | $0.80M | $0.73M | $0.77M | $0.70M |
| **H — Capital** | **$1.67M** | **$8.57M** | $0.80M | **$10.86M** |

Capital-fund spending is mostly `H16202` Operation of Plant, capital outlay
($6.95M) plus bus purchases ($0.65M); capital-fund revenue is `H5710` **Serial
Bonds, $10.24M**, which replaced a $10.2M bond anticipation note carried at the end
of 2024-25. The project's headline measure, `current_operating` (funds A+F+C less
capital outlay, debt service and transfers), is unaffected by this: it is $23.09M
in 2025 and **$22.31M in 2026 (−3.4%)**. That decline is itself unusual (statewide
median +3.3%; 96 of 677 districts show a decline) and comes mainly from lower
teacher-salary lines (−$0.76M) and contractual spending (−$0.28M), partly offset by
BOCES (+$0.41M) — worth re-checking against a later snapshot before leaning on it.
General-fund end-of-year fund balance (`A8029`): $5.56M (2024), $5.18M (2025),
$6.92M (2026).

## Update log

- **2026-11-19** — recorded the source URL and added `src/download_osc_finance.py`
  (closing the provenance gap noted when the files were first obtained); replaced
  all yearly files with the 2026-10-30 snapshot; extended the stack to **2026**.
  The previous `2026_SchoolDistrict.csv` was a header-only placeholder.
