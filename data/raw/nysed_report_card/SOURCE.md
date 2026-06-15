# NYSED — Report Card Database (SRC)

Each year NYSED posts a single statewide **Report Card Database** ("SRC") — a
zipped MS-Access database holding *every* New York State Report Card table for that
school year: grades 3-8 ELA / Math / Science assessment results, Regents results,
accountability, enrollment, staff, graduation, expenditures, and more, aggregated
at the **statewide, county, Need/Resource-Capacity (N/RC), public-school-district,
and public-school** levels.

This project currently uses the SRC only for the **grades 3-8 ELA & Math
assessment** tables (built into a seamless multi-year panel — see
[Processed output](#processed-output)). The same databases also contain Regents,
science, graduation, etc., which can be added later without re-downloading.

| item | value |
|---|---|
| publisher | NYSED (New York State Education Department), data.nysed.gov |
| source page | <https://data.nysed.gov/downloads.php> (section "Report Card Database") |
| coverage downloaded | **SY 2015-16 → 2024-25** (10 annual files, `year_end` 2016–2025) |
| grain (raw) | one row per entity × school-year × assessment × subgroup |
| format | zipped Access DB (`.accdb` **and** `.mdb`, same data) + a per-year ReadMe PDF |

## Downloads (original NYSED URL → on-disk name)

Downloaded by [`src/download_report_card.py`](../../../src/download_report_card.py)
into `zips/` under the original NYSED basename `SRC{year}.zip` (`year` is the
school-year-**end** year; NYSED "2024-25" → `SRC2025.zip`). The `REGISTRY` dict in
that script is the machine-readable copy of this mapping. Files are large
(~250–385 MB each, ~3.3 GB total) and **git-ignored**; re-fetch with
`python src/download_report_card.py`.

| year_end | SY | on-disk | original NYSED URL |
|---:|---|---|---|
| 2025 | 2024-25 | `zips/SRC2025.zip` | `/files/essa/24-25/SRC2025.zip` |
| 2024 | 2023-24 | `zips/SRC2024.zip` | `/files/essa/23-24/SRC2024.zip` |
| 2023 | 2022-23 | `zips/SRC2023.zip` | `/files/essa/22-23/SRC2023.zip` |
| 2022 | 2021-22 | `zips/SRC2022.zip` | `/files/essa/21-22/SRC2022.zip` |
| 2021 | 2020-21 | `zips/SRC2021.zip` | `/files/essa/20-21/SRC2021.zip` |
| 2020 | 2019-20 | `zips/SRC2020.zip` | `/files/essa/19-20/SRC2020.zip` |
| 2019 | 2018-19 | `zips/SRC2019.zip` | `/files/essa/18-19/SRC2019.zip` |
| 2018 | 2017-18 | `zips/SRC2018.zip` | `/files/essa/17-18/SRC2018.zip` |
| 2017 | 2016-17 | `zips/SRC2017.zip` | `/files/reportcards/16-17/SRC2017.zip` |
| 2016 | 2015-16 | `zips/SRC2016.zip` | `/files/reportcards/15-16/SRC2016.zip` |

(NYSED moved the path from `/files/reportcards/` to `/files/essa/` starting with
SY 2017-18. Files back to SY 1999-00 exist on the page if the panel is ever
extended further.) The base URL is `https://data.nysed.gov`.

## Time convention

NYSED's `YEAR` field is the school-year-**end** (spring) year — the ReadMe states
"2025 for 2024-25" — so it is stored verbatim as `year_end`, **no off-by-one**.
Grades 3-8 tests are administered in **spring**, so `year_end = 2025` is the spring
**2025** administration (SY 2024-25). Each SRC file carries ~2 school years and
consecutive files overlap; the build de-duplicates **latest-file-wins**.

**Verified against NYSED's published release:** statewide ("All Public Schools")
grades 3-8 ELA, All Students, reads **46% proficient (2024) → 53% (2025)** in this
data, exactly matching NYSED's August 2025 statement that the rate rose seven
points to 53%.

## Entity codes (`ENTITY_CD`)

12-digit BEDS code. Per the ReadMe: digits 1-2 = county, 3-4 = city/town, 5-6 =
district number, 7-8 = LEA type, **9-12 = building (`0000` = the district total)**.
So a district-total row ends in `0000` and its **8-digit `nysed_district_cd` = the
first 8 digits** (the project's stable join key). Example: Cambridge CSD =
`641610040000` → `nysed_district_cd = 64161004` (Washington County, N/RC 5
"Average Need").

Entity level is taken from the DB's own `Institution Grouping` table
(`GROUP_CODE`: 1 statewide, 2 county, 3 N/RC group, 5 district, 6 school). The
build **keeps levels 1/2/3/5** (districts plus those ready-made benchmark
aggregates) and **drops individual schools (level 6)** — school rows remain in the
raw DBs if ever needed.

## Two source layouts (stitched seamlessly)

The assessment tables changed shape across the 10 years. The builder handles both
and emits one unified schema:

| layout | files | structure |
|---|---|---|
| **old** | `SRC2016`, `SRC2017` | one table **per grade**: `ELA3 Subgroup Results` … `Math8 Subgroup Results`. No `ASSESSMENT_NAME`, **no proficiency column**, only 4 performance levels, no test-taken/not-tested counts. |
| **new** | `SRC2018`–`SRC2025` | single `Annual EM ELA` / `Annual EM MATH` tables with an `ASSESSMENT_NAME` column, pre-computed `NUM_PROF`/`PER_PROF`, tested/not-tested counts, and (math only) a 5th level for the accelerated variants. |

After latest-file-wins de-dup, the **old layout uniquely supplies `year_end`
2015–2016** and the **new layout supplies 2017–2025**. The 2016→2017 seam is purely
a *file-format* change within one testing era (see below), so it should be smooth;
the build validates it.

To make the two layouts comparable, **`num_prof` and `pct_prof` are computed the
same way for every year**: `num_prof = level3 + level4 + level5` (proficient =
Level 3 and above; level 5 exists only for the accelerated-math variants) and
`pct_prof = num_prof / num_tested × 100`. In the new-layout years this reproduces
NYSED's reported `PER_PROF` exactly (checked at build time).

**Accelerated middle-school math.** The new-layout `Annual EM MATH` table also
carries `RegentsMath6/7/8` (grade 6-8 students taking a Regents math exam, e.g.
Algebra I) and `Combined{6,7,8}Math` (grade-level + Regents combined). These are
kept and flagged via `test_type` (`grade` / `aggregate` / `regents` / `combined`).
The plain grade-level tests (`MATH4`, `MATH8`, …) are `test_type='grade'`. The old
layout has **only** the grade-level math test, so grade-8 math proficiency based on
`MATH8` alone excludes accelerated students consistently across all years — but the
mix of who takes the grade-8 test vs. a Regents exam shifts over time and across
districts, so compare `Combined8Math` when that matters.

## Suppression & text artifacts

- **Suppression** (ReadMe): assessment data are suppressed when **fewer than 5
  students** are in a subgroup; the suppressed group and the next-smallest are both
  blanked and combined into a "Small Group Total". Suppressed values appear as
  `"s"` (and occasionally blank); the builder casts these to **null**. Raw counts
  (`NUM_TESTED`, level counts) are generally present even when percents are
  suppressed.
- **Access text artifact** (ReadMe): numbers are stored as text, sometimes with a
  trailing `"."` (e.g. `"2024."`, `"703."`); the builder strips the trailing dot
  and casts to numeric.

## ⚠ Comparability across years — "seamless" is not "comparable"

The panel is **seamless** (one consistent schema and one consistently-computed
proficiency metric across all years), but NY reset its grades-3-8 tests twice
inside this window, so **proficiency rates and mean scale scores are NOT comparable
across those breaks** — they are comparable only *within* each era:

| era | years (`year_end`) | basis |
|---|---|---|
| 1 | **2015, 2016, 2017** | Common Core tests, 3 sessions |
| 2 | **2018, 2019, (2020 cancelled), 2021*, 2022** | redesigned **two-session** tests + new performance standards (2018 = new baseline) |
| 3 | **2023, 2024, 2025** | **Next Generation Learning Standards** — new tests + new standard-setting (2023 = new baseline) |

- **2017 → 2018 break:** "Due to the State's new two-session test design and
  performance standards, the 2018 results cannot be compared with prior-year
  results." (NYSED)
- **2022 → 2023 break:** the 2023 tests "represent student achievement on the new
  Next Generation Learning Standards … not comparable to those from prior years."
  (NYSED) 2024 and 2025 *are* comparable to 2023.
- **2020 (`year_end` 2020) is absent:** spring-2020 grades 3-8 tests were
  **cancelled** for COVID-19; `SRC2020` contains no assessment tables.
- **\*2021 is anomalous:** spring-2021 tests were given with greatly reduced
  participation (widespread COVID opt-outs / remote learning), so `num_tested` and
  proficiency for 2021 are **not representative** — interpret with care.

For trend reading, **mean scale score** is the most fragile across breaks; **% at
or above proficiency** is also reset at each break (the cut scores changed).
*Within* an era both are comparable.

## Processed output

[`src/build_assessments.py`](../../../src/build_assessments.py) →
`data/processed/assessments_em_ela_math.parquet` — the full seamless panel (one row
per entity × `year_end` × assessment × subgroup; districts + statewide/county/N-RC
benchmark rows; all grades 3-8, both subjects, all subgroups, plus the accelerated
math variants). A focused convenience view
`data/processed/assessments_grade4_8_all_students.csv` carries just grade 4 & 8
grade-level tests, All Students.

Key columns: `nysed_district_cd`, `entity_cd`, `entity_name`, `district_name`
(canonical, from the BOCES/N-RC table), `entity_level`, `county_name`,
`needs_rc_category`/`needs_rc_description`, `boces_name`, `year_end`, `subject`,
`grade`, `test_type`, `assessment_name`, `subgroup`, `num_tested`, tested /
not-tested counts (new years only), `level1_count`…`level5_count` (+ `_pct`),
`num_prof`, `pct_prof` (both **computed**, seamless), `total_scale_scores`,
`mean_score`, `is_computed_aggregate` (true for the synthesized 2015-16 grades-3-8
aggregate), `src_year` (which SRC file the row came from).

**Synthesized 3-8 aggregate (2015-16 only).** The old layout has no NYSED-provided
grades-3-8 aggregate row, so for 2015-16 the build reconstructs one
(`ELA3_8`/`MATH3_8`, `is_computed_aggregate=true`) by summing the grade-level tests.
Checked against NYSED's *provided* aggregate in the new-layout years (districts,
All Students):

- **ELA** — the reconstruction is essentially exact (median |diff| = 0.0 pts,
  99.4% within 0.1 pt), so the 2015-16 `ELA3_8` is reliable.
- **Math** — the reconstruction differs from NYSED's provided `MATH3_8` by a median
  ~2 pts because NYSED's math aggregate **includes accelerated grade-7/8 students
  who take a Regents math exam** (the `RegentsMath*`/`Combined*Math` variants),
  whereas a sum of the grade-level `MATH3…MATH8` tests excludes them. So the
  2015-16 `MATH3_8` reconstruction is on a slightly **narrower** basis than the
  NYSED-provided `MATH3_8` for 2017+ (the old layout had no accelerated-math
  reporting to add back). Treat the math 3-8 aggregate's 2016→2017 step with that
  in mind; the grade-level `MATH4`/`MATH8` series are unaffected.

**Suppressed `MATH3_8` level breakdown.** In the new-layout years NYSED frequently
blanks (`s`) the *level counts* of the provided `MATH3_8` aggregate while still
reporting its `num_tested` — for **~130 districts/year (~18%)**, vs. only ~2-4% for
`ELA3_8`. Because proficiency is computed from those level counts, `pct_prof` is
**null** for the math 3-8 aggregate in those district-years (e.g. Cambridge 2022,
2024). The grade-level `MATH4`/`MATH8` rows are usually still populated, so use
those (the project's focus) rather than back-filling the aggregate with a
non-matching computed value.

### Rebuild

```bash
pip install -r requirements.txt          # access-parser, polars, pyarrow
python src/download_report_card.py       # fetch the 10 SRC zips (~3.3 GB, git-ignored)
python src/build_assessments.py          # parse + stitch -> processed parquet + CSV
```

### Build results (current)

- **`assessments_em_ela_math.parquet`: 1,993,212 rows × 36 columns.**
- **Coverage:** `year_end` 2015–2019 and 2021–2025 (10 years; **2020 absent** — COVID
  cancellation). ~705–707 districts per year, plus the statewide, 63 county, and 7
  N/RC benchmark aggregates. Entity levels: district 1,772,884 rows · county 194,362
  · N/RC 22,694 · statewide 3,272.
- **`assessments_grade4_8_all_students.csv`: 30,735 rows** (grade 4 & 8 grade-level
  tests, All Students).
- `pct_prof` is bounded [0, 100] with no NaN; it is **null** where `num_tested = 0`
  or the cell is suppressed. `mean_score` is null on the 3-8 aggregate rows (see
  above) and is populated for grade-level tests only.

### Validation performed

- **Matches NYSED's published figures.** Computed statewide grades-3-8 ELA "All
  Students" proficiency = **53.2% (2025), 46.3% (2024)** vs. NYSED's published
  53% / 46%. In the new-layout years the **computed** `pct_prof` reproduces NYSED's
  reported `PER_PROF`.
- **Old↔new format seam is clean.** The 2016→2017 transition crosses the
  per-grade→`Annual EM` reader boundary *within* testing era 1. District median
  change in ELA4 proficiency 2016→2017 = **−0.2 pts** — the flattest of any
  transition (vs. +6.5 for 2015→2016, +6.1 for the 2017→2018 standards break,
  +5.9 for the 2022→2023 break, +7.6 for 2024→2025). So the dual-format stitching
  introduces no artificial step; the visible jumps fall exactly on the documented
  standard-setting breaks.
- **Era breaks visible in scale scores (as expected).** Mean scale score for a
  fixed grade jumps between eras because the scale itself was reset: e.g. grade-4
  ELA runs ~300 (2015–17), ~600 (2018–19), and grade-8 math ~600 (2021–22) → ~460
  (2023–25). This is why `mean_score` is comparable only **within** an era.
- **Synthesized 3-8 aggregate checked** against NYSED's provided aggregate in the
  new-layout years: **ELA** reconstruction is ~exact (median |diff| 0.0 pts), but
  **Math** differs ~2 pts because NYSED's math aggregate includes accelerated
  Regents math-takers (see "Synthesized 3-8 aggregate" above) — so only `ELA3_8`
  for 2015-16 is on the same basis as later years; the `MATH3_8` reconstruction is
  narrower.
