# NY school-district K-12 enrollment & teachers — time series

District-level **K-12 enrollment** and **number of teachers** for every New York
public school district, built from NYSED's public downloads. Built by
[`src/build_enrollment_teachers.py`](../../../src/build_enrollment_teachers.py).

> **Provenance note.** This folder holds the **raw** NYSED downloads (in the
> `enrollment/` and `studed/` subfolders). The build script writes its **processed**
> outputs to [`data/processed/`](../../processed/). Raw archive/database filenames
> are kept **exactly as downloaded from NYSED** (e.g. `ENROLL_2024.mdb`,
> `studed2022.zip`) — not renamed.

| metric | source dataset | coverage (school years) |
|---|---|---|
| K-12 enrollment | NYSED **Enrollment** DB, table `BEDS Day Enrollment` (2017-18→2025-26) **+ Report Card (SRC)** DBs SRC2005-17, same table (2005-06→2016-17) | **2005-06 → 2025-26** (22 yrs) |
| number of teachers (+ principals, counselors, social workers, turnover) | NYSED **Student & Educator** ("STUDED") database, table `Staff` | **2017-18 → 2024-25** (8 yrs) |
| county / BOCES / Need-Resource-Capacity crosswalk | Enrollment database, table `BOCES and N/RC` | — |

Source page: https://data.nysed.gov/downloads.php
(9 annual `enrollment_*.zip` and 8 annual `studed*.zip` archives, each an MS-Access
`.mdb`/`.accdb`). The raw archives/databases live next to this file in
`enrollment/` and `studed/` and are git-ignored (too large for GitHub); re-download
from the source page above into those folders. Each zip holds a date-stamped
`.mdb` (e.g. `ENROLL2026_20261107.mdb`); extract it and save it as
`ENROLL_<year>.mdb` / `STUDED_<year>.mdb`, the names the builder globs for.

> ⚠ **Enrollment is one year ahead of staff.** NYSED posted the 2025-26
> Enrollment database on 2026-11-09 (`ENROLLMENT_2026.zip`, direct URL
> `https://data.nysed.gov/files/enrollment/25-26/ENROLLMENT_2026.zip`). The
> 2025-26 Student & Educator database was **not yet posted** as of 2026-11-19, so
> `year_end = 2026` rows have enrollment but `num_teachers = null` (no
> students-per-teacher, no class size).

### Pre-2018 enrollment comes from the Report Card (SRC) databases

NYSED's standalone Enrollment database begins school year **2017-18** (year_end
2018); for the earlier years the same BEDS-day K-12 count lives inside the
statewide **Report Card (SRC)** Access databases. `build_enrollment_teachers.py`
reads **SRC2005-SRC2017** (in `../nysed_report_card/zips/`, downloaded by
`src/download_report_card.py`) for year_end **2005-2017** and the Enrollment DB
for **2016-2026**; the two overlap at 2016-17 and latest-source-wins keeps the
Enrollment DB there. The SRC enrollment table is the *same* `BEDS Day Enrollment`
table in the 12-digit-coded files (SRC2005 uses `bedscode` + zero-padded grades
`01`..`12`; SRC2006-17 use `ENTITY_CD` + `1`..`12`), so K-12 is computed once as
K + grades 1-12 + ungraded (excl. PK) for every year — matching NYSED's own
precomputed `K12` column exactly (max |Δ| = 0).

**District rows only.** The SRC `BEDS Day Enrollment` table also holds
statewide / county / Need-Resource-Capacity aggregate rows, some of which end in
`0000` and would leak past the `…0000` district filter. The build keeps only true
district rows via each DB's `Institution Grouping` table (`GROUP_CODE` 5) and
drops charters (LEA type 86). A side effect: SRC-era years have no retained
statewide/charter row, so the build's reconciliation diagnostic shows
`statewide_k12 = 0` (and a large negative residual) for year_end < 2016 — the
**district sum** is the figure of record and is correct.

**Seam validated.** SRC vs Enrollment DB agree exactly at the 2016-17 overlap —
max |ΔK12| = **0.0** across ~2,884 district-cells. Statewide district K-12:
**2.78M (2005) → 2.52M (2016) → 2.24M (2025) → 2.19M (2026)**.

> ⚠ **Not used: SRC2000-2004 (year_end 1997-2004).** The oldest SRC files use a
> 6-digit district code (`GRP_CD`/`DISTRICT_CD`) and a different table layout
> (long-format for SRC2000), with an unresolved `YEAR` convention (start- vs
> end-year) and a likely gap at year_end 2000. They are downloaded and kept on
> disk but not parsed. See "Extending further back" below.

## Time convention

`year_end` = the **school-year-end (spring) year**, per NYSED's `YEAR` field
("2024 == 2023-24"). So `year_end = 2025` is school year **2024-25**.
Enrollment is the **BEDS-day** count (first Wednesday of October).

**Numerator and denominator share the same fall snapshot.** Teacher (and other
staff) counts come from NYSED's **Staff Snapshot**, reported through SIRS as of
**BEDS Day** — the same first-Wednesday-in-October date as enrollment. So
`k12_students_per_teacher` (and its reciprocal, teachers per 100 students) is a
**point-in-time fall ratio, not a full-year average**: enrollment on BEDS Day
÷ teacher headcount on BEDS Day. `NUM_TEACH` is a **headcount, not an FTE**.
> ⚠ **Provenance note (well-supported, not formally certified).** That staff
> counts are a BEDS-Day snapshot is documented in NYSED's BEDS-PMF / Staff
> Snapshot reporting framework, but NYSED's short public glossary does not
> restate the exact "as-of" date for the *published* "Number of Teachers"
> figure. Treat the BEDS-Day alignment of teachers with enrollment as
> well-supported but not certified to the day.
> *Sources:* [NYSED Report Card glossary](https://data.nysed.gov/glossary.php?report=reportcards),
> [Student & Educator glossary](https://data.nysed.gov/glossary.php?report=studed),
> [BEDS-PMF Teacher/Staff Data](https://www.p12.nysed.gov/irs/beds/PMF/home.html).

**Class size is measured differently.** NYSED's `Average Class Size` table (in
the STUDED database, beginning 2018-19) is computed from actual course rosters —
students enrolled in specified sections ÷ number of sections, for K-2 and
assessment-aligned courses — **not** from any teacher count, headcount or FTE.
It is the better source for true class size; the teacher-based ratios here are a
staffing measure, not class size.

## Output files

| file | grain | notes |
|---|---|---|
| `district_enrollment_teachers_panel.parquet` / `.csv` | one row per district × `year_end` | **primary deliverable** — enrollment + teachers + ratio + county/N-RC |
| `enrollment_k12_by_district.parquet` | district × year | K-12 total **plus full grade detail** (PK…12, ungraded) |
| `teachers_by_district.parquet` | district × year | staff counts + teacher attendance/turnover |
| `class_size_by_district.parquet` | district × year × **reported class** | NYSED Average Class Size, long form (see below) |
| `washington_area_enrollment_teachers.csv` | district × year | convenience subset: Washington Co. + neighbors (Warren, Saratoga, Rensselaer, Essex) |

### `class_size_by_district.parquet`

Long: one row per `district_cd` × `year_end` × `class_description`
(`entity_cd`, `district_name`, `average_class_size`, `acs_source_year`). From the
STUDED `Average Class Size` table; **roster/section-based** (students in a
section ÷ number of sections), **not** teacher-derived (see above). Only rows
NYSED flags `DATA_REPORTED='Y'` with a value are kept.

**Stable taxonomy columns** (added by `classify_class()` in the builder, since
NYSED's raw labels drift across years): `class_canonical` (one name per class,
collapsing the 2019-20 vs 2021+ grade/subject relabel and the Regents
`(Common Core)`/`(Framework)` suffixes), `class_tier`
(`elementary` = self-contained K/1/2; `grades_3_8` = ELA/Math/Science by grade;
`high_school` = Regents courses), `class_subject` (ELA/Math/Science/Social
Studies/Self-contained), and `class_grade` (`K`,`1`…`8`; null for HS). These let
you pick a like-for-like basket — e.g. grades 3-8 ELA+Math, or the HS Regents
core — instead of mixing elementary with high-school electives (see
`cc.class_size_for`). All 50 raw labels map cleanly (0 land in `tier='other'`).

- **Coverage `year_end` 2019–2025** (school years 2018-19 → 2024-25). The pre-2018-19
  era is a **different format and method**: `STUDED_2018.mdb` stores this table in a
  *wide* layout (`COMMON_BRANCH`, `GRADE_8_MATH`, … ; years 2016-2018) collected from
  teacher forms. That file is **skipped with a logged reason** (non-comparable); all of
  2019-2025 lives in the modern long-layout files.
- **No single overall figure.** NYSED reports class size only for specific classes
  (Kindergarten, Grades 1–2, and grade/subject courses aligned to State tests, ~18–27
  per district-year). A per-district "average class size" must be **aggregated
  downstream** — the book uses the **median** across reported classes (resists
  small-section outliers); see `cc.class_size_median_for`.
- **Small-N caution.** Values are NYSED-rounded to integers and volatile for small
  districts (a lightly-enrolled elective swings them). **2020–21 (`year_end` 2021)
  reporting is disrupted** — e.g. Salem's median drops to 8 that year (a pandemic
  artifact, not a real change).

### Panel columns

| column | meaning |
|---|---|
| `district_cd` | 8-digit NYSED district code — **stable join key** |
| `entity_cd` | 12-digit BEDS code of the district-total row (`district_cd` + `0000`) |
| `district_name` | district name, **standardized to its most recent reported name** |
| `county_cd`, `county_name` | county of district location |
| `needs_rc_category` (1-7), `needs_rc_description` | NYSED Need/Resource-Capacity peer group (see below) |
| `boces_name` | BOCES the district belongs to |
| `washington_area` | `true` for the 5-county convenience region |
| `year_end` | school-year-end year |
| `k12_enrollment` | **K-12 enrollment** (K through 12 incl. ungraded elem./secondary; **excludes PK**) |
| `num_teachers` | NYSED "total number of teachers" (district total) |
| `k12_students_per_teacher` | `k12_enrollment / num_teachers` (approximate — see caveats) |
| `num_principals`, `num_counselors`, `num_social_workers` | other staff counts |
| `pct_teacher_attendance`, `pct_teacher_turnover` | from the `Staff` table |

### Need/Resource-Capacity categories (`needs_rc_category`)
`1` High Need: NYC · `2` High Need: Large City (Buffalo/Rochester/Syracuse/Yonkers) ·
`3` High Need: Urban-Suburban · `4` High Need: Rural · `5` Average Need ·
`6` Low Need · `7` Charter. *Cambridge CSD = 5 (Average Need).*

## Comparability — what's clean and what to watch

**Directly comparable within each series.** All 9 enrollment files share an
identical schema, as do all 8 STUDED files. Each annual file repeats ~3 years of
data; across the overlapping years the values are **identical (0 revisions)**, so
stacking + de-duplicating (latest file wins) is lossless.

Things to keep in mind:

1. **Different spans.** Enrollment covers 2005-06→2025-26; teachers only
   2017-18→2024-25. In the panel, pre-2018 rows **and 2025-26 rows** have
   `num_teachers = null` (and therefore no `k12_students_per_teacher`).

2. **"By district" excludes charter schools.** Charters have no district-total
   (`…0000`) row, so they are absent from this district series. Reconciliation to
   NYSED's published statewide K-12 total (which *includes* charters):

   | year_end | statewide K-12 | district sum (here) | charters | residual |
   |---:|---:|---:|---:|---:|
   | 2017 | 2,629,970 | 2,501,186 | 128,784 | 0 |
   | 2020 | 2,581,069 | 2,421,858 | 159,211 | 0 |
   | 2025 | 2,421,491 | 2,235,044 | 186,447 | 0 |
   | 2026 | 2,382,001 | 2,192,533 | 189,468 | 0 |

   (Figures as printed by the 2026-11-19 build; the district sum reconciles to
   the statewide total exactly.) **Charter enrollment grew 129K→189K** while
   district enrollment fell, so the district-only series declines faster than
   the all-public total.

3. **District reorganizations.** A merger creates a new `district_cd` and retires
   the predecessors. In this window, statewide only **6** districts have partial
   coverage. Local example: **Boquet Valley CSD** (Essex) was formed in 2019-20 by
   merging **Westport** + **Elizabethtown-Lewis** (which end in 2018-19). The
   2025-26 file adds another: **Rockland CSD** (Sullivan, `59130304`, 533 K-12
   students) replaces **Livingston Manor** (385 in 2024-25) + **Roscoe** (186),
   which have no 2025-26 rows.

4. **`num_teachers` is a headcount, not an FTE,** and counts *all* teachers in the
   district (all grades), while `k12_enrollment` is K-12 only (excludes PK).
   `k12_students_per_teacher` is therefore an approximation, not an official
   pupil-teacher or class-size ratio. NYSED's "Average Class Size" table (in
   STUDED) is the better source for class size if needed.

5. **COVID.** District K-12 fell ~3.3% in a single year (2,421,858 → 2,342,473
   between 2019-20 and 2020-21); part real decline, part pandemic un-enrollment.

6. **Access text artifact.** NYSED stores numbers as text (e.g. `"703."`); the
   build strips the trailing dot and casts to numeric.

## Update log

- **2026-11-19** — added `ENROLL_2026.mdb` (school year 2025-26, NYSED file dated
  2026-11-07). K-12 enrollment for every earlier district-year is **unchanged**
  from the previous build. Cambridge K-12: 741 (2024-25) → **713** (2025-26).
  Crosswalk changes carried by the new file (latest file wins): Boquet Valley CSD
  N/RC 5 → 4; Randolph Academy UFSD BOCES name. The builder's crosswalk
  de-duplication now prefers a row with a non-blank `NEEDS_INDEX` (the 2026 file
  has 10 school rows with a blank code, which had nulled the category for four
  districts). The reconciliation table above was refreshed from the build output.

## Rebuild

```bash
pip install -r requirements.txt          # adds access-parser
python src/build_enrollment_teachers.py  # re-reads raw/*.mdb, rewrites outputs
```

## Extending further back

**Enrollment is now built back to 2005-06** via SRC2005-SRC2017 (see above).
Going further back to **1999-00 (year_end 2000)** is possible — SRC2000-SRC2004
are downloaded in `../nysed_report_card/zips/` — but those files use a 6-digit
district code (`GRP_CD`/`DISTRICT_CD`, needing a crosswalk to the 8-digit
`nysed_district_cd`) and a different table layout (SRC2000 is long-format, one
row per grade). They also have an unresolved `YEAR` time convention (the file
"SRC2000" carries `YEAR` 1997-99 — ambiguous whether that is year_end 1997-99 or
1998-2000) with a likely gap at year_end 2000. Readable by `access-parser` but
left undone deliberately (low marginal value: nothing else in the book predates
2013, and the 2005-2025 series already spans two decades). Teachers before
2017-18 are not available in these tables.
