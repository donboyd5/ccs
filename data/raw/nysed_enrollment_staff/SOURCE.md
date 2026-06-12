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
| K-12 enrollment | NYSED **Enrollment** database, table `BEDS Day Enrollment` | **2015-16 → 2024-25** (10 yrs) |
| number of teachers (+ principals, counselors, social workers, turnover) | NYSED **Student & Educator** ("STUDED") database, table `Staff` | **2017-18 → 2024-25** (8 yrs) |
| county / BOCES / Need-Resource-Capacity crosswalk | Enrollment database, table `BOCES and N/RC` | — |

Source page: https://data.nysed.gov/downloads.php
(8 annual `enrollment_*.zip` and 8 annual `studed*.zip` archives, each an MS-Access
`.mdb`/`.accdb`). The raw archives/databases live next to this file in
`enrollment/` and `studed/` and are git-ignored (too large for GitHub); re-download
by hand from the source page above into those folders.

## Time convention

`year_end` = the **school-year-end (spring) year**, per NYSED's `YEAR` field
("2024 == 2023-24"). So `year_end = 2025` is school year **2024-25**.
Enrollment is the **BEDS-day** count (first Wednesday of October).

## Output files

| file | grain | notes |
|---|---|---|
| `district_enrollment_teachers_panel.parquet` / `.csv` | one row per district × `year_end` | **primary deliverable** — enrollment + teachers + ratio + county/N-RC |
| `enrollment_k12_by_district.parquet` | district × year | K-12 total **plus full grade detail** (PK…12, ungraded) |
| `teachers_by_district.parquet` | district × year | staff counts + teacher attendance/turnover |
| `washington_area_enrollment_teachers.csv` | district × year | convenience subset: Washington Co. + neighbors (Warren, Saratoga, Rensselaer, Essex) |

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

**Directly comparable within each series.** All 8 enrollment files share an
identical schema, as do all 8 STUDED files. Each annual file repeats ~3 years of
data; across the overlapping years the values are **identical (0 revisions)**, so
stacking + de-duplicating (latest file wins) is lossless.

Things to keep in mind:

1. **Different spans.** Enrollment covers 2015-16→2024-25; teachers only
   2017-18→2024-25. In the panel, 2016 & 2017 rows have `num_teachers = null`.

2. **"By district" excludes charter schools.** Charters have no district-total
   (`…0000`) row, so they are absent from this district series. Reconciliation to
   NYSED's published statewide K-12 total (which *includes* charters):

   | year_end | statewide K-12 | district sum (here) | charters | residual\* |
   |---:|---:|---:|---:|---:|
   | 2016 | 2,640,250 | 2,516,429 | 117,617 | 6,204 |
   | 2020 | 2,581,069 | 2,416,201 | 159,211 | 5,657 |
   | 2025 | 2,421,491 | 2,229,960 | 186,447 | 5,084 |

   \*Residual ≈ state-operated / special-act schools that also lack a district row.
   **Charter enrollment grew 118K→186K** while district enrollment fell, so the
   district-only series declines faster than the all-public total.

3. **District reorganizations.** A merger creates a new `district_cd` and retires
   the predecessors. In this window, statewide only **6** districts have partial
   coverage. Local example: **Boquet Valley CSD** (Essex) was formed in 2019-20 by
   merging **Westport** + **Elizabethtown-Lewis** (which end in 2018-19).

4. **`num_teachers` is a headcount, not an FTE,** and counts *all* teachers in the
   district (all grades), while `k12_enrollment` is K-12 only (excludes PK).
   `k12_students_per_teacher` is therefore an approximation, not an official
   pupil-teacher or class-size ratio. NYSED's "Average Class Size" table (in
   STUDED) is the better source for class size if needed.

5. **COVID.** District K-12 fell ~3.3% in a single year (2,416,201 → 2,337,046
   between 2019-20 and 2020-21); part real decline, part pandemic un-enrollment.

6. **Access text artifact.** NYSED stores numbers as text (e.g. `"703."`); the
   build strips the trailing dot and casts to numeric.

## Rebuild

```bash
pip install -r requirements.txt          # adds access-parser
python src/build_enrollment_teachers.py  # re-reads raw/*.mdb, rewrites outputs
```

## Extending further back (not yet built)

Teachers before 2017-18 and enrollment before 2015-16 are available in the NYSED
**Report Card (SRC)** Access databases (`reportcards/` + `essa/` on the downloads
page, **1999-00 → 2024-25**). These would extend the series to match the finance
data (2013-2025) but use a different schema and definitions (a documented
noncomparability) and are large (~300 MB/yr for recent years).
