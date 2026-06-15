# Decisions log

Newest first. Each entry: the decision, and why.

## 2026-06-15 — NYSED Report Card (SRC) grades 3-8 ELA & Math assessment panel

- **Adopted the NYSED Report Card Database (SRC)** as the source for student
  performance, downloading **10 annual files (SY 2015-16 → 2024-25, `SRC2016`–
  `SRC2025`)** from data.nysed.gov. Chose the full SRC over the smaller "3-8
  Assessment Database" because the SRC also contains **Regents** and other tables
  we'll want later (no re-download needed). Raw zips (~3.3 GB) are git-ignored; the
  `REGISTRY` in `download_report_card.py` + the `SOURCE.md` are the provenance
  record. Done on **its own worktree/branch `feature/report-card-assessments`** (a
  second assistant was working `feature/cambridge-staffing-table` concurrently).
- **One seamless panel** `data/processed/assessments_em_ela_math.parquet`
  (1,993,212 rows) — one row per entity × `year_end` × assessment × subgroup.
  Built `download_report_card.py` + `build_assessments.py`.
- **Two source layouts stitched.** 2016-17 use per-grade tables (`ELA4 Subgroup
  Results` …, 4 levels, no proficiency column); 2018-25 use single `Annual EM
  ELA`/`MATH` tables. The builder reads both and **computes proficiency identically
  for every year** (`num_prof = L3+L4+L5`; `pct_prof = num_prof/num_tested`),
  reproducing NYSED's reported `PER_PROF` exactly in the new-layout years.
- **Kept districts + statewide/county/N-RC benchmark aggregates; dropped individual
  schools** (via `Institution Grouping` GROUP_CODE). `nysed_district_cd` (8-digit,
  = first 8 of the 12-digit BEDS) is the join key; canonical district name/county/
  N-RC/BOCES attached from the `BOCES and N/RC` table.
- **Scope now = grades 3-8 ELA & Math** (the user's focus is grade 4 & 8; all of
  3-8 kept since it's the same extraction, plus the accelerated-math variants
  `RegentsMath*`/`Combined*Math` flagged via `test_type`). Regents, science, etc.
  are in the same DBs and can be added later from disk.
- **"Seamless" ≠ "comparable."** NY reset its tests twice in-window, so proficiency
  & scale scores compare only within three eras (2015-17 Common Core, 2018-22
  redesign, 2023-25 Next-Gen). **2020 is absent** (tests cancelled); **2021 is
  anomalous** (~half participation). The old↔new *format* seam (2016→2017) was
  validated as continuous (district median ΔELA4-prof = −0.2 pts), confirming the
  stitching adds no artifact. Full detail + the standard-setting timeline in
  [`data/raw/nysed_report_card/SOURCE.md`](../data/raw/nysed_report_card/SOURCE.md).
- **Next planned (separate branches):** a student-performance chapter on the
  website; later, pull Regents performance from these same SRC databases.

## 2026-06-13 — PTRC panel extended back to SY 2014-15 (year_end 2015)

- **Extended the Property Tax Report Card panel from year_end 2019 → 2015** (now
  8,691 district-years, 2015–2027, 675 districts). The old 2019 floor was just where
  the download list stopped, **not a source limit** — NYSED posts files back to
  2004-05.
- **2015 is the practical floor.** 2016–2027 share the modern 35-column layout;
  **SY 2014-15 (year_end 2015)** uses an earlier 29-column layout (header on the 4th
  row, levy not yet broken out) handled by a dedicated reader (`read_2015` /
  `COLS_2015`). Its budget-year column map is **proven** by the identity
  `levy_vs_limit == proposed_levy_wo - levy_limit_wo` holding for every district.
  Earlier years are left undone: year_end ≤ 2014 use narrower pre-tax-cap layouts and
  the 2007-08…2012-13 `.xls` files crash the calamine reader → would need per-year
  parsing. The raw files are kept locally so that work can start from disk.
- **4 measures are null for 2015** (`proposed_tax_levy_to_support_budget`,
  `levy_for_library_debt`, `levy_for_nonexcludable_propositions`,
  `tax_cap_reserve_used`) — that report predates NYSED's levy breakout;
  `total_proposed_tax_levy` is populated and comparable across all years.
- **Continuity validated at both splices** (2015→2016 and 2018→2019): every major
  measure's median year-over-year change at the seam sits within the band of
  neighboring transitions, so no concept/measure definition changes across the
  extension.
- **Builder now skips non-conforming workbooks with a logged reason** instead of
  aborting; the null-vs-zero quirk in 3 sparse columns (blank in 2019 & 2021) is
  documented — **treat null ≡ 0** for those.
- Landed via **PR #2 straight to `main`** (independent of the OSC work — touches only
  the 3 PTRC files). Full provenance in
  [`data/raw/nysed_property_tax_report_card/SOURCE.md`](../data/raw/nysed_property_tax_report_card/SOURCE.md).

## 2026-06-13 — OSC school-district property-tax dataset

- **Adopted the OSC "Table 3" school-district tax workbooks, years 2015–2025.**
  2015+ is the consistent full-value-only era; 2013–2014 add assessed value +
  equalization-rate columns under a different layout and were left out for now
  (the older Excel back to 2008 and PDFs to 2001 exist on the page if ever needed).
- **One tidy long table** (`data/processed/osc_school_district_tax_levies.parquet`),
  one row per district × city/town (`record_type='detail'`) plus one per district
  (`record_type='district_total'`), rather than separate files.
- **District totals are computed from the detail rows for every year**, even though
  OSC publishes its own Total rows for 2021–2025 — because the computed sum equals
  OSC's published total to floating-point precision (verified), this gives a single
  consistent method. `total_source` flags whether OSC also published a total.
- **Consistent id = `osc_sd_muni_code`** (OSC's 12-digit ORPTS code) on every row;
  `nysed_district_cd` attached via a generated, tracked crosswalk
  (`data/crosswalks/osc_rpt_district_crosswalk.csv`). Linked 678/680; left *New
  York City* and *South Mountain-Hickory Common* unmatched **on purpose** (no
  single NYSED counterpart) rather than forcing a bad match. Matching was kept
  "within reason": normalized-name + county fallback + 9 hand-verified overrides.
- **Time convention `year_end` = OSC file-year**, verified against the PTRC panel
  (no off-by-one).
- See [`data/raw/osc_school_district_tax_levies/SOURCE.md`](../data/raw/osc_school_district_tax_levies/SOURCE.md)
  for full provenance, the three format eras, and known quirks (notably the 2019
  district-wide-rate-on-every-town-row issue, and that OSC publishes no official
  data dictionary for the school-district table).

### Next planned (separate branches)

- **Property-tax chapter.** After this database is in, add a new chapter on
  property taxes to the website — on its own new feature branch (not this one).

## 2026-06-13 — Working agreements

- Created this `meta/` folder so conventions/decisions persist across sessions.
- Set assistant autonomy to **"everything but merge to `main`"**: `acceptEdits`
  mode + a `PreToolUse` guard hook that blocks writes to `main`. See
  [`workflow.md`](workflow.md).
