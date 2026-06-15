# Decisions log

Newest first. Each entry: the decision, and why.

## 2026-06-15 — Book strategy: exploratory now, polished summary later

- **`books/cambridge-comparisons` is an exploratory book.** We deliberately keep
  *multiple measures per topic* rather than committing to one — e.g. for class
  size, a **median across all reported classes** *and* a **grades 3–8 ELA/Math**
  comparison; for staffing, NYSED teacher **headcount** *and* NCES **FTE**. The
  aim right now is breadth and understanding the data, not a finished narrative.
- **A polished, lay-audience summary comes later** — drawing on this exploratory
  work and using the **best measure for each chapter**. It may be a **separate,
  clean public-facing book** built on top of the shared `comparisons.py` helpers
  and processed datasets.
- **Working rule:** when we find a better measure, **add it alongside** the old
  one — do not drop the old. Keep helpers and processed datasets reusable so the
  future summary can pick the best of each.

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
