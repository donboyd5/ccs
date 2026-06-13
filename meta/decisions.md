# Decisions log

Newest first. Each entry: the decision, and why.

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
