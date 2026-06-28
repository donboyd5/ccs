# Decisions log

Newest first. Each entry: the decision, and why.

## 2026-06-28 — Mergers chapter (state-aid dataset; research-sourced)

- **Filled in `front/mergers.qmd`** (was a 9-line outline) — the first Options
  chapter. Factual scaffolding only; **no recommendation** (Don writes the
  verdict). Covers: the three reorganization forms (centralization, annexation,
  city annexation) + tuitioning; the ~year-long process (straw vote →
  Commissioner's order → statutory vote); the centralization-vs-annexation **vote
  asymmetry** (centralization needs yes in both districts; annexation final after
  60 days, one yes carries it); **RIOA** (40% of combined Foundation Aid for 5
  years, −4 pts/yr, 14-year cap; the 2024 change pegged it to Foundation Aid and
  ~tripled it) + Building Aid; the Cambridge illustration; two real cases
  (Central Valley/Ilion-Mohawk 2013 ≈$42M, now at its aid cliff; Boquet
  Valley/Elizabethtown-Lewis-Westport 2019, $6M old vs $18M new formula); the
  research (savings largest for smallest districts, mixed academic effects); and
  Cambridge's position.
- **Adopted the NYS state-aid dataset** (`data.ny.gov 9pb8-dg53`) — flipped from
  Candidate to **In use**. `src/download_state_aid.py` pulls the 13 comparison
  districts (via Socrata API, `beds_code` = `nysed_district_cd[:6]`, so no new
  crosswalk); `src/build_state_aid.py` → `data/processed/state_aid.parquet`. Used
  for **Foundation Aid** (Cambridge $9.55M, 2025-26 — the RIOA base) and to show
  every comparison district's **reorganization-incentive aid is $0** (none has
  merged). Provenance in `data/raw/state_aid/SOURCE.md`.
- **The RIOA chart is illustrative, not actual:** it applies the formula to
  Cambridge's *own* Foundation Aid as a base, so it understates a real
  Cambridge-centered merger (which would use the larger combined base). Labeled
  as such in the caption and prose; the "long-dormant limit" caveat is noted.
- **Sources cited inline** (NYSED Primer 2024, NYSSBA *On Board* Apr 2025, the
  consolidation research review) rather than added to `references.bib` — a web
  chapter; prose links + a Sources footer, matching the `taxes.qmd` style.

## 2026-06-28 — Demographics chapter (popfc + Census PEP)

- **Filled in `front/demographics.qmd`** (was a stub with a bracketed instruction)
  — two **indexed** county-population charts and a Washington County
  components-of-change table. Per the user's instruction, **no interpretive text**
  was written (he writes it); only factual chart/table introductions and captions.
  Plot 1: total population indexed to 2000 (=100), 2000–2020, for Washington,
  Warren, Saratoga, Rensselaer (NY) + Bennington & Rutland (VT). Plot 2: indexed
  to 2020 (=100), 2020–2035, NY counties only, with **history solid / forecast
  dashed**. Washington County (Cambridge's county) is the red focus line.
- **New York data read in place from the sibling `popfc` project**
  (`~/Documents/python_projects/popfc`, commit `6a42d26`). History = its
  `population_reconciled` series (Census PEP v2025 / NYSDOL, 2000–2024); forecast
  = `county_forecast_totals` baseline (2025–2050). Per `.gitignore`, raw data
  under `data/` is not tracked, so popfc is **cited, not vendored** —
  `build_demographics.py` reads its working tree (path overridable via
  `POPFC_DIR`); provenance in `data/raw/popfc/SOURCE.md`. This makes the
  demographics rebuild depend on popfc being present locally.
- **Vermont counties fetched fresh from the Census Bureau** (Bennington/Rutland
  are not in popfc). `src/download_census_vt.py` stitches three PEP sources, all
  July-1: the 2000s intercensal **API** (2000–2009, needs a `CENSUS_API_KEY`) +
  the key-free bulk files `co-est2019-alldata.csv` (2010–2019) and
  `co-est2024-alldata.csv` (2020). Output: `data/raw/census_pep/vt_county_population.csv`.
- **Three honest limits surfaced in the chapter:** (1) the **domestic vs
  international migration split exists only for history (through 2025)**; the
  forecast carries only net migration — so the Washington components table shows
  the full split 2021–2025 (—"—" for 2026–2035). User chose **option (a)**.
  (2) **2020 is omitted from the components table**: the 2020 Census boundary
  makes PEP's first vintage year a partial April–July period (births ≈ 142, not a
  full year). (3) the **VT 2020 uptick** is the 2020 Census rebenchmark (the
  census counted more than prior estimates) — noted in the chart caption.
- **Convention note:** demographics is the book's only **calendar-year** chapter
  (July-1 population estimates); all other chapters use school-year-ending. The
  forecast uses the **baseline** scenario. Sources appendix + methods appendix
  updated accordingly.

## 2026-06-28 — Methods appendix

- **Filled in `appendix/methods.qmd`** (was a TODO stub) — a reader-facing
  reference consolidating the measure definitions, caveats, year conventions, and
  cross-checks that were scattered across the front chapters. Sections: conventions
  (school-year-ending label + the vote-year exception; comparison groups; nominal
  dollars; the `nysed_district_cd` join key); measure definitions + caveats by
  topic (enrollment/teachers, class size, test results, spending, taxes, tax cap,
  reserves, contingency budgets); cross-checks (NYSED headcount vs NCES FTE;
  contingency defeats vs NYSED totals; OSC ST-3 vs audited FAC; PTRC % Yes); and
  limitations (no inflation adjustment; small-district volatility; per-pupil scale
  effect; name-based vote matching; era/tax-cap breaks).
- Lets the front chapters stay narrative while pointing here for the "how." Dataset
  *provenance* stays in the Sources appendix + per-source `SOURCE.md` files; this
  appendix covers *how measures are defined and where comparisons break down*.

## 2026-06-28 — Reserves & fund-balance chapter

- **Filled in `front/reserves.qmd`** (was a stub) — the reserve-funds vs. fund-
  balance distinction, the **4% cap** on unassigned fund balance, a statewide
  median-vs-cap chart, a Cambridge reserves-by-type table (2013–2025), and what
  Cambridge's audited reports show. Sits in the "CCS finances" arc.
- **Added a `docs/` folder** of primary-source reference manuals (tracked): OSC's
  *Accounting and Reporting Manual for School Districts* (the Uniform System of
  Accounts / chart of accounts, with the reserve-fund codes A814–A884 and
  fund-balance classes A913–A917) and NYSED's *Guide to the Headings of the
  Fiscal Profile* (defines the 4%-capped **Total Unexpended Surplus Funds,
  AT0994**). README notes provenance/source URLs.
- **Key distinction explained:** reserve funds are the *restricted* part of fund
  balance; the 4% cap (RPTL §2-c / Ed Law §2023-a) applies only to the
  *unassigned* portion — so a district can hold large restricted reserves and
  still be "at the cap" on its flexible balance.
- **Findings:** the **statewide median district reports exactly 4%** unrestricted
  fund balance every year (the cap binds), with ~¼–⅓ over 4%; **Cambridge tracks
  right at the cap** (3.0–4.6%). Cambridge's total General Fund balance roughly
  **doubled ($2.5M→$5.2M, 2013→2025)**, adding TRS-retirement, employee-benefit,
  and capital reserves; the debt reserve retired. The 2024–25 audit flagged
  **tax-certiorari reserve over-funding** (GML §3651.1-a; corrected).
- **Data sources:** PTRC `fund_balance_unrestricted_pct_of_budget` (statewide),
  `cambridge_finance.parquet` reserve A-codes (Cambridge time series), and the
  FAC annual reports (audited snapshot + findings). OSC ST-3 and audited figures
  reconcile (~$2.94M vs $2.95M restricted, 2024–25). Read parquets directly in the
  qmd (the `taxes.qmd` pattern); no `comparisons.py` change. Interpretation kept
  light.

## 2026-06-27 — Tax-cap chapter (PTRC data) + a sign-correction

- **Added `front/tax-cap.qmd`** (the property-tax cap), the first chapter to use
  the built-but-unused `property_tax_report_card.parquet` (NYSED PTRC: proposed
  levy, levy **limit**, permissible exclusions, fund balances). It completes the
  finances arc — `spending → taxes → tax-cap → reserves → contingency-budgets` —
  and gives the contingency chapter the levy/limit context it lacked. Added
  `load_ptrc()` + `ptrc_for()` to `comparisons.py`; placed after `taxes` in
  `_quarto.yml`; flipped the PTRC row in `sources.qmd` to **In use**.
- **Found and documented a sign bug in the stored `levy_vs_limit_wo_exclusions`
  field:** NYSED **flips its sign at year_end 2016** (2015 = `proposed − limit`,
  2016+ = `limit − proposed`), so it is **not sign-comparable across years**. The
  chapter computes the gap itself (`proposed − limit`, positive = over). Corrected
  the (2015-only) identity claim in `build_ptrc.py`, the PTRC `SOURCE.md`, and
  this file. (The 2015 identity is valid *for 2015*; the cross-year flip is the
  gotcha.)
- **Exclusions nuance:** a district whose proposed levy is above its base limit
  may still be **within the cap** once permissible exclusions (capital, pension,
  PILOT) are applied. The chapter's "over the cap" measure is `proposed − limit −
  permissible_exclusions > 0` (3–23 districts/yr statewide), and it defers to the
  vote-file 60%-flag for actual override outcomes — consistent with the
  contingency chapter.
- **Cambridge:** at or **under** its levy limit every year 2014–15 → 2025–26
  (proposing right at the cap), then for **2026–27 proposed ~$1.1M (≈12%) over**
  the limit — ~$0.5M over even after exclusions, which would require 60% approval.
  Interpretation kept light (Don writes the stakes).

## 2026-06-27 — NYSED budget vote / contingency-budget dataset (R→Python port)

- **Adopted NYSED's statewide *School District Budget Voting Results*** (May vote
  + June re-vote, budget-span years 2013–2026 → vote calendar years 2012–2025) as
  the source for the **contingency-budgets** chapter. Built the standard pipeline:
  [`src/download_budget_votes.py`](../src/download_budget_votes.py) (exact-URL
  `REGISTRY`) + [`src/build_budget_votes.py`](../src/build_budget_votes.py) →
  `data/processed/budget_votes.parquet`. The 27 raw files were already on disk;
  the gap this closes is **reproducibility from tracked code** (they were
  previously downloaded inline by a standalone R notebook).
- **Faithful R→Python port of the heuristic layout detector.** NYSED changes the
  headers/format/layout every year, so each sheet is reduced to a schema
  (`year`, `kind`, `district`, `district_key`, `yes`, `no`, `above_cap`) by
  inferring the name / Yes-No / 60%-flag columns. Two fixes beyond the original R:
  (a) reject a "SCHOOL DISTRICT" column whose data is mostly numeric (a merged
  title over a leading code column had fooled it on the 2014 re-vote); (b) drop
  `FIRST VOTE:` / `REVOTE:` section-label rows the sectioned re-vote files leak.
- **Validated against NYSED's own statewide totals:** computed first-vote defeat
  counts match NYSED's published P/D statewide totals **exactly for 2021–2025**
  (within 1 for the anomalous 2020 COVID year); the `% Yes` reconciliation is
  0.0000 wherever a `% Yes` column exists. Stronger than the source notebook's
  vague "19" claim (which was the 2024 figure).
- **Names, not codes; no levy data.** These files carry district *names* and only
  a "60% required" flag — **no BEDS codes, no levy figures** — so they are **not
  joinable** to the `nysed_district_cd` crosswalk. A normalized `district_key`
  (upper-cased, suffix-stripped) links a first vote to its re-vote across NYSED's
  inconsistent name forms (~100% from 2020 on, best-effort older). `year` is the
  **vote calendar year** (May 2024 → 2024), one off from the book's
  school-year-ending label — documented in the chapter.
- **Local findings surfaced in the chapter:** Cambridge's budget has **passed on
  the first vote every year (2012–2025)** (closest call 2023, 53%); Washington
  County neighbor **Fort Edward** went to contingency in **both 2018 and 2020**;
  **Salem** failed May 2024 at ~59% (above the cap, needing 60%) and passed the
  June re-vote at 71%. Chapter stays in `front/` (statewide/exploratory in
  flavor); Don may later relocate or focus it more on Cambridge.
- **Fixed the reorg's stale-`comparisons.py` import.** Moving chapters into
  `front/`-style subdirs had been coped with by **copying** `comparisons.py` into
  each subdir, which silently went stale (the copies lacked new helpers). Replaced
  the `front/`, `exploratory/`, and `appendix/` copies with **symlinks to the book
  root `comparisons.py`** — one source of truth, the fix the reorg plan itself
  suggested. Verified all sibling chapters still render.
- See [`data/raw/nysed_budget_votes/SOURCE.md`](../data/raw/nysed_budget_votes/SOURCE.md)
  for full provenance, the URL registry, and known data-quality notes.

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
  `COLS_2015`). Its budget-year column map is **proven** (for year_end 2015) by the
  identity `levy_vs_limit == proposed_levy_wo - levy_limit_wo` holding for every
  district. ⚠ **Sign gotcha (found 2026-06-27):** NYSED **flips the sign** of
  `levy_vs_limit_wo_exclusions` from year_end 2016 on (2015 = `proposed − limit`,
  2016+ = `limit − proposed`), so it is **not sign-comparable across years** —
  compute `proposed − limit` from the component columns.
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
