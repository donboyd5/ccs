# Conventions

The standing rules for analysis, data, and writing on `ccs`. (The data scheme
itself is specified in [`../data/README.md`](../data/README.md); this restates the
*discipline* around it.)

## Data provenance & governance

- **Hard raw/processed split.** Source data goes in `data/raw/<source>/` exactly
  as obtained; anything a script derives goes in `data/processed/`. Nothing in
  `processed/` is hand-edited — delete it and it rebuilds from `raw/` + `src/`.
- **Every raw source carries a tracked `SOURCE.md`** giving: publisher, source
  URL, coverage, time convention, the original→on-disk filename mapping, and how
  to re-download. Data blobs (csv/parquet/xls/xlsx/pdf/mdb/zip) are git-ignored;
  the `SOURCE.md` and any `src/download_*.py` are tracked, so the data is always
  reproducible.
- **Crosswalks are tracked CSVs** under `data/crosswalks/` (the parallel parquet
  is ignored). `nysed_district_cd` (8-digit) is the project's stable join key;
  each source's native id maps to it through a crosswalk.
- **Surface provenance gaps honestly.** When something can't be confirmed, mark a
  `⚠ Provenance gap` in the `SOURCE.md` and explain it — never paper over it with
  a guess. Where a fact *can* be pinned down (e.g. a time convention), verify it
  empirically and record the evidence.
- **Don't commit data blobs.** Confirm `git status` before committing; the
  `.gitignore` should already exclude them.

## Precision in claims

- Use exact, verifiable wording. Don't overstate. Verify data provenance before
  asserting it. Prefer "computed from X" / "as reported by Y" to vague attribution.
- When two series look comparable, check the time basis and definitions first
  (actual vs. proposed, headcount vs. FTE, fiscal-year alignment).

## Plain-language writing (reader-facing prose)

- Audience is the **general public**. No jargon; explain any technical term in
  everyday words the first time it appears.
- Keep reader-facing year labels consistent: **school-year-ending** (2025 = 2024–25).

## Charts

- A chart **title states just the facts** (what is plotted). The interpretation /
  takeaway goes in a **headline above the plot**, not in the title.
- Tooling: Python + **polars** (not pandas); plots via **plotnine**; tables via
  **great-tables**; outputs as **parquet** (+ CSV convenience views where useful).
  Analysis writeups are Quarto `.qmd` under `books/`.

## Source index

The reader-facing index of all data sources lives in
[`books/cambridge-comparisons/sources.qmd`](../books/cambridge-comparisons/sources.qmd).
Add a row (and a short prose section) there whenever a new dataset is adopted.
