# popfc — Washington County, NY population forecast (sibling project)

The demographics chapter's **New York** population history and forecast come from
`popfc`, a sibling Python project that forecasts Washington County, NY and its
towns with a cohort-component model. Cambridge Central School District sits in
Washington County, so popfc is the natural source for the area's population trend.

| item | value |
|---|---|
| project | `popfc` — Washington County, NY Population Forecast |
| repository | <https://github.com/donboyd5/popfc> (public) |
| location | `~/Documents/code_projects/popfc/` (outside this repo) |
| version used | git commit `6a42d26` (2026-06-06); county final outputs dated 2026-05-28 / 2026-06-05 |
| methodology | [`docs/methodology.md`](file:///home/donboyd5/Documents/code_projects/popfc/docs/methodology.md) (cohort-component; plain-language) |
| data dictionary | [`docs/data_dictionary.md`](file:///home/donboyd5/Documents/code_projects/popfc/docs/data_dictionary.md) |

`popfc` is **not vendored into this repo** (its data files are git-ignored here
anyway — see `.gitignore`); `src/build_demographics.py` reads its outputs **in
place**. This means the demographics chapter rebuilds correctly only on a machine
that has `popfc` checked out at `~/Documents/code_projects/popfc/`. The path is
overridable via the `POPFC_DIR` env var.

## Files used and columns read

| popfc file | used for | columns read |
|---|---|---|
| `data_interim/population_all_sources.parquet` | NY county **pre-2000 history** (NYSDOL **July-1 intercensal** estimates, 1970–1999), spliced ahead of the reconciled series | `geoid`, `year`, `kind` (=`intercensal`), `source` (=`nysdol`), `population` (the 4 counties) |
| `data_interim/population_reconciled.parquet` | NY county **history** (Census PEP + NYSDOL, reconciled to one July-1 series per county), 2000–2025 | `geoid`, `geography`, `year`, `population` (the 4 counties) |
| `data_final/county_yearly_components.csv` | NY **history + forecast** population & components (baseline scenario); the forecast is 2025–2050 | `population` (plot 2); `births`, `deaths`, `net_mig`, `pop_change` (Washington table, forecast rows) |
| `data_interim/county_agesex_1990_2024.parquet` | Washington County **population by age, history** (single year of age × sex), grouped into under 5, 5 to 17, 18 to 64, 65 and over | `geoid` (=`36115`), `year`, `source`, `kind`, `age`, `population`. Rows used: `source = cdc_bridged` for 1990–2019; `source = census_sya`, `kind = estimate` for 2020–2024 (the April-1 `census` rows for 2020 are skipped) |
| `data_final/county_forecast_agesex.parquet` | Washington County **population by age, forecast** (baseline), 2025–2050 | `geoid`, `scenario` (=`baseline`), `year`, `age`, `population` (the file's 2024 launch row is dropped in favor of the history value) |
| `data_final/washington_components.csv` | Washington County **domestic vs international migration split** (history, by Census PEP vintage) | `v2025` vintage rows, 2020–2025: `births`, `deaths`, `domestic_mig`, `international_mig`, `net_mig`, `pop_change` |

**Pre-2000 splice basis.** The reconciled series' own rule for 2000+ is
`july1_nysdol_intercensal`, so the 1970–1999 rows take popfc's NYSDOL
**intercensal** rows (July-1), *not* the April-1 census-count rows the same
table carries at 1970/1980/1990 — one continuous July-1 series. Seam check at
1999→2000 (from `build_demographics.py`): Washington −0.1%, Warren +0.4%,
Rensselaer +0.2%, Saratoga +1.4% (Saratoga was growing ~1%/yr throughout) — all
within the band of neighboring transitions, so no definition break at the
splice.

## The four New York counties (and why these)

The chapter compares Washington County with its neighbors:

| geoid | county | state |
|---|---|---|
| 36115 | Washington | NY (Cambridge CSD is here — the focus) |
| 36113 | Warren | NY (Glens Falls; just east) |
| 36091 | Saratoga | NY (fastest-growing of the group) |
| 36083 | Rensselaer | NY (Troy / Albany metro edge) |

`popfc` also forecasts Columbia (36021) and Essex (36031); they are available
but not used by the chapter.

## Two coverage notes that shape the chapter

1. **Forecast years carry only combined net migration.** The domestic vs
   international migration split exists in `popfc` for **history** (Census PEP
   vintages, through 2025) but the **forecast** components file reports only
   `net_mig` (the split exists in the model as age/sex rates, not a clean
   forecast count). So the Washington components-of-change table shows the full
   split **2020–2025** and total net migration **2026–2035**, with a footnote.
2. **Forecast launch year is 2024; the baseline scenario is used.** `popfc`'s
   history runs through 2024 (and its reconciled series to 2025); the forecast
   runs 2025–2050. Per the user's choice, the chapter uses the **baseline**
   scenario throughout.

## Update log

- **2026-11-23** — added Washington County population by broad age group
  (`demographics_washington_age.parquet`) from the two age files listed above,
  for the age chart in the demographics chapter. Seam check from
  `build_demographics.py`: at 2019→2020, where the CDC bridged-race estimates
  meet the Census estimates benchmarked to the 2020 census, ages 5 to 17 rise
  1.2% against a trend of about −1% a year (a step of about 2%); ages 65 and
  over and 18 to 64 continue their trends; under 5 falls 2.5%. At 2024→2025,
  where history meets the forecast, all four groups continue their trends
  except under 5 (+1.2% after several years of small declines). The two
  existing datasets were rebuilt and are identical to the previous build.
  popfc itself is unchanged (still commit `6a42d26`). Rutland County, Vermont
  is still built into the county population file but is no longer charted.

- **2026-11-19** — corrected the documented location and the builder's default
  path: popfc now lives at `~/Documents/code_projects/popfc/` (it had moved from
  `python_projects/`, which broke `build_demographics.py`). popfc itself is
  unchanged (still commit `6a42d26`); the rebuilt NY series and the Washington
  components table are identical to the previous build.
