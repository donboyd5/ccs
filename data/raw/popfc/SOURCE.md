# popfc — Washington County, NY population forecast (sibling project)

The demographics chapter's **New York** population history and forecast come from
`popfc`, a sibling Python project that forecasts Washington County, NY and its
towns with a cohort-component model. Cambridge Central School District sits in
Washington County, so popfc is the natural source for the area's population trend.

| item | value |
|---|---|
| project | `popfc` — Washington County, NY Population Forecast |
| location | `~/Documents/python_projects/popfc/` (outside this repo) |
| version used | git commit `6a42d26` (2026-06-06); county final outputs dated 2026-05-28 / 2026-06-05 |
| methodology | [`docs/methodology.md`](file:///home/donboyd5/Documents/python_projects/popfc/docs/methodology.md) (cohort-component; plain-language) |
| data dictionary | [`docs/data_dictionary.md`](file:///home/donboyd5/Documents/python_projects/popfc/docs/data_dictionary.md) |

`popfc` is **not vendored into this repo** (its data files are git-ignored here
anyway — see `.gitignore`); `src/build_demographics.py` reads its outputs **in
place**. This means the demographics chapter rebuilds correctly only on a machine
that has `popfc` checked out at `~/Documents/python_projects/popfc/`. The path is
overridable via the `POPFC_DIR` env var.

## Files used and columns read

| popfc file | used for | columns read |
|---|---|---|
| `data_interim/population_reconciled.parquet` | NY county **history** (Census PEP + NYSDOL, reconciled to one July-1 series per county), 2000–2025 | `geoid`, `geography`, `year`, `population` (the 4 counties) |
| `data_final/county_yearly_components.csv` | NY **history + forecast** population & components (baseline scenario); the forecast is 2025–2050 | `population` (plot 2); `births`, `deaths`, `net_mig`, `pop_change` (Washington table, forecast rows) |
| `data_final/washington_components.csv` | Washington County **domestic vs international migration split** (history, by Census PEP vintage) | `v2025` vintage rows, 2020–2025: `births`, `deaths`, `domestic_mig`, `international_mig`, `net_mig`, `pop_change` |

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
