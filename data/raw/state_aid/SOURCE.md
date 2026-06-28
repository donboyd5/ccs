# NYS — State Aid to School Districts (data.ny.gov `9pb8-dg53`)

New York publishes each district's state school aid, by **aid category**, under
each year's enacted budget. The [mergers](../../../books/cambridge-comparisons/front/mergers.qmd)
chapter uses **Foundation Aid** — the base against which **Reorganization
Incentive Operating Aid (RIOA)** is calculated after a merger — and the two
reorganization-incentive line items themselves.

| item | value |
|---|---|
| publisher | New York State (data.ny.gov open-data portal) |
| dataset | [State Aid to School Districts](https://data.ny.gov/Education/State-Aid-to-School-Districts/9pb8-dg53) (`9pb8-dg53`) |
| coverage | enacted budgets **1996-97 → 2025-26** (this project pulls the comparison districts only) |
| grain | one row per **enacted-budget event × district × aid category** |
| fetched by | [`src/download_state_aid.py`](../../../src/download_state_aid.py) → [`src/build_state_aid.py`](../../../src/build_state_aid.py) → `data/processed/state_aid.parquet` |

## Schema and conventions

Each row carries `event` (e.g. "2025-26 Enacted Budget"), `beds_code` (6-digit),
`county`, `district`, `aid_category`, `base_year` (prior year's amount),
`school_year` (the enacted amount for that year), and `change` / `pct_change`.
The build keeps `amount` = the enacted `school_year` figure and parses
`year_end` = the calendar year the enacted school year ends ("2025-26" → 2026),
matching the rest of the project.

The state-aid `beds_code` (e.g. Cambridge `641610`) equals the first six digits
of the project's `nysed_district_cd`, i.e. the `beds6` already in the district
crosswalk — so the panel joins cleanly via the crosswalk (no new crosswalk
needed). The download pulls only the **13 comparison districts** (Cambridge +
every Washington County K-12 district + Schuylerville + Hoosick Falls); the raw
extract is git-ignored, regenerable from the tracked script.

## Aid categories in use

The dataset carries ~80 categories. The mergers chapter uses:

- **Foundation Aid** — the RIOA base. Cambridge's is $9.55M (2025-26).
- **Operating Reorganization Incentive Aid** — RIOA itself; **$0 for every
  comparison district** (none has merged), which is the point the chapter makes.
- **Building Reorganization Incentive Aid** — the building-aid incentive; also $0.
- **Total Formula-Based Aids** — for context.

## Note on RIOA

The RIOA **formula** (40% of combined Foundation Aid for 5 years, then −4 pts/yr,
ending after 14 years; pegged to Foundation Aid since 2024-25) comes from NYSED's
*School District Reorganization: A Primer* (July 2024) and Education Law §3602 —
not from this dataset. The dataset supplies the **Foundation Aid base** the
chapter applies that formula to.
