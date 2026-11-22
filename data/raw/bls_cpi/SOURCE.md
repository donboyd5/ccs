# Consumer Price Index (CPI-U) — U.S. Bureau of Labor Statistics

| | |
|---|---|
| publisher | U.S. Bureau of Labor Statistics (BLS) |
| series | `CUUR0000SA0` — CPI for All Urban Consumers (CPI-U), All items, U.S. city average, **not seasonally adjusted**, 1982-84 = 100 |
| source | BLS Public Data API v2, <https://api.bls.gov/publicAPI/v2/timeseries/data/> (documentation: <https://www.bls.gov/developers/>) |
| coverage downloaded | monthly, January 1994 through the latest published month |
| used for | adjusting dollar amounts for inflation (`comparisons.to_real()`) |

## Files

| on disk | what it is |
|---|---|
| `cpi_u_monthly.csv` (git-ignored) | one row per month: `year`, `month`, `value`, `note` (BLS footnote text, if any) |

Re-download: `python src/download_cpi.py`. No API key is needed; unregistered
requests are limited to 10 years each and 25 per day, and the script makes one
request per decade.

## Time convention

The processed file `data/processed/price_index_school_year.parquet`
(`python src/build_price_index.py`) averages the monthly index over each **school
year, July through June**. `year_end` 2025 is July 2024 through June 2025. This
matches the fiscal year of the school finance, tax and budget data. A school year
is written only when all twelve months are available.

## Known gap

**October 2025 has no published value** (BLS footnote: "Data unavailable due to
the 2025 lapse in appropriations"). The builder fills a single missing month
with the midpoint of the months on either side and records it in
`months_interpolated`. This affects only school year 2025-26 (`year_end` 2026).

## Why this index, and its limits

CPI-U is the broadest standard measure of consumer prices and the one most
readers know. It measures what households pay, not what school districts pay
(mostly salaries and health insurance), so adjusted amounts answer "what did this
cost in today's dollars", not "how much more schooling did it buy". The project
owner's decision (2026-11): CPI-U for now; the index may change later. Only
`download_cpi.py` and `build_price_index.py` name the index.

## Update log

- **2026-11-22** — First download: 394 months, 1994-01 through 2026-10. School-year
  index built for 1995 through 2026. Values used most: 238.273 (2015-16),
  317.731 (2024-25), 327.400 (2025-26, one month interpolated).
