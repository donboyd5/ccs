# U.S. Census Bureau — Population Estimates Program (PEP), Vermont counties

The demographics chapter compares the Cambridge area's population trend with
neighboring counties, including two just across the state line in Vermont:
**Bennington County (FIPS 50003)** and **Rutland County (FIPS 50021)**. New York
county history comes from the sibling `popfc` forecast project (its reconciled
Census-PEP / NYSDOL series — see `../popfc/SOURCE.md`); **Vermont is not in
popfc** (it is NYS-only), so the two Vermont counties are pulled directly from
the U.S. Census Bureau's Population Estimates Program (PEP).

| item | value |
|---|---|
| publisher | U.S. Census Bureau, Population Estimates Program |
| source page | <https://www.census.gov/programs-surveys/popest.html> |
| coverage | Bennington & Rutland counties, **July-1 resident population, 2000–2020** |
| grain | one row per county per year |
| fetched by | [`src/download_census_vt.py`](../../../src/download_census_vt.py) |
| output | `vt_county_population.csv` (this folder; git-ignored, regenerable) |

## Why three sources stitched together

PEP rebenchmarks to each decennial census, so **no single file spans 2000–2020**.
The download script stitches three sources, all **July-1 resident population**
(the same basis as popfc's NY series, so the two compare in one indexed chart):

| years | source | how obtained |
|---|---|---|
| 2000–2009 | PEP **2000s intercensal** | Census API `2000/pep/int_population`, `DATE_` 2–11 (7/1/2000…7/1/2009). **Requires a `CENSUS_API_KEY`** env var (Census now rejects keyless API requests). |
| 2010–2019 | PEP **2010s vintage** | key-free bulk file `co-est2019-alldata.csv` (`POPESTIMATE2010…2019`) |
| 2020 | PEP **2020s vintage** (2020-census-based) | key-free bulk file `co-est2024-alldata.csv` (`POPESTIMATE2020`) |

The two large bulk files (all U.S. counties) are cached under `_cache/`
(git-ignored) and only the two Vermont rows are extracted.

## Known data quirk — the 2020 rebenchmark kink

The **2010–2019 values come from the 2019 vintage** (pre-2020-census), while the
**2020 value comes from the 2024 vintage** (rebenchmarked to the 2020 Census).
The 2020 Census counted **more** people in both Vermont counties than the 2019
vintage had estimated (Bennington 35,470 → 37,312; Rutland 58,191 → 60,464), so
the series shows a visible **uptick at 2020**. This is a real rebenchmark, not a
data error — it reflects the 2020 Census finding the 2010s-vintage estimates had
undercounted these counties. New York's `popfc` series does not show the same
kink because its reconciled series uses the v2025 vintage throughout. The chapter
notes this where the Vermont 2020 endpoint is shown.

Census does publish a revised **2010–2020 intercensal** series (consistent at
both census endpoints); it is not pulled here. If the 2020 kink is undesirable,
swapping the 2010–2019 Vermont values for that revised intercensal file would
smooth it.

## Reproducing

```bash
CENSUS_API_KEY=<your-key>  python src/download_census_vt.py        # cached
CENSUS_API_KEY=<your-key>  python src/download_census_vt.py --force  # re-fetch
```

Without a key, only 2010–2020 are produced (the bulk files need no key). The key
is the same one `popfc` uses for its ACS pulls; it is read from the environment,
never written to the repo.
