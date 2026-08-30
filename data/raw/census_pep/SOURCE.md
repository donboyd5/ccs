# U.S. Census Bureau — county population estimates, Vermont counties

The demographics chapter compares the Cambridge area's population trend with
neighboring counties, including two just across the state line in Vermont:
**Bennington County (FIPS 50003)** and **Rutland County (FIPS 50021)**. New York
county history comes from the sibling `popfc` forecast project (see
`../popfc/SOURCE.md`); **Vermont is not in popfc** (it is NYS-only), so the two
Vermont counties are pulled directly from U.S. Census Bureau population-estimate
releases.

| item | value |
|---|---|
| publisher | U.S. Census Bureau (Population Estimates Program; legacy county intercensal releases via NBER) |
| source page | <https://www.census.gov/programs-surveys/popest.html> |
| coverage | Bennington & Rutland counties, **resident population, 1970–2024** |
| grain | one row per county per year |
| fetched by | [`src/download_census_vt.py`](../../../src/download_census_vt.py) |
| output | `vt_county_population.csv` (this folder; git-ignored, regenerable) |

## Why four sources stitched together

No single file spans 1970–2024. The download script stitches four sources:

| years | source | how obtained |
|---|---|---|
| 1970–1999 | Census legacy **county intercensal** releases, via NBER's consolidated file | key-free CSV <https://data.nber.org/census/population/popest/county_population.csv> (`pop1970…pop1999`) |
| 2000–2009 | PEP **2000s intercensal** | Census API `2000/pep/int_population`, `DATE_` 2–11 (7/1/2000…7/1/2009). **Requires a `CENSUS_API_KEY`** env var (Census now rejects keyless API requests). |
| 2010–2019 | PEP **2010s vintage** | key-free bulk file `co-est2019-alldata.csv` (`POPESTIMATE2010…2019`) |
| 2020–2024 | PEP **2020s vintage** (2020-census-based) | key-free bulk file `co-est2024-alldata.csv` (`POPESTIMATE2020…2024`) |

**Basis note (1970–1999).** Years after 1999 are all **July-1 resident
population**, matching popfc's NY series. The legacy decades follow the Census
convention of the time: **census-year values (1970, 1980, 1990) are April-1
census counts; other years are July-1 intercensal estimates**. 1970s county
values are rounded to hundreds as published. On an indexed trend chart the
April/July difference (a few tenths of a percent) is invisible.

**Why NBER for the legacy decades.** The Census Bureau's own pre-2000 files are
awkward: the 1970s county totals are fixed-width, the 1980s annual county totals
exist only inside a 34 MB age-sex-race file (the totals file carries just the
1980/1990 endpoints), and the 1990s state-county totals are published as PDF.
NBER's file is a documented consolidation of exactly those releases
(errata maintained at `…/popest/errata.txt`).

The large files (all U.S. counties) are cached under `_cache/` (git-ignored)
and only the two Vermont rows are extracted.

## Known data quirks — rebenchmark steps

- **At 2000** (1999→2000): the legacy estimates end and the 2000s intercensal
  (2000-census-based) begins. The 2000 Census counted **more** people than the
  legacy estimates had shown (Bennington 35,965 → 36,965, +2.8%; Rutland
  62,407 → 63,373, +1.5%), so the series steps up at 2000. A rebenchmark, not an
  error; noted in the chapter's chart caption.
- **At 2020** (2019→2020): the **2010–2019 values come from the 2019 vintage**
  (pre-2020-census), while **2020 onward comes from the 2024 vintage**
  (rebenchmarked to the 2020 Census), which counted more people in both counties
  (Bennington 35,470 → 37,312; Rutland 58,191 → 60,464) — a visible uptick at
  2020. New York's `popfc` series does not show this kink because its reconciled
  series uses the v2025 vintage throughout. The chapter notes this where the
  Vermont 2020 point is shown.

Census also publishes a revised **2010–2020 intercensal** series (consistent at
both census endpoints); it is not pulled here. If the 2020 kink is undesirable,
swapping the 2010–2019 Vermont values for that revised intercensal file would
smooth it.

## Seam validation (1999→2000 splice)

Per project conventions the splice was validated: year-over-year changes at the
seam vs. neighboring transitions (from `build_demographics.py`'s seam check):

| county | 1998→99 | 1999→00 (seam) | 2000→01 |
|---|---|---|---|
| Bennington | +0.1% | **+2.8%** (rebenchmark) | +0.0% |
| Rutland | −0.2% | **+1.5%** (rebenchmark) | −0.4% |

The seam jump is entirely the 2000-census rebenchmark (documented above), not a
series break: the neighbors on both sides are near zero.

## Reproducing

```bash
CENSUS_API_KEY=<your-key>  python src/download_census_vt.py        # cached
CENSUS_API_KEY=<your-key>  python src/download_census_vt.py --force  # re-fetch
```

Without a key, 2000–2009 are skipped (the other three sources need no key). The
key is the same one `popfc` uses for its ACS pulls; it is read from the
environment, never written to the repo.
