# A Guide to the Headings of the Fiscal Profile (NYSED)

> Saved from <https://www.nysed.gov/fiscal-analysis-research/guide-headings-fiscal-profile>
> (NYSED Fiscal Analysis & Research). Reference for how each ST-3 fiscal-profile
> item is calculated from the Uniform System of Accounts account codes. Captured
> 2026-06-28 for the reserves / fund-balance chapter; see `docs/README.md`.

The descriptions below explain how each data item is calculated. Any aggregate
totals or averages (for example, the State) are based on the State's major
districts. The data displayed are aggregated from individual line items of the
Annual Financial Report (ST-3), as reported by districts.

## Total Unexpended Surplus Funds, UFB, or AT0994  *(the 4%-capped measure)*

For school years starting with 2010-11:

Total Unexpended Surplus Funds (AT0994). For each year, this item consists of the
**Committed, Assigned Unappropriated, and Unassigned fund balances** minus the
encumbrances included in Committed and Assigned fund balance minus the reserve
for insurance recovery. A negative number was reported by some districts. This
item does **not** reflect the total fund balance of the General Fund, Special Aid
Fund or Debt Service Fund of a district. (Big Five City districts are excluded
from aggregate totals/averages.)

> Project note: under the property-tax-cap law (RPTL §2-c / Ed Law §2023-a, the
> "tax cap"), a school district's unassigned/unexpended-surplus fund balance
> effective for the budget year is limited to **4% of the prior year's budget**.
> The PTRC's `fund_balance_unrestricted_pct_of_budget` column is this measure.

## Revenues

Three revenue sources (State, Local, and Federal) are displayed for the General,
Debt Service and Special Aid Funds. Because of STAR, three categories for State
revenues:

- **State Aid.** Sum of account codes AT3999 and FT3999.
- **STAR.** State school-tax-relief payments (A1085).
- **Total State Revenue.** A1085 + A1187 + AT3999 + FT3999.
- **Local Revenue.** General Fund revenues/transfers/proceeds of long-term debt
  (AT5999) + Debt Service (VT5599) + Special Aid local (FT2999) … (several
  interfund-transfer subtractions to avoid double counting).
- **Federal Revenue.** AT4999 + FT4999.

## Expenditures (GF, SAF, DSF)

The ST-3 is a fiscal accounting document, not an educational program document.
Major instructional categories and their account-code sums include:

- **Teacher Salaries.** A2110.11/.12/.13/.14, A2250.15, A2280.15, A2330.15, …
- **Instructional Salaries for Pupil Personnel Services.** A2805.15–A2830.15, …
- **Curriculum Development and Supervision.** A2010.15, A2020.15, A2040.15, …
- **BOCES Instructional Services.** A2010.49, A2020.49, …
- **Tuition 1, Paid To Other School Districts (excl. Special Acts).** A2110.471, …
- **Tuition 2, All Other.** A2110.472, …
- **Other Instructional Salaries / Expenditures.** (many A/F 20xx–28xx codes)
- **Community Service.** A7140.0, A7310.0, A8060.0, FT8099.0 …
- **Operation & Maintenance.** A1620.0/.1/.2, F1620.x … (includes repair-reserve
  expenditures).

## Undistributed

- **Teacher Retirement (General Fund).** A9020.8.
- **Health.** A9060.8 (hospital, medical, dental).
- **Other Fringe Benefits.** A9010.8, A9030.8, A9040.8, A9045.8, A9050.8,
  A9055.8, A9070.8, A9089.8, plus the F-prefixed benefit codes.
- **Total Fringe Benefits.** Teacher Retirement + Health + Other Fringe.
- **Other Undistributed.** A1660.0, A1670.0, A1680.0, AT1998.0 less A1710.4
  (central storeroom; printing/mailing; data processing; special items;
  expenditures from other reserves; unallocated insurance).
- **Other (Includes Interfund Transfers).** AT9951.0 + F9901.9 − several
  specific interfund transfers.

**Expenditure Subtotal** = sum of the categories above.

- **Transportation.** AT5599.0 + FT5599.0 − F5510.8 − F5511.8.
- **Debt Service Principal.** AT9798.6 + V9798.6.
- **Debt Service Interest.** AT9798.7 + V9798.7.
- **Total Expenditures.** GF expenditures/transfers (AT9999.0) + Debt Service
  (V1380.4, V9798.6, V9798.7) + Special Aid (FT9999.0) + F9901.9 − specific
  interfund transfers.
- **Total Unexpended Surplus Funds / Total Expenditures.** (×100). State percent
  excludes the Big Five's surplus but includes their expenditures.
- **Revenue − Expenditures; (Rev − Exp)/Total Expenditures.**

## Pupil counts (for per-pupil amounts)

- **Duplicated Combined Adjusted Average Daily Membership (DCAADM).** Pupil count
  for per-pupil revenue/expenditure items (from State Aid worksheets / SIRS):
  ADM of enrolled pupils (half-day K = 0.5) + equivalent secondary attendance +
  dual-enrolled ADA + BOCES/full-time special-ed + approved-private + tuitioned +
  incarcerated youth; charter-school residents from 1999-2000; full-day Pre-K
  weighted 1.0 from 2007-08. A duplicated count (residents attending other
  districts also count there). State total = sum of rounded district counts.
- **Revenue/Pupil.** Total revenue ÷ DCAADM.
- **Expenditure/Pupil.** Total expenditure ÷ DCAADM.

## Fiscal characteristics — wealth measures

- **Actual Value/TWPU; Income/TWPU.** Property/income per Total Wealth Pupil
  Unit, used to distribute operating aid.
- **Pupil Wealth Ratio.** AV (two years prior) ÷ TWPU (one year prior) ÷ State
  average AV/TWPU.
- **Alternate Pupil Wealth Ratio.** AGI-based analog.
- **Combined Wealth Ratio (CWR).** Weights the property and income wealth ratios
  equally; State average = 1.0 (>1.0 = wealthier than average).
- **TWPU.** Weighted K-12 resident ADM + weightings for special needs, students
  with disabilities, secondary pupils (half-day K = 0.5).

## Salaries & instructional share

- **Salaries.** Instructional + noninstructional salaries.
- **Instructional Salaries.** Teachers + PPS + Curriculum/Supervision + BOCES +
  Other Instructional.
- **Instructional Fringe Benefits.** Estimated fringe benefits for instructional
  personnel (= all fringes ÷ all salaries × instructional salaries).
- **Fringe Benefit Rate.** Total Fringe Benefits ÷ Total Salaries.
- **Transfers to Capital Fund (A9950.9).** Subtracted for adjusted expenditures.
- **Adjusted Expenditures.** Subtotal − Tuition 1 − A9950.9.
- **IE1 (instructional ex. fringe), IE2 (incl. fringe), IE3 (incl. fringe, less
  Tuition 1)** and their %-of-expenditure ratios — see the source page for the
  full code lists.
- **Local Revenue Effort Rate.** Local revenue ÷ prior-year actual property
  value × 1,000.

*Full code lists for every item are on the source page; this is a condensed
capture of the definitions most relevant to revenue, expenditure, fund balance,
and per-pupil analysis.*
