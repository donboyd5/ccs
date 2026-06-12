# Federal Audit Clearinghouse — audited annual financial reports (PDF), Cambridge CSD

Audited annual financial statements for **Cambridge Central School District**,
fiscal years ending June 30, **2020–2025** (6 PDFs), retrieved from the **Federal
Audit Clearinghouse** (<https://app.fac.gov/>). Held for reference and
hand-checking the account-code finance numbers; not yet parsed into any processed
dataset.

The Clearinghouse was operated by the **U.S. Census Bureau** through ~2022 and by
the **U.S. General Services Administration** from 2023 on — which is why the
2020–2022 files carry a `CENSUS` tag (titled *"Financial statements and
independent auditor's report"*) and the 2023–2025 files carry a `GSAFAC` tag
(titled *"Financial report"*). Same series of district audits, two operators.

## Files

`raw/fac_annual_reports/<YYYY>-06-<SYSTEM>-<docid>.pdf`

| file | FYE | source-system token | doc id |
|---|---|---|---|
| `2020-06-CENSUS-0000160842.pdf` | 2020-06-30 | CENSUS | 0000160842 |
| `2021-06-CENSUS-0000160842.pdf` | 2021-06-30 | CENSUS | 0000160842 |
| `2022-06-CENSUS-0000160842.pdf` | 2022-06-30 | CENSUS | 0000160842 |
| `2023-06-GSAFAC-0000008194.pdf` | 2023-06-30 | GSAFAC | 0000008194 |
| `2024-06-GSAFAC-0000060740.pdf` | 2024-06-30 | GSAFAC | 0000060740 |
| `2025-06-GSAFAC-0000389347.pdf` | 2025-06-30 | GSAFAC | 0000389347 |

The names are a **project convention** (`<fye-year>-<fye-month>-<system>-<docid>`),
not the original download filenames. `06` is the June 30 fiscal year end; the
`<system>` token is the Clearinghouse operator (CENSUS vs GSAFAC, above) and
`<docid>` is its report id, so each PDF can be re-found on <https://app.fac.gov/>
by searching Cambridge CSD for that fiscal year.
