# Cambridge Central School District: issues and comparisons

**Website: <https://donboyd5.github.io/ccs/>**

An analysis of the Cambridge Central School District (Washington County, New
York) using public data: enrollment, class size, spending, taxes, reserves and
test results, compared with nearby and similar districts. The website informs;
it does not make recommendations.

## What is in this repository

| folder | what it holds |
|---|---|
| [`books/cambridge-comparisons/`](books/cambridge-comparisons/) | the website's source (a Quarto book) |
| [`src/`](src/) | scripts that download the source data and build the datasets |
| [`data/`](data/README.md) | data organization, sources, and how to refresh the data |
| [`docs/`](docs/README.md) | reference manuals consulted for the analysis |
| [`meta/`](meta/README.md) | working agreements and a log of decisions |

## Building the website

```bash
python -m venv .venv
.venv/bin/pip install -r requirements.txt
cd books/cambridge-comparisons
QUARTO_PYTHON=../../.venv/bin/python quarto render
```

The source data are not stored in this repository. Each folder under
`data/raw/` has a `SOURCE.md` saying where its data come from and how to
download them; [`data/README.md`](data/README.md) has the full procedure.
