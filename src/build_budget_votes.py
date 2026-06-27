"""Parse NYSED budget vote & re-vote workbooks into one tidy parquet.

Reads the raw per-year workbooks downloaded by ``download_budget_votes.py`` and
produces a clean panel: one row per district × vote with the Yes/No counts and
the 60%-supermajority flag. Faithful Python port of the R notebook's
``detect_layout()`` / ``harmonize()`` (the original is preserved in the git
history of ``front/contingency-budgets.qmd``).

**Why heuristic layout detection, not fixed column positions.** NYSED changes
the column headers and even the file format (.xls vs .xlsx) from year to year,
adds title rows, and uses multi-row merged headers. The sheets are also *not*
tidy: they carry no levy data, only a "60% required" supermajority flag, and
they bundle the budget block alongside an "additional propositions" block. So
each sheet is reduced to a standard schema by inferring its structure (see
``detect_layout``), not by matching a header row.

**District identity.** These files carry district *names* (no BEDS codes) and no
county, so they cannot be joined to the project's ``nysed_district_cd``
crosswalk. Name is kept as-is (whitespace-squashed); downstream Cambridge /
neighbor surfacing is by name match.

**Year convention.** ``year`` is the vote's *calendar* year = the budget-span
end year (file name) minus 1 (the "2025" file holds the May-2024 vote). Watch
this when joining other series.

Output schema (``data/processed/budget_votes.parquet``, git-ignored):

| column       | type    | meaning                                                  |
|--------------|---------|----------------------------------------------------------|
| year         | Int32   | vote calendar year (budget-end year − 1)                 |
| kind         | Utf8    | "vote" (May) or "revote" (June)                          |
| district     | Utf8    | district name, whitespace-squashed (case as NYSED gave)  |
| district_key | Utf8    | normalized name for joins (upper, suffix-stripped)       |
| yes          | Int64   | Yes votes (null if not a real vote row)                  |
| no           | Int64   | No votes                                                 |
| above_cap    | Boolean | True = needed 60% to pass (proposed levy above the cap)  |

Usage:  python src/build_budget_votes.py   (run download_budget_votes.py first)
"""

from __future__ import annotations

import re
import statistics
import sys
from pathlib import Path

import fastexcel
import polars as pl

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw" / "nysed_budget_votes"
OUT_PATH = PROJECT_ROOT / "data" / "processed" / "budget_votes.parquet"

# Header tokens that mark a sheet's header block (top rows carrying any of these
# are treated as headers, not data).
_TOKS = [
    r"\bYes\b", r"\bNo\b", "Total", "BUDGET", "60%", "Approval",
    r"\(Y/N\)", r"% *Yes", "PROPOSITION", "REVOTE", "FIRST",
]

# Rows whose district cell matches this are dropped (title/section/total rows).
_DROP_RE = re.compile(r"statewide|percentage|override distric|^boces", re.IGNORECASE)

# Section-label rows that are not districts (sectioned revote files interleave
# "FIRST VOTE:" / "REVOTE:" block labels among the data rows).
_LABELS = {
    "FIRST VOTE", "REVOTE", "FIRST BUDGET", "REVOTE BUDGET", "BUDGET",
    "ADDITIONAL PROPOSITION", "PROPOSITION", "OVERRIDE DISTRICTS",
}

# Suffixes stripped when building the normalized join key, so the same district
# is linkable across NYSED's varying name forms ("Corning-Painted Post Area SD"
# vs "...CSD"; all-caps old files vs title-case new files).
_SUFFIX_RE = re.compile(
    r"\s+(CENTRAL SCHOOL DISTRICT|UNION FREE SCHOOL DISTRICT|COMMON SCHOOL DISTRICT|"
    r"CITY SCHOOL DISTRICT|CENTRAL SD|CITY SD|AREA SD|COMMON SD|CSD|UFSD)\s*$",
    re.IGNORECASE,
)


def _is_label(d: str) -> bool:
    """True if a district cell is really a section label, not a district."""
    s = d.strip()
    if s.endswith(":"):
        return True
    return s.upper().rstrip(":").strip() in _LABELS


def _district_key(name: str) -> str:
    """Normalized district name for joins: upper, punctuation→space, suffix stripped."""
    s = re.sub(r"[^A-Za-z0-9 ]", " ", str(name).upper())
    s = " ".join(s.split())
    return " ".join(_SUFFIX_RE.sub("", s).split())


# --- small numeric helpers (mirrors the R num() / is_numlike()) -------------

def _num(x) -> float:
    """Coerce a possibly-formatted cell ("1,248", "75.1%", 0.97) to a float."""
    if x is None:
        return float("nan")
    s = re.sub(r"[,$%]", "", str(x))
    try:
        return float(s)
    except ValueError:
        return float("nan")


def _is_numlike(x) -> bool:
    return x == x and not (_num(x) != _num(x))  # num(x) is not NaN


def _median(vals: list[float]) -> float:
    v = [x for x in vals if x == x]  # drop NaN
    return statistics.median(v) if v else float("nan")


def _col(raw: list[list], idx: list[int], j: int) -> list:
    """Column j over row indices ``idx``."""
    return [raw[i][j] for i in idx]


def _read_raw(path: Path):
    """Read a sheet header-less, all-text (mirrors readxl ``col_types="text"``)."""
    df = fastexcel.read_excel(str(path)).load_sheet(0, header_row=None).to_polars()
    df = df.select(pl.all().cast(pl.Utf8))
    return [[df.row(i)[j] for j in range(df.width)] for i in range(df.height)], df.width, df.height


def detect_layout(raw, nc, nr, kind) -> dict:
    """Locate the name / Yes / No / 60%-flag columns and the data rows in one
    header-less, all-text sheet. Returns 0-indexed column positions.

    Detection rules (faithful port of the R ``detect_layout``):
      * name col  — header cell containing "SCHOOL DISTRICT"; else the column
                    richest in long non-numeric text among data rows.
      * Yes/No    — (1) the adjacent numeric pair whose yes/(yes+no) best matches
                    the file's own "% Yes" column; else (2) a Total = Yes + No
                    triple, choosing the right *block* by its label (prefer the
                    revote block in revote files; never the additional-
                    propositions block); else (3) the two right-most numeric cols.
      * 60% flag  — a column of Y/N values (Y = above cap, 60% needed).
    """
    # Header block = top rows (within the first 10) carrying any header token.
    hdr_rows0 = []
    for i in range(min(nr, 10)):
        cells = raw[i]
        if any(re.search(t, str(c), re.IGNORECASE)
               for t in _TOKS for c in cells if c is not None):
            hdr_rows0.append(i)
    header_end0 = max(hdr_rows0) if hdr_rows0 else -1
    data_idx = list(range(header_end0 + 1, nr))
    # Rows scanned for header text (R's `hdr = raw[seq_len(max(header_end,1)),]`).
    hdr_rows = list(range(max(header_end0, 0) + 1))

    # --- helpers over data cells of a column (non-blank only) ---
    def _nb(j):
        return [c for c in _col(raw, data_idx, j)
                if c is not None and str(c).strip() != ""]

    def _frac(j, pred):
        nb = _nb(j)
        return 0.0 if len(nb) < 3 else sum(pred(c) for c in nb) / len(nb)

    def _cnt(j, pred):
        return sum(1 for c in _nb(j) if pred(c))

    # --- most-text column (the fallback name column), computed over data rows ---
    best_j, best_ft = 0, float("-inf")
    for j in range(nc):
        cells = [c for c in _col(raw, data_idx, j) if c is not None]
        conds = [(not _is_numlike(c)) and len(str(c).strip()) > 4 for c in cells]
        ft = statistics.fmean(conds) if conds else float("nan")
        if ft == ft and ft > best_ft:
            best_j, best_ft = j, ft

    # --- district name column ---
    # Prefer a column whose *header* says "SCHOOL DISTRICT", but only if its data
    # is actually names: a merged title row can drop "SCHOOL DISTRICT" over a
    # leading code column (e.g. the 2014 revote file), which would otherwise be
    # mistaken for the name. Fall back to the most-text column otherwise.
    sd_col = next(
        (j for j in range(nc)
         if any(re.search("SCHOOL DISTRICT", str(c), re.IGNORECASE)
                for c in _col(raw, hdr_rows, j) if c is not None)),
        None,
    )
    name_col = best_j if (sd_col is None or _frac(sd_col, _is_numlike) > 0.5) else sd_col

    # --- numeric columns (right of the name col) ---
    numcols = [j for j in range(nc)
               if _frac(j, _is_numlike) > 0.7 and _cnt(j, _is_numlike) >= 3
               and j > name_col]

    # --- "% Yes" share column (anchored at cell start so "60% YES NEEDED" is excluded) ---
    share_col = next(
        (j for j in range(nc)
         if any(re.match(r"^\s*%\s*Yes", str(c), re.IGNORECASE)
                for c in _col(raw, hdr_rows, j) if c is not None)),
        None,
    )

    yes_col = no_col = None

    # (1) Adjacent numeric pair whose share best matches the "% Yes" column.
    if share_col is not None:
        sv = [_num(c) for c in _col(raw, data_idx, share_col)]
        mv = _median(sv)
        if mv == mv and mv > 1.5:  # stored as percent, not proportion
            sv = [v / 100 for v in sv]
        best, best_err = None, float("inf")
        for a in numcols:
            for b in numcols:
                if not (b > a and b <= a + 2):
                    continue
                ya = [_num(c) for c in _col(raw, data_idx, a)]
                yb = [_num(c) for c in _col(raw, data_idx, b)]
                sh = []
                for x, y in zip(ya, yb):
                    den = x + y
                    sh.append(x / den if (den == den and den != 0) else float("nan"))
                err = _median([abs(s - z) for s, z in zip(sh, sv)])
                if err == err and err < best_err:
                    best_err, best = err, (a, b)
        if best is not None and best_err < 0.02:
            yes_col, no_col = best

    # (2) Total = Yes + No triple, choosing the right block by its header label.
    if yes_col is None:
        triples = []
        for a in numcols:
            b, c = a + 1, a + 2
            if b in numcols and c in numcols:
                ya = [_num(x) for x in _col(raw, data_idx, a)]
                yb = [_num(x) for x in _col(raw, data_idx, b)]
                yc = [_num(x) for x in _col(raw, data_idx, c)]
                oks = [(x + y == z) for x, y, z in zip(ya, yb, yc)
                       if x == x and y == y and z == z]
                if len(oks) > 3 and statistics.fmean(oks) > 0.8:
                    lbl = ""
                    for jj in range(a, -1, -1):
                        hits = [str(h) for h in _col(raw, hdr_rows, jj)
                                if h is not None and re.search(
                                    r"BUDGET|REVOTE|PROPOSITION|FIRST|ADDITIONAL",
                                    str(h), re.IGNORECASE)]
                        if hits:
                            lbl = hits[0].upper()
                            break
                    triples.append((a, b, lbl))
        if triples:
            if kind == "revote":
                rv = [t for t in triples if "REVOTE" in t[2]]
                pool = rv or [t for t in triples
                              if "PROPOSITION" not in t[2] and "ADDITIONAL" not in t[2]]
            else:
                pool = [t for t in triples if not any(
                    k in t[2] for k in ("PROPOSITION", "ADDITIONAL", "REVOTE"))] or triples
            yes_col, no_col = pool[0][0], pool[0][1]

    # (3) Last resort: the two right-most numeric columns.
    if yes_col is None and len(numcols) >= 2:
        yes_col, no_col = numcols[-2], numcols[-1]

    # --- 60% supermajority flag: a column of Y/N values (right of the name col) ---
    def _is_yn(c):
        return c is not None and str(c).strip().upper() in ("Y", "N")

    cand = [j for j in range(nc)
            if _frac(j, _is_yn) > 0.6 and _cnt(j, _is_yn) >= 3 and j > name_col]
    flag_col = cand[0] if cand else None

    return {
        "name_col": name_col, "yes_col": yes_col, "no_col": no_col,
        "flag_col": flag_col, "share_col": share_col, "data_idx": data_idx,
    }


def harmonize(path: Path, year: int, kind: str):
    """Parse one workbook to a tidy frame (or ``None`` if Yes/No not located)."""
    raw, nc, nr = _read_raw(path)
    lay = detect_layout(raw, nc, nr, kind)
    di = lay["data_idx"]
    info = {"year": year, "kind": kind, "name_col": lay["name_col"],
            "yes_col": lay["yes_col"], "no_col": lay["no_col"],
            "flag_col": lay["flag_col"], "n_kept": 0}

    if lay["yes_col"] is None or lay["no_col"] is None:
        return None, info

    nj = lay["name_col"]
    district = [" ".join(str(raw[i][nj]).split()) if raw[i][nj] is not None else None
                for i in di]
    yes = [_num(raw[i][lay["yes_col"]]) for i in di]
    no = [_num(raw[i][lay["no_col"]]) for i in di]
    fj = lay["flag_col"]
    flag = ([str(raw[i][fj]).strip().upper() == "Y" if raw[i][fj] is not None else None
             for i in di] if fj is not None else [None] * len(di))

    recs = {"year": [], "kind": [], "district": [], "district_key": [],
            "yes": [], "no": [], "above_cap": []}
    for d, y, n, fl in zip(district, yes, no, flag):
        if d is None or d == "" or _DROP_RE.search(d) or _is_label(d):
            continue
        if not (y == y) or not (n == n) or (y + n) <= 0:
            continue
        recs["year"].append(year)
        recs["kind"].append(kind)
        recs["district"].append(d)
        recs["district_key"].append(_district_key(d))
        recs["yes"].append(int(round(y)))
        recs["no"].append(int(round(n)))
        recs["above_cap"].append(fl)

    # Validation: where a "% Yes" column exists, do the detected Yes/No rebuild it?
    recon = float("nan")
    if lay["share_col"] is not None:
        sv = [_num(c) for c in _col(raw, di, lay["share_col"])]
        mv = _median(sv)
        if mv == mv and mv > 1.5:
            sv = [v / 100 for v in sv]
        ya = [_num(raw[i][lay["yes_col"]]) for i in di]
        yb = [_num(raw[i][lay["no_col"]]) for i in di]
        sh = [x / d if (d := x + y) == d and d != 0 else float("nan")
              for x, y in zip(ya, yb)]
        recon = _median([abs(s - z) for s, z in zip(sh, sv)])
    info["n_kept"] = len(recs["year"])
    info["recon_err"] = recon

    if not recs["year"]:
        return None, info
    frame = pl.DataFrame(
        recs,
        schema={"year": pl.Int32, "kind": pl.Utf8, "district": pl.Utf8,
                "district_key": pl.Utf8, "yes": pl.Int64, "no": pl.Int64,
                "above_cap": pl.Boolean},
    )
    return frame, info


def main() -> None:
    files = sorted(f for f in RAW_DIR.glob("*.xls*") if not f.name.startswith("djb"))
    if not files:
        raise FileNotFoundError(f"No vote workbooks in {RAW_DIR}; run download_budget_votes.py first")

    frames, infos = [], []
    for f in files:
        m = re.search(r"(\d{4})_(vote|revote)", f.stem)
        if not m:
            print(f"  SKIP {f.name}: name does not match <year>_<kind>")
            continue
        budget_end, kind = int(m.group(1)), m.group(2)
        frame, info = harmonize(f, budget_end - 1, kind)  # vote calendar year
        info["file"] = f.name
        infos.append(info)
        if frame is not None:
            frames.append(frame)
        flag = "Y" if info["flag_col"] is not None else "-"
        recon = (f"{info['recon_err']:.4f}" if info.get("recon_err") == info.get("recon_err") else "-")
        status = "OK" if frame is not None else "SKIP (no Yes/No)"
        print(f"  {f.name:<20} name={info['name_col']} yes/no={info['yes_col']}/{info['no_col']} "
              f"flag={flag} rows={info['n_kept']:<5} recon={recon}  {status}")

    if not frames:
        raise RuntimeError("No workbook yielded rows")
    panel = pl.concat(frames, how="vertical").sort("year", "kind", "district")

    # Validation summary: any share reconciliation that exceeded tolerance.
    bad = [i for i in infos if i.get("recon_err") == i.get("recon_err") and i["recon_err"] > 0.02]
    print(f"\nShare-reconciliation median |err| > 0.02 (investigate): "
          f"{len(bad)} file(s)" + ("" if not bad else f" -> {[i['file'] for i in bad]}"))

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    panel.write_parquet(OUT_PATH, compression="zstd")
    yrs = panel["year"].unique().sort().to_list()
    print(f"\nWrote {panel.height:,} district-votes to {OUT_PATH.relative_to(PROJECT_ROOT)}")
    print(f"  years {yrs[0]}–{yrs[-1]} ({len(yrs)} vote years); "
          f"{panel.filter(pl.col('kind')=='vote').height} vote rows, "
          f"{panel.filter(pl.col('kind')=='revote').height} revote rows")


if __name__ == "__main__":
    main()
