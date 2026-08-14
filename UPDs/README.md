# UPDs — post-release data-quality updates

**This folder is not part of the paper replication.** `00_master.R` and
`scripts/01`–`09` reproduce the *Scientific Data* article exactly as submitted and
must not be changed. Everything here runs *after* that pipeline, against the
released `data/red.Rds`, and is additive.

Two things live here:

1. **A verification script** that recomputes every quality figure quoted on
   <https://georgytarasenko.com/red/coverage.html> straight from the release, so
   no number on the site is asserted rather than measured.
2. **A build script** that produces `red_plus.Rds` — the released data with the
   defects listed in `KNOWN_ISSUES.md` corrected or flagged, plus the derived
   columns the website computes internally but never shipped to downloaders.

Nothing here mutates `data/red.Rds`.

## Contents

| File | What it does |
|---|---|
| `01_verify_release.R` | Recomputes every figure in `KNOWN_ISSUES.md` from `data/red.Rds` and writes `output/red_qc_report.txt`. Read-only. |
| `02_build_red_plus.R` | Writes `output/red_plus.Rds` — corrections + derived columns + provenance flags. |
| `KNOWN_ISSUES.md` | The verified defect list, with the R expression that reproduces each one. |

## Why `red_plus.Rds` exists

The website's build layer already computes several things that never reach anyone
who downloads RED from Dataverse. The most valuable is **coordinate inheritance**:
`build_explorer_points_inherited.R` carries a unit's observed coordinate backwards
and forwards along `(region, uik)`, which is what lets the map show precinct dots
for the eight elections that have no coordinates of their own. That logic lived
only in the site. `02_build_red_plus.R` moves it into the data, with an explicit
`coord_source` column so an inherited point is never mistaken for an observed one.

The same script applies the small, unambiguous corrections — normalising string
encoding, wrapping the five unwrapped Chukotka longitudes, nulling the two
impossible latitudes — and **flags rather than deletes** everything judgement-laden:
duplicates, the Yakutia handbook rows, zero-electorate rows, and units whose
coordinates are mutually inconsistent across elections.

## Running

```r
setwd("<repo root>")
source("UPDs/01_verify_release.R")   # ~5 min, read-only
source("UPDs/02_build_red_plus.R")   # ~10 min, writes UPDs/output/
```

Both use base R only — no data.table, no sf — so they run anywhere the release
itself can be opened. Use a UTF-8 locale.

## Columns added by `red_plus.Rds`

| Column | Meaning |
|---|---|
| `coord_source` | `observed` · `inherited` · `NA` |
| `coord_inherited_from` | year the coordinate was taken from, when inherited |
| `coord_drift_km` | max distance between a unit's own coordinates across elections |
| `flag_coord_unstable` | `TRUE` where `coord_drift_km > 25` — at least one election's coordinate is wrong |
| `other_votes` / `other_share` | 2000 residual: `valid − Σ(named candidates)`; the four missing 2000 candidates are not recoverable from the release |
| `ballot_gap` | `turnout − (valid + invalid)` |
| `is_online` | remote e-voting aggregate row, not a polling station |
| `flag_source_handbook` | one of the 791 hand-digitised Yakutia 2000 rows |
| `flag_arith_impossible` | `turnout > electorate`, `valid > turnout`, or non-integer `valid` |
| `flag_zero_electorate` | `electorate == 0` |
| `flag_exact_duplicate` | row is byte-identical to an earlier row across all 91 columns |
| `flag_dup_key` | shares `(year, region, tik, uik)` with another row |
| `flag_dup_key_results_differ` | …and the rows disagree about a reported result |

`red_plus.Rds` is gitignored — it is a derived artifact, rebuild it from the release.
