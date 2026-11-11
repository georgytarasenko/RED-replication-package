# 2026 State Duma party-list BETA extension

This is a **separate preliminary research snapshot**, not part of the peer-reviewed RED 2000–2024 release in `data/red.Rds`. It is based on CEC public API report 242 responses collected by Vladimir Ivanov on 30 September 2026 and distributed in [his repository](https://github.com/Spiceman161/duma-election-2026). The original archive and its checksum are available in the repository's `research-2026-09-30` release. The CEC responses were not independently re-collected for this extension.

## Rebuild

Download `duma-2026-csv-2026-09-30.zip` from the source release. From the RED repository root, run:

```sh
bash extensions/2026_beta/run.sh /absolute/path/to/duma-2026-csv-2026-09-30.zip
```

This runs the existing RED first-stage cleaning code with a 2026 adapter in an isolated temporary directory, then exports the 2024 published anchor and builds the separate CSV files. It requires R with `tidyverse`, `stringi`, `data.table`, `arrow`, and Python with `pandas` and `pyarrow`. The baseline `data/red.Rds` is read only.
The expected SHA-256 of the source CSV archive is `e1fbff4c4994e8ea8d74b3a73685c7be4c2d418d1573b446e643be42cf0ee87c`; the runner checks it before processing.

## Scope and interpretation

- Ordinary precinct party-list protocols only, in the 83 regions represented in published RED. Crimea, Sevastopol, the four annexed regions, and abroad/Baikonur are excluded from this comparable geographic scope.
- Remote electronic voting (DEG) is processed separately at district level. It is never joined to a physical precinct. The source's platform coverage has not been independently verified.
- Single-member candidate results are outside this release.
- `match_status=provisional_number_match` means unique 2024 region, TIK, and UIK number with 2026/2024 electorate ratio between 0.5 and 2. It is a *candidate* longitudinal match, not independent proof that the polling station is the same. The local output's `panel_key_2024` field is blank for all other rows.
- `review_electorate_change` means a numeric match exists but electorate changed by more than a factor of two (or has an undefined ratio). `unmatched` means no unique exact numeric match. **All 2026 observations remain in the release.**
- Most source coordinates and addresses were inherited from a historical map based on region and UIK number. They are approximate and cannot independently validate precinct continuity. Unplaced observations remain in the downloadable CSV.
- Preliminary and recounted protocol statuses are retained. The source has only its latest collected version; earlier versions and certified final results have not been checked.

The runner writes local CSV and quality-check files under `release/`. They are not distributed with this BETA visualization. The published RED 2000–2024 dataset is unchanged.

The optional website arrow map in `release/swing_2021_2026_beta.json` compares party vote shares and turnout with 2021 using only unique 2021 records linked through a provisional 2024 numeric key. It has 77,590 paired precincts, of which 75,748 have map positions. It is descriptive; the arrows represent percentage-point changes, not geographic movement or independently verified physical continuity. The map shows a regular sample at national scale for performance and all visible linked points when zoomed in.
