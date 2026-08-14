# RED — known issues in the released `red.Rds`

**Release profiled:** `data/red.Rds`, md5 `488f33c56f52fece5ba76ca58f6b9df1`,
1,044,599 rows × 91 columns (1,044,540 precinct rows + 59 online aggregate rows).
**Verified:** 14 Aug 2026. Every figure here is recomputed by
`UPDs/01_verify_release.R`; none is quoted from an earlier document.

Nothing below is hidden from the release. It is written down so nobody discovers
it by accident, and so the same numbers appear here, on
<https://georgytarasenko.com/red/coverage.html>, and in the build log of
`UPDs/02_build_red_plus.R`.

---

## 1 · Spatial coverage is not uniform across regions

The most consequential issue, and the one most likely to be mistaken for real
absence. **Returns are complete for every region in every election** — row counts
and electorate are stable. The gap is spatial only.

| Region | 2012 | 2018 | 2021 | 2024 |
|---|---:|---:|---:|---:|
| **Adygea** | 0% | 0% | 0% | 0% |
| **Udmurtia** | 98.1% | 12.1% | 11.9% | **0%** |
| **Belgorod** | 97.3% | 97.7% | 98.0% | **0.1%** |
| **Lipetsk** | 98.2% | **7.5%** | **7.3%** | 98.7% |
| *national mean* | 95.5% | 94.1% | 94.2% | 94.3% |

Adygea is the only region in the country with **no addresses and no coordinates in
any of the eleven elections**.

Coordinate coverage tracks address coverage exactly for Adygea, Udmurtia and
Lipetsk, so for those three the failure is upstream in the address merge
(`scripts/04_merge_addresses.R`), not in geocoding. **Belgorod 2024 is different**:
97% of rows have an address and 0.1% have coordinates, which is a geocoding
failure on inputs that exist.

```r
round(100 * tapply(!is.na(red$lat), list(red$region, red$year), mean), 1)
```

## 2 · Coordinates are mutually inconsistent across elections

Of the 91,421 harmonized units geocoded in more than one election (measured after
normalising the antimeridian):

| max drift between elections | units | share |
|---|---:|---:|
| > 1 km | 16,708 | 18.28% |
| > 5 km | 5,168 | 5.65% |
| > 25 km | 2,036 | 2.23% |
| > 100 km | **944** | 1.03% |
| > 500 km | 90 | 0.10% |

A polling station does not move 100 km, so at least one election's coordinate is
wrong for these units. The errors concentrate in **2012**:

| region | uik | 2012 | 2018–2024 | reading |
|---|---:|---|---|---|
| Oryol Oblast | 600 | 56.164 / 92.980 | 53.007 / 36.155 | 2012 lands in Krasnoyarsk Krai |
| Novosibirsk Oblast | 122 | 55.750 / 37.208 | 55.528 / 84.472 | 2012 lands in Moscow |
| Chelyabinsk Oblast | 1035 | 45.250 / 38.327 | 55.254 / 61.735 | 2012 lands in Krasnodar |
| Zabaykalsky Krai | 3414 | — | 51.423 / 110.046 vs 54.649 / 32.870 | 4,969 km apart |

Prefer the 2018–2024 coordinate where a unit's coordinates disagree.
`red_plus.Rds` exposes this as `coord_drift_km` / `flag_coord_unstable`.

## 3 · Mixed longitude convention in Chukotka

Chukotka crosses the antimeridian and the release records it two ways at once:
**62 rows use standard negative longitudes** (down to −179.484) and **5 rows from
2012 use an unwrapped convention** (180.737 to 188.984).

Four stations carry both, which settles which is the anomaly:

| uik | 2012 `lon` | 2018 / 2021 / 2024 `lon` | 2012 wrapped |
|---:|---:|---:|---:|
| 30 | 180.7374 | −179.1265 | −179.2626 |
| 34 | 181.1092 | −178.8793 | −178.8908 |
| 54 | 188.9836 | −171.0119 | −171.0164 |
| 55 | 188.2867 | −171.7005 | −171.7133 |

Loading the release into any standard −180…180 CRS puts those five points in the
Pacific and yields invalid geometry with no warning. `02_build_red_plus.R` wraps
them; renderers that want a contiguous Russia should unwrap at draw time, which is
what the website does.

## 4 · Two impossible latitudes

| year | region | uik | lat | lon |
|---|---|---:|---:|---:|
| 2024 | Republic of Altai | 106 | 87.2567 | 87.6087 |
| 2021 | Kemerovo Oblast – Kuzbass | 249893 | 87.5622 | 87.5622 |

Both sit north of Russia's northernmost land; both look like latitude copied from
longitude. `02_build_red_plus.R` sets them to `NA` rather than guessing.

A further **23 rows have `lat` exactly equal to `lon`** in the 47–60° band, where
that is geographically possible in European Russia. Several propagate across
elections through coordinate inheritance (uik 1710 at 56.27079/56.27079 in 2018,
2021 *and* 2024), so if any is a copy error it has been replicated. Left alone
pending a manual check.

## 5 · Duplicate rows

| measure | count |
|---|---:|
| rows byte-identical across all 91 columns | **632** |
| duplicate-key `(year, region, tik, uik)` groups | **2,566** |
| …that are pure exact duplicates | 340 |
| …that differ in ≥1 of the 91 columns | 2,226 |
| …that disagree about any reported result | **433** |

Exact duplicates by year: 2018 186 · 2024 150 · 2021 149 · 2016 45 · 2012 22 ·
2011 21 · 2008 16 · 2007 15 · 2004 12 · 2003 9 · 2000 7.

**What differs matters more than the count.** Ranked by groups affected:
`address` 1,992 · `lon` 1,345 · `lat` 1,345 · `electorate` 432 · `valid` 432 ·
`uik_old` 428 · `turnout` 428. The typical duplicate-key group is one station
carrying two addresses and two coordinate pairs — an **address-merge artifact**,
not conflicting returns. Only 433 groups disagree about a vote count.

> Earlier releases of this documentation stated "2,561 groups, 469 of which differ
> in content." That was wrong and internally inconsistent: 2,561 − 469 = 2,092
> supposedly-identical groups would require ≥ 2,092 exact duplicate rows, and only
> 632 exist.

## 6 · Mixed encoding flags — checked, **not** a defect

Recorded here because it looks alarming and has already been mis-reported once.

`red.Rds` has 14 character columns and 8 of them carry both encoding flags — the
same column holds some strings marked UTF-8 and others marked `"unknown"`:

| column | UTF-8 | unknown |
|---|---:|---:|
| `address` | 450,089 | 594,510 |
| `tik` | 1,023,368 | 21,231 |
| `oik_name` | 1,021,798 | 22,801 |
| `tik_old` | 1,044,572 | 27 |
| `source_primary` | 0 | 1,044,599 |

**This is benign.** Every string containing a non-ASCII byte is correctly flagged
UTF-8; the `"unknown"`-flagged strings are pure ASCII, where `"unknown"` is R's
normal marking and carries no risk. Verified:

```r
sum(sapply(names(red)[sapply(red, is.character)], function(v)
  sum(grepl("[^ -~]", red[[v]]) & Encoding(red[[v]]) != "UTF-8", na.rm = TRUE)))
# 0
```

`02_build_red_plus.R` still calls `enc2utf8()` on every character column, but as
cosmetic normalisation only — it changes no bytes. The mojibake seen in earlier
website builds (`*.mojibake.bak`, `*.pre-utf8fix.bak` in the site repo) came from
the JSON export path and the build locale, **not** from the release.

## 7 · `tik_2018` and `tik_2021` contradict their documentation

| column | populated rows | distinct values | regions | years populated |
|---|---:|---:|---:|---|
| `tik_2018` | 100,062 | **7** (`6, 9, 12, 16, 26, 28, 30`) | 50 | all 11 |
| `tik_2021` | 140,463 | **7** (all Yekaterinburg district names) | 81 | all 11 |

The codebook describes both as commission names anchored to 2018/2021 boundaries.
`tik_2018` holds integers, not names; `tik_2021` spreads seven Yekaterinburg
districts across 81 regions. Both are populated in all eleven elections, which a
year-anchored column should not be. **Do not use either for analysis.** They are
candidates for repair in `scripts/06_harmonize_precincts_to_2024.R` or for removal
(reducing the documented schema to 89 columns).

## 8 · Yakutia 2000 handbook rows

All 791 rows with `source_primary = "Mikhailov et al. 2000, Electoral Handbook"`
are from Sakha (Yakutia), 2000 — the only hand-digitised block in RED.

- 5 rows with `turnout > electorate` (max `turnout_rate` **927.87%**)
- 3 rows with `valid > turnout`
- 1 row with a non-integer `valid` (0.985)
- 80 rows where `turnout ≠ valid + invalid`

Nobody else has these returns at all. They need filtering before arithmetic, not
deletion. `red_plus.Rds` flags them (`flag_source_handbook`,
`flag_arith_impossible`).

## 9 · 2000 presidential votes do not exhaust `valid`

The 2000 ballot had eleven candidates; the release carries eight columns
(Govorukhin, Skuratov, Podberezkin and Dzhabrailov are absent). The residual
`valid − Σ(eight named)` is **771,739.6 votes = 1.0646% of valid**, non-zero on
**81.86%** of rows and **negative on 27**. Every other election reconciles exactly
(2024 residual = 0).

A user dividing by their own column sum gets systematically wrong 2000 shares.
Divide by `valid`, or use `other_votes` / `other_share` in `red_plus.Rds`.

## 10 · Two turnout definitions

`turnout_rate` is `turnout / electorate`. The website's maps and scatters use
`(valid + invalid) / electorate`. The difference is `ballot_gap = turnout −
(valid + invalid)`, non-zero on:

| 2000 | 2003 | 2004 | 2007 | 2008 | 2011 | 2012 | 2016 | 2018 | 2021 | 2024 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 18.26% | 15.14% | 11.70% | 13.54% | 12.68% | 12.68% | 12.78% | 12.07% | 11.11% | 12.07% | 9.35% |

Almost always positive, median 2 ballots. Clerical at the median — **not** a fraud
measure at an individual precinct.

## 11 · Smaller items

| Item | Detail |
|---|---|
| **Zero-electorate rows** | 53 total: 2000 (19), 2007 (8), 2008 (1), 2012 (1), 2021 (17), 2024 (7). `turnout/electorate` divides by zero. |
| **Absentee voting** | `turnout_absentee` is populated 2000–2016 and **exactly 0** for 2018, 2021, 2024 — absentee certificates were abolished. The codebook's "2000–2024" is misleading. |
| **2016 coordinates** | 96,301 rows, **90,308 with an address, 0 with coordinates** — the only election where addresses exist without them. Recoverable by inheritance for ~94.7% of rows. |
| **`tik` is not a key** | 113 harmonized commission names recur across regions; `(region, uik)` maps to exactly one `tik` for all 195,339 units (0 violations). |
| **`uik` is not a key** | 43,028 `(year, uik)` groups hold more than one row; one `uik` value occurs in up to **76** regions in a single election. |
| **Municipality polygons** | `rayon_geo.json` (website asset, not the release) has no polygons for Adygea, Udmurtia, Moscow or St Petersburg; Belgorod has 1 where it should have ~20; 67 polygons carry a null region. |
| **Channel identity** | `onsite + offsite + early == turnout` holds on **100%** of precinct rows. Safe to build on. |
| **Online rows** | 59 rows (32 in 2021, 27 in 2024), `turnout` is `NA` on all of them, 10,361,769 valid+invalid votes. Exclude with `tik != "online_voting"`. |

---

## Correction status

| Issue | Corrected in `red_plus.Rds` | Still open in `red.Rds` |
|---|:--:|:--:|
| Mixed encoding flags | cosmetic only — **not a defect** | n/a |
| Chukotka longitudes | ✓ wrapped | ✓ |
| Impossible latitudes | ✓ set to NA | ✓ |
| Missing coordinates 2000–2011, 2016 | ✓ inherited + `coord_source` | ✓ |
| Coordinate drift | flagged | ✓ |
| Duplicates | flagged | ✓ |
| Yakutia arithmetic | flagged | ✓ |
| 2000 residual | `other_votes` added | ✓ |
| Two turnout definitions | `ballot_gap` added | ✓ |
| Adygea / Udmurtia / Belgorod / Lipetsk geocoding | **not fixable here** — needs the address merge re-run | ✓ |
| `tik_2018` / `tik_2021` | **not fixable here** — needs diagnosis in `06_` | ✓ |
