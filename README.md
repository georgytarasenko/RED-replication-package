# RED: Russian Electoral Data — Replication Package

**Citation:** Tarasenko, Georgy; Bogatyrev, Konstantin; Savin, Nikita, 2026, "Russian Election Data", <https://doi.org/10.7910/DVN/DFGNTP>, Harvard Dataverse, V1

---

## Overview

This replication package contains the raw data and scripts needed to reproduce the RED dataset and all figures and tables in the accompanying paper. The pipeline covers Russian presidential and parliamentary election data from 2000 to 2024 (six presidential elections, five parliamentary elections) and produces the harmonized panel dataset described in the paper.

The final dataset is also available directly from the Harvard Dataverse repository above for researchers who do not need to re-run the full pipeline.

---

## License

Creative Commons Attribution 4.0 International (CC BY 4.0)
<https://creativecommons.org/licenses/by/4.0/>

You are free to use, share, and adapt the materials provided you give appropriate credit and indicate if changes were made. Please cite the paper above when using the RED dataset or any replication materials.

---

## Software Requirements

| Software | Tested version |
|---|---|
| R | 4.3.1 |
| Operating system | macOS 15 (Apple Silicon / x86-64); also runs on Windows and Linux |
| RAM | ≥ 8 GB recommended (spatial join in script 08 is memory-intensive) |
| Disk space | ~3 GB for raw and processed data |

### R packages (tested versions)

| Package | Version |
|---|---|
| DBI | 1.2.3 |
| forcats | 1.0.0 |
| furrr | 0.3.1 |
| fuzzyjoin | 0.1.6.1 |
| ggspatial | 1.1.9 |
| ggplot2 | 3.5.1 |
| httr | 1.4.7 |
| jsonlite | 1.8.9 |
| openxlsx | 4.2.7.1 |
| patchwork | 1.2.0 |
| progress | 1.2.3 |
| progressr | 0.18.0 |
| rnaturalearth | 1.1.0 |
| rnaturalearthdata | 1.0.0 |
| RSQLite | 2.4.4 |
| scales | 1.4.0 |
| sf | 1.0.19 |
| stringdist | 0.9.15 |
| stringi | 1.8.7 |
| tibble | 3.2.1 |
| tidyr | 1.3.1 |
| tidyverse | 2.0.0 |
| xtable | 1.8.4 |

All packages can be installed automatically by running `00_master.R` (uses `pacman::p_load()`).

**Runtime note:** Script `08_paper_maps.R` performs a spatial join (`sf::st_intersects`) that takes approximately 10 minutes on a modern laptop. All other scripts complete in under 5 minutes total.

---

## How to Replicate

1. Install R (version 4.0 or higher).
2. Set your working directory to the root of this folder:
   ```r
   setwd("/path/to/replication_package")
   ```
3. Run the master script:
   ```r
   source("00_master.R")
   ```
   This executes all nine steps sequentially and saves outputs to `output/`.

To reproduce only figures and tables (skipping dataset construction), load `data/red.Rds` directly and source scripts 07–09.

---

## File Structure

```
replication_package/
│
├── 00_master.R                        # Run this to reproduce everything
│
├── scripts/
│   ├── 00_function_source_by_steps.R  # Helper: source with section-header logging
│   ├── 01_clean_merge_elections.R     # Clean and merge raw election CSVs
│   ├── 02_check_flag_precinct_missingness.R  # Flag missing precincts
│   ├── 03_harmonize_precincts_pre_2012.R     # Harmonize pre-2012 precinct IDs
│   ├── 04_merge_addresses.R           # Merge 2012-2024 addresses with precincts
│   ├── 05_clean_addresses.R           # Clean and standardize address strings
│   ├── 06_harmonize_precincts_to_2024.R      # Harmonize all IDs to 2024 numbering
│   ├── 07_paper_figures.R             # Reproduce Figs 1-5
│   ├── 08_paper_maps.R                # Reproduce density maps (Fig in paper)
│   └── 09_paper_tables.R              # Reproduce Tables 2, 4, 5
│
├── R/
│   └── figure_style_nature.R          # Color palette and save function for figures
│
├── data/
│   ├── red.Rds                        # Final harmonized dataset (main output)
│   ├── grid_layout_ru_regions.xlsx    # Grid layout for region facet plots
│   ├── hex_russia_2024.rds            # Hexagon shapefile for Russia (2024 boundaries)
│   ├── 01_elec_offline.rds            # Intermediate: cleaned offline precinct data
│   ├── 01_elec_online.rds             # Intermediate: cleaned online voting data
│   ├── 02_elec_to_harmonize.rds       # Intermediate: precincts flagged for harmonization
│   ├── 03_harmonized_2000_2012.rds    # Intermediate: harmonized pre-2012 precincts
│   ├── 04_elec_with_address.rds       # Intermediate: election data with addresses merged
│   ├── 05_address_data.rds            # Intermediate: cleaned address data
│   ├── raw_votes/                     # Raw election CSVs (one per election year)
│   │   ├── 2000.csv, 2000_sakha.csv
│   │   ├── 2003.csv, 2004.csv, 2007.csv, 2008.csv
│   │   ├── 2011.csv, 2012.csv, 2016.csv, 2018.csv
│   │   ├── 2021.csv
│   │   └── 2024.csv, 2024-electronic-voting.csv
│   └── raw_addresses/                 # Polling station address databases
│       ├── uiks_2012/                 # 2012 address files (6 CSVs)
│       ├── uiks_20160629.sqlite
│       ├── uiks_20180215.sqlite
│       ├── uiks_20210914.sqlite
│       └── uiks_20240314.sqlite
│
└── output/
    ├── figures/                       # Figures 1-5 + maps (PNG + TIFF, 600 dpi)
    └── tables/                        # Tables 2, 4, 5 (LaTeX .tex files)
```

---

## Dataset: `data/red.Rds`

The final harmonized dataset. Each row is one polling station (UIK) in one election year. Key variables:

| Variable | Description |
|---|---|
| `year` | Election year |
| `type` | Election type: `"pres"` (presidential) or `"parl"` (parliamentary) |
| `region` | Region name (standardized) |
| `tik` | Territorial Election Commission identifier |
| `uik_old` | Original precinct number for that election year |
| `uik` | Harmonized precinct number (2024 numbering) |
| `electorate` | Number of registered voters |
| `turnout` | Number of votes cast |
| `turnout_online` | Online votes (zero for precinct rows; non-zero only in online aggregates) |
| `incumb` | Vote share for incumbent/leading party or candidate |

**Notes on data coverage and caveats:**

- Precinct identifiers are standardized to the 2024 electoral geography. These harmonized IDs may not match original printed protocols from earlier years and may change in future cycles.
- Data for Chechnya in 2000 may be incomplete.
- Online voting results are aggregated at the electoral district level (2021) or regional level (2024). Rows with `turnout_online > 0` represent these aggregates and should not be mixed with precinct-level rows in turnout analyses.
- Precinct boundary *redrawing* (as opposed to renumbering) is not captured; this may introduce measurement error in strict panel analyses.
- The 2020 constitutional referendum is not included; this dataset covers only federal elections.

---

## Outputs

### Figures (`output/figures/`)

| File | Content |
|---|---|
| `fig1_voting_modes.png/.tiff` | Voting modes and turnout trends (Figs 1a–1b in paper) |
| `fig2_digit_analysis.png/.tiff` | Last-digit distribution analysis |
| `fig3_incumbent_share_maps.png/.tiff` | Regional maps of incumbent vote share |
| `fig4_correlation_change.png/.tiff` | Correlation of turnout–incumbent-share change |
| `fig5_offsite_heatmap.png/.tiff` | Off-site voting share heatmap |
| `fig_maps_density.png/.tiff` | Polling station and electorate density maps |

All figures are 600 dpi, saved as both TIFF (LZW compression) and PNG.

### Tables (`output/tables/`)

| File | Content |
|---|---|
| `table2_coverage.tex` | Table 2: Dataset coverage by election year |
| `table4_patterns.tex` | Table 4: Top-10 panel presence patterns |
| `table5_comparison.tex` | Table 5: RED totals vs. official CEC reported totals |

Tables 1 and 3 in the paper are constructed manually from domain knowledge and are not reproduced by code.

---

## Raw Data Sources

- Dezhin, A. GIS-Lab: Dannye UIKov iz webvybory2012.ru [Polling station data from webvybory2012.ru]. <https://perma.cc/XM97-33KW> (2012).
- GIS-Lab.info. Koordinatsionnaya tablitsa i istochniki dannykh po UIK RF. <https://archive.ph/6PBk5> (2013).
- Kobak, D. Polling station data for Russian federal elections. Data scraped by Sergey Shpilkin (2000–2021) and Ivan Shukshin (2024). <https://github.com/dkobak/elections/tree/master/data> (2024).
- Mikhailov, V. N. et al. (eds.) *Vybory Prezidenta Rossiiskoi Federatsii: 26 Marta 2000 g.: Elektoralnyi Spravochnik*. Central Electoral Commission of the Republic of Sakha (Yakutia), Yakutsk, 2000.
- Shukshin, I. Izbirkomy. <https://perma.cc/H543-SB63> (2024).
