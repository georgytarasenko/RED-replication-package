###############################################################################
# RED: Russian Electoral Data
# Authors: Konstantin Bogatyrev, Georgy Tarasenko, Nikita Savin
# Purpose: Run all steps to reproduce the dataset and analysis from raw data
# Usage:   Set working directory to this folder, then: source("00_master.R")
# Tested:  R 4.3.1 on macOS 15 (Apple Silicon / x86-64)
###############################################################################

rm(list = ls())

# ── 0. Package management ─────────────────────────────────────────────────────
# Install pacman if missing; it handles all subsequent installs and loads.
if (!"pacman" %in% installed.packages()) install.packages("pacman")

# Packages for dataset reproduction
pacman::p_load(
  DBI, furrr, fuzzyjoin, httr, jsonlite,
  progress, progressr,
  RSQLite, stringdist, stringi, tidyverse
)

# Additional packages for paper analyses (figures, maps, tables)
pacman::p_load(
  forcats, ggspatial, ggplot2, openxlsx, patchwork,
  rnaturalearth, rnaturalearthdata, scales, sf,
  tibble, tidyr, xtable
)

# ── Helper: source scripts while printing section headers ────────────────────
source("scripts/00_function_source_by_steps.R")

# ── DATASET ───────────────────────────────────────────────────────────────────

# 1. Clean and merge raw election data
run_with_stage_progress("scripts/01_clean_merge_elections.R")

# 2. Check and flag precinct missingness
run_with_stage_progress("scripts/02_check_flag_precinct_missingness.R")

# 3. Harmonize precincts for pre-2012 elections
run_with_stage_progress("scripts/03_harmonize_precincts_pre_2012.R")

# 4. Merge 2012-2024 addresses with precincts
run_with_stage_progress("scripts/04_merge_addresses.R")

# 5. Clean and harmonize addresses
run_with_stage_progress("scripts/05_clean_addresses.R")

# 6. Harmonize precincts to 2024 numbering (saves final red.Rds)
run_with_stage_progress("scripts/06_harmonize_precincts_to_2024.R")

# ── PAPER ANALYSES ────────────────────────────────────────────────────────────

# 7. Produce figures (Figs 1-5)        → output/figures/
run_with_stage_progress("scripts/07_paper_figures.R")

# 8. Produce maps (density maps)       → output/figures/
run_with_stage_progress("scripts/08_paper_maps.R")

# 9. Produce tables (Tables 2, 4, 5)   → output/tables/
run_with_stage_progress("scripts/09_paper_tables.R")
