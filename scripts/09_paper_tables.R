# ============================================================
# Script 09: Paper Tables (Tables 2, 4, 5)
# Reproduces key summary tables; numbers update automatically
# when the pipeline is re-run.
# ============================================================

library(dplyr)
library(tidyr)
library(tibble)
library(xtable)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

path_data   <- "data/"
path_output <- "output/tables/"
if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# ---- Load data ----
red      <- readRDS(paste0(path_data, "red.Rds"))
cat("RED rows:", nrow(red), "| cols:", ncol(red), "\n\n")

# Election year → label lookup
etype <- c(
  "2000" = "Presidential",   "2003" = "Parliamentary",
  "2004" = "Presidential",   "2007" = "Parliamentary",
  "2008" = "Presidential",   "2011" = "Parliamentary",
  "2012" = "Presidential",   "2016" = "Parliamentary",
  "2018" = "Presidential",   "2021" = "Parliamentary",
  "2024" = "Presidential"
)
years_all <- names(etype)

# All rows in RED have type "parl" or "pres" — no separate online rows.
# Use the full dataset directly for Tables 2 and 4.
offline <- red

# ============================================================
# TABLE 2: Dataset Coverage
# ============================================================

tab2 <- offline %>%
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  summarise(
    n_regions = n_distinct(region),
    n_tiks    = n_distinct(paste(region, tik)),
    n_uiks    = n_distinct(paste(region, tik, uik_old)),
    .groups   = "drop"
  ) %>%
  arrange(match(year, years_all)) %>%
  mutate(`Election Type` = etype[year]) %>%
  select(Year = year, `Election Type`,
         `# Regions` = n_regions, `# TIKs` = n_tiks, `# UIKs` = n_uiks)

overall_row <- tibble(
  Year           = "Overall",
  `Election Type` = "",
  `# Regions`    = sum(tab2$`# Regions`),
  `# TIKs`       = sum(tab2$`# TIKs`),
  `# UIKs`       = sum(tab2$`# UIKs`)
)
tab2_full <- bind_rows(tab2, overall_row)

cat("=== TABLE 2: Dataset Coverage ===\n")
print(as.data.frame(tab2_full), row.names = FALSE)
cat("\n")

# Save LaTeX
xt2 <- xtable(tab2_full,
              caption = "Dataset Coverage",
              label   = "tab:coverage",
              digits  = 0)
print(xt2,
      file              = paste0(path_output, "table2_coverage.tex"),
      include.rownames  = FALSE,
      caption.placement = "bottom",
      format.args       = list(big.mark = ","))
cat("-> saved table2_coverage.tex\n\n")

# ============================================================
# TABLE 4: Top-10 Panel Presence Patterns
# ============================================================

# Build a wide presence matrix: one row per unique precinct (region+tik+uik_old),
# one column per election year, value = 1 if precinct observed that year.
presence_wide <- offline %>%
  mutate(year = as.character(year)) %>%
  select(region, tik, uik_old, year) %>%
  distinct() %>%
  mutate(present = 1L) %>%
  pivot_wider(names_from = year, values_from = present, values_fill = 0L)

# Ensure all 11 year columns exist (some may be absent if no data)
for (y in years_all) {
  if (!y %in% names(presence_wide)) presence_wide[[y]] <- 0L
}
presence_wide <- presence_wide %>%
  select(region, tik, uik_old, all_of(years_all))

# Build a pattern string and count occurrences
presence_wide <- presence_wide %>%
  mutate(pattern = apply(select(., all_of(years_all)), 1, paste, collapse = "-"))

top10 <- presence_wide %>%
  count(pattern, sort = TRUE) %>%
  slice_head(n = 10)

# Expand pattern strings back into year columns
pattern_mat <- do.call(rbind, strsplit(top10$pattern, "-"))
colnames(pattern_mat) <- years_all

# Console version with Unicode checkmarks
tab4_console <- as_tibble(pattern_mat) %>%
  mutate(across(everything(), as.integer)) %>%
  mutate(across(all_of(years_all), ~ifelse(.x == 1L, "✓", " "))) %>%
  bind_cols(Count = format(top10$n, big.mark = ","))
names(tab4_console) <- c("'00","'03","'04","'07","'08","'11","'12","'16","'18","'21","'24","Count")

cat("=== TABLE 4: Top-10 Panel Presence Patterns ===\n")
print(as.data.frame(tab4_console), row.names = FALSE)
cat(sprintf("Total Unique Precincts (top-10 shown): %s\n",
            format(sum(top10$n), big.mark = ",")))
cat(sprintf("Total Unique Precincts (all patterns): %s\n\n",
            format(nrow(presence_wide), big.mark = ",")))

# LaTeX version with $\checkmark$
tab4_tex <- as_tibble(pattern_mat) %>%
  mutate(across(everything(), as.integer)) %>%
  mutate(across(all_of(years_all), ~ifelse(.x == 1L, "$\\checkmark$", ""))) %>%
  bind_cols(Count = format(top10$n, big.mark = ","))
names(tab4_tex) <- c("'00","'03","'04","'07","'08","'11","'12","'16","'18","'21","'24","Count")

xt4 <- xtable(tab4_tex,
              caption = paste0(
                "Top-10 Panel Presence Patterns in the Harmonized Dataset. ",
                sprintf("Total Unique Precincts (top-10 shown): %s",
                        format(sum(top10$n), big.mark = ","))
              ),
              label = "tab:patterns")
print(xt4,
      file                    = paste0(path_output, "table4_patterns.tex"),
      include.rownames        = FALSE,
      caption.placement       = "bottom",
      sanitize.text.function  = identity)
cat("-> saved table4_patterns.tex\n\n")

# ============================================================
# TABLE 5: Election Data Comparison (RED vs Official)
# ============================================================

# Official CEC-reported totals (hardcoded reference; do not change)
official <- tribble(
  ~year, ~election_type,               ~off_turnout,    ~off_voters,
  "2000","Presidential Election",        75070776,       109372046,
  "2003","Parliamentary Election",       60712301,       109373000,
  "2004","Presidential Election",        69504278,       108064281,
  "2007","Parliamentary Election",       69537065,       109145517,
  "2008","Presidential Election",        74717992,       107222016,
  "2011","Parliamentary Election",       65722412,       109820679,
  "2012","Presidential Election",        71701665,       109860331,
  "2016","Parliamentary Election",       53646688,       111724534,
  "2018","Presidential Election",        73578992,       109008428,
  "2021","Parliamentary Election",       56483685,       109204662,
  "2024","Presidential Election",        82985310.5,     108091103
)

# Sum RED totals per year (all rows: offline UIKs + online entries)
red_totals <- red %>%
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  summarise(
    red_turnout = sum(turnout,    na.rm = TRUE),
    red_voters  = sum(electorate, na.rm = TRUE),
    .groups     = "drop"
  )

tab5 <- official %>%
  left_join(red_totals, by = "year") %>%
  arrange(match(year, years_all)) %>%
  mutate(
    prec_turnout = sprintf("%.2f%%", red_turnout / off_turnout * 100),
    prec_voters  = sprintf("%.2f%%", red_voters  / off_voters  * 100)
  ) %>%
  select(
    Year             = year,
    `Election Type`  = election_type,
    `Off. Turnout`   = off_turnout,
    `Off. Voters`    = off_voters,
    `RED Turnout`    = red_turnout,
    `RED Voters`     = red_voters,
    `Prec. Turnout`  = prec_turnout,
    `Prec. Voters`   = prec_voters
  )

cat("=== TABLE 5: Election Data Comparison ===\n")
print(as.data.frame(tab5), row.names = FALSE)
cat("\n")

# LaTeX: format large numbers with commas for numeric cols
tab5_tex <- tab5 %>%
  mutate(
    `Off. Turnout` = format(round(`Off. Turnout`), big.mark = ","),
    `Off. Voters`  = format(`Off. Voters`,  big.mark = ","),
    `RED Turnout`  = format(round(`RED Turnout`), big.mark = ","),
    `RED Voters`   = format(round(`RED Voters`),  big.mark = ",")
  )

xt5 <- xtable(tab5_tex,
              caption = "Election Data Comparison (Database vs Official Reported)",
              label   = "tab:comparison")
print(xt5,
      file              = paste0(path_output, "table5_comparison.tex"),
      include.rownames  = FALSE,
      caption.placement = "bottom",
      sanitize.text.function = identity)
cat("-> saved table5_comparison.tex\n\n")

cat("=== All tables saved to:", path_output, "===\n")
