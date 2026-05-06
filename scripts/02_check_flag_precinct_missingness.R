### Сheck precinct persistence over the years
### Flag precincts that disappear or show abnormal electorate changes

# Load UIK-level Data -----
uiks_all_elec <- readRDS("data/01_elec_offline.rds") %>%
  select(year, type, region, tik, uik, electorate)

cross_tik_lookup <- readRDS("data/01_cross_tik_lookup.rds")

# Compute Next-Period Changes ----
# Create a mapping of each year to its next period.
unique_years <- sort(unique(uiks_all_elec$year))
year_map <- setNames(lead(unique_years), unique_years)

# Join next-period electorate info.
uiks_all_elec <- uiks_all_elec %>%
  mutate(year = as.character(year),
         next_year = year_map[year]) %>%
  left_join(
    uiks_all_elec %>% 
      select(uik, tik, region, year, electorate) %>% 
      rename(next_year = year, electorate_next = electorate),
    by = c("uik", "tik", "region", "next_year")
  ) %>%
  mutate(
    next_exists = if_else(!is.na(electorate_next), 1, 0),
    N_change_perc = round(((electorate_next / (electorate + 1)) - 1)*100, 1),
    N_change = electorate_next - electorate
  ) %>%
  select(-next_year)

# UIK-level Flagging (Only for years before 2024) ----
# We flag UIKs (with >50 voters) for:
# - No next period (flag_no_next)
# - Large changes (>300 voters or >50% change) (flag_large_change)
uiks_all_elec <- uiks_all_elec %>%
  mutate(
    flag_no_next = if_else(year != "2024" & next_exists == 0, 1, 0),
    flag_large_change = if_else(year != "2024" & electorate > 50 &
                                  (abs(N_change) > 300 | abs(N_change_perc) > 50),
                                1, 0)
  ) %>%
  select(-next_exists, -N_change_perc, -N_change)

# Aggregate to TIK-Level Diagnostics ----
# (A) TIKs explicitly renumbered (tiks_renumbered) based on the share of UIKs with no match.
tiks_renumbered <- uiks_all_elec %>%
  filter(year != "2024") %>%
  group_by(region, tik, year) %>%
  summarise(
    total_uiks = n(),
    renumb_uiks = sum(flag_no_next, na.rm = TRUE),
    share_renumb = renumb_uiks / total_uiks,
    .groups = "drop"
  ) %>%
  filter(share_renumb > 0.2)


# Create new UIK-level flags based on TIK-level aggregates.
uiks_all_elec <- uiks_all_elec %>%
  left_join(tiks_renumbered %>% select(region, tik, year) %>% mutate(flag_tik_renumb = 1), 
            by = c("region", "tik", "year")) %>%
  mutate(flag_tik_renumb = if_else(is.na(flag_tik_renumb), 0L, 1L))


# (B) TIKs "shuffled" (tiks_shuffled) based on the combined share of UIKs with no match or large change.
tiks_shuffled <- uiks_all_elec %>%
  filter(year != "2024", flag_tik_renumb != 1) %>%
  group_by(region, tik, year) %>%
  summarise(
    total_uiks = n(),
    problem_uiks = sum(flag_no_next, na.rm = TRUE) + sum(flag_large_change, na.rm = TRUE),
    share_problem = problem_uiks / total_uiks,
    .groups = "drop"
  ) %>%
  filter(share_problem > 0.5)

# Join TIK-Level Flags Back to UIK-Level Data ----
uiks_all_elec <- uiks_all_elec %>%
  left_join(tiks_shuffled %>% select(region, tik, year) %>% mutate(flag_tik_shuffled = 1), 
            by = c("region", "tik", "year")) %>%
  mutate(flag_tik_shuffled = if_else(is.na(flag_tik_shuffled), 0L, 1L))

# Add flag for UIKs in TIKs that are sources of cross-TIK transfers
uiks_all_elec <- uiks_all_elec %>%
  left_join(
    cross_tik_lookup %>%
      mutate(year = old_year, flag_likely_transfer = 1L) %>%
      select(region = region_from, tik = tik_from, year, flag_likely_transfer) %>%
      distinct(),
    by = c("region", "tik", "year")
  ) %>%
  mutate(
    flag_likely_transfer = replace_na(flag_likely_transfer, 0L),
    # Overall flag: 1 if any condition is met
    flagged = if_else(
      flag_tik_renumb == 1 | flag_tik_shuffled == 1 |
        flag_no_next == 1 | flag_large_change == 1 |
        flag_likely_transfer == 1,
      1, 0)
  )

# Summary & Visualization ----
flag_summary <- uiks_all_elec %>%
  summarise(
    flag_tik_renumb       = sum(flag_tik_renumb, na.rm = TRUE),
    flag_tik_shuffled     = sum(flag_tik_shuffled, na.rm = TRUE),
    flagged_no_next       = sum(flag_no_next == 1, na.rm = TRUE),
    flagged_large_change  = sum(flag_large_change == 1, na.rm = TRUE),
    flag_likely_transfer  = sum(flag_likely_transfer == 1, na.rm = TRUE),
    flagged_total         = sum(flagged, na.rm = TRUE)
  )
print(flag_summary)
rm(flag_summary, tiks_renumbered, tiks_shuffled)

# Save Outputs ----
saveRDS(uiks_all_elec, file = "data/02_elec_to_harmonize.rds")
# write_csv(uiks_all_elec, file = "02_elec_to_harmonize.csv")

cat("Saved data/02_elec_to_harmonize.rds\n")
cat("Rows:", nrow(uiks_all_elec), "Columns:", ncol(uiks_all_elec), "\n")
