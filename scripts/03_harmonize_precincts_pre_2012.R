# Harmonize UIKs with Dynamic Programming and Vectorized Updates
# - Preserve original UIK as `uik_old`
# - Match flagged UIKs using DP across transitions 2000–2011
# - Use vectorized left_join updates for speed


# Load the processed diagnostic data ----
uiks_all_elec <- readRDS("data/02_elec_to_harmonize.rds")
cross_tik_lookup <- readRDS("data/01_cross_tik_lookup.rds")

# Save the original uik identifiers as uik_old ----
harmonized_2000_2012 <- if (!"uik_old" %in% names(uiks_all_elec)) {
  uiks_all_elec %>% mutate(uik_old = uik)
} else {
  uiks_all_elec
}
cat("Initial data loaded. Total records:", nrow(harmonized_2000_2012), "\n")

# Define dynamic programming match function ----
skip_cost <- 200

match_cost <- function(e_old, e_new) {
  diff <- abs(e_old - e_new)
  if (diff > 500) return(Inf) else return(diff)
}

match_uiks_dp <- function(df_old, df_new) {
  df_old <- df_old %>% arrange(as.numeric(uik))
  df_new <- df_new %>% arrange(as.numeric(uik))
  
  n_old <- nrow(df_old)
  n_new <- nrow(df_new)
  
  dp <- matrix(0, nrow = n_old + 1, ncol = n_new + 1)
  pointer <- matrix("", nrow = n_old + 1, ncol = n_new + 1)
  
  for (j in 1:n_new) {
    dp[1, j + 1] <- dp[1, j] + skip_cost
    pointer[1, j + 1] <- "skip_new"
  }
  for (i in 1:n_old) {
    dp[i + 1, 1] <- dp[i, 1] + skip_cost
    pointer[i + 1, 1] <- "skip_old"
  }
  
  for (i in seq_len(n_old)) {
    for (j in seq_len(n_new)) {
      c_match <- match_cost(df_old$electorate[i], df_new$electorate[j])
      cost_match <- dp[i, j] + c_match
      cost_skip_old <- dp[i, j + 1] + skip_cost
      cost_skip_new <- dp[i + 1, j] + skip_cost
      
      best <- min(cost_match, cost_skip_old, cost_skip_new)
      dp[i + 1, j + 1] <- best
      
      if (is.finite(c_match) && best == cost_match) {
        pointer[i + 1, j + 1] <- "match"
      } else if (best == cost_skip_old) {
        pointer[i + 1, j + 1] <- "skip_old"
      } else {
        pointer[i + 1, j + 1] <- "skip_new"
      }
    }
  }
  
  i <- n_old; j <- n_new
  matches <- list()
  while (i > 0 && j > 0) {
    move <- pointer[i + 1, j + 1]
    if (move == "match") {
      matches <- append(matches, list(
        tibble(
          uik_old = df_old$uik[i],
          electorate_old = df_old$electorate[i],
          uik_new = df_new$uik[j],
          electorate_new = df_new$electorate[j]
        )
      ))
      i <- i - 1; j <- j - 1
    } else if (move == "skip_old") {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }
  bind_rows(rev(matches))
}

# Define transitions ----
transitions <- list(
  list(old = "2000", new = "2003"),
  list(old = "2003", new = "2004"),
  list(old = "2004", new = "2007"),
  list(old = "2007", new = "2008"),
  list(old = "2008", new = "2011"),
  list(old = "2011", new = "2012")
)

# Match and update uiks ----
total_start <- Sys.time()

# Start global counter for conflict resolution numbering
conflict_counter <- 10000

for (tr in transitions) {
  old_year <- tr$old
  new_year <- tr$new
  cat("\n------------------------------------------------------------\n")
  cat("Processing transition", old_year, "→", new_year, "...\n")
  trans_start <- Sys.time()
  
  flagged_old <- harmonized_2000_2012 %>% filter(year == old_year, flagged == 1)
  
  # Identify stable (non-flagged) uiks that exist in both years
  nonflag_stable_uiks <- harmonized_2000_2012 %>%
    filter(year %in% c(old_year, new_year)) %>%
    group_by(region, tik, uik) %>%
    summarise(years_present = n_distinct(year), .groups = "drop") %>%
    filter(years_present == 2) %>%
    inner_join(
      harmonized_2000_2012 %>% filter(year == old_year, flagged == 0),
      by = c("region", "tik", "uik")
    ) %>%
    select(region, tik, uik)
  
  # Filter new-year candidates to exclude those stable, non-flagged uiks
  candidates_new <- harmonized_2000_2012 %>%
    filter(year == new_year) %>%
    anti_join(nonflag_stable_uiks, by = c("region", "tik", "uik"))
  
  flagged_tiks <- flagged_old %>% distinct(region, tik)
  
  df_pair <- bind_rows(
    flagged_old %>% mutate(year = as.numeric(old_year)),
    candidates_new %>% mutate(year = as.numeric(new_year))
  ) %>% inner_join(flagged_tiks, by = c("region", "tik"))
  
  groups_list <- df_pair %>% group_by(region, tik) %>% group_split()
  cat("Transition", old_year, "→", new_year, ": ", length(groups_list), "groups.\n")
  
  if (length(groups_list) == 0) {
    cat("⚠ No groups to match. Skipping.\n")
    next
  }
  
  pb <- progress_bar$new(
    format = "Matching [:bar] :current/:total (:percent) ETA: :eta",
    total = length(groups_list),
    clear = FALSE, width = 60
  )
  
  mapping_results <- map_dfr(seq_along(groups_list), function(i) {
    pb$tick()
    df_group <- groups_list[[i]]
    df_old <- df_group %>% filter(year == as.numeric(old_year))
    df_new <- df_group %>% filter(year == as.numeric(new_year))
    if (nrow(df_old) == 0 || nrow(df_new) == 0) return(tibble())
    match_uiks_dp(df_old, df_new) %>% mutate(
      region = first(df_group$region),
      tik = first(df_group$tik)
    )
  }) %>% ungroup()
  
  cat("\n→ Found", nrow(mapping_results), "within-TIK matches.\n")

  # Cross-TIK matching for TIKs with territorial transfers 
  ct_trans <- cross_tik_lookup %>%
    filter(old_year == !!old_year, new_year == !!new_year)
  
  cross_tik_matches <- tibble()
  
  if (nrow(ct_trans) > 0) {
    cat("Running cross-TIK DP for", nrow(ct_trans), "TIK pairs...\n")
    
    for (k in seq_len(nrow(ct_trans))) {
      ct <- ct_trans[k, ]
      
      # All flagged UIKs in source TIK — no anti_join, we compare costs below
      source_uiks <- harmonized_2000_2012 %>%
        filter(year == old_year, region == ct$region_from, tik == ct$tik_from,
               flagged == 1)
      
      # New UIKs in target TIK only (exclude stable ones)
      stable_to <- harmonized_2000_2012 %>%
        filter(year %in% c(old_year, new_year),
               region == ct$region_to, tik == ct$tik_to) %>%
        group_by(region, tik, uik) %>%
        filter(n_distinct(year) == 2) %>%
        ungroup()
      
      candidates_to <- harmonized_2000_2012 %>%
        filter(year == new_year, region == ct$region_to, tik == ct$tik_to) %>%
        anti_join(stable_to, by = c("region", "tik", "uik"))
      
      if (nrow(source_uiks) == 0 || nrow(candidates_to) == 0) next
      
      dp_cross <- match_uiks_dp(source_uiks, candidates_to) %>%
        mutate(
          region     = ct$region_from,
          tik        = ct$tik_from,
          tik_new    = ct$tik_to,
          cost_cross = abs(electorate_old - electorate_new)
        )
      
      if (nrow(dp_cross) == 0) next
      
      # Compare against existing within-TIK matches for the same UIKs
      within_this_tik <- mapping_results %>%
        filter(region == ct$region_from, tik == ct$tik_from) %>%
        mutate(cost_within = abs(electorate_old - electorate_new))
      
      contested <- dp_cross %>%
        inner_join(within_this_tik %>% select(uik_old, cost_within),
                   by = "uik_old")
      
      if (nrow(contested) > 0) {
        n_cross_wins <- sum(contested$cost_cross < contested$cost_within)
        prop_cross_wins <- n_cross_wins / nrow(contested)
        
        if (prop_cross_wins >= 0.5) {
          # Block-level: cross-TIK wins for the entire sequence
          cat("  Block-level replacement:", nrow(contested),
              "UIKs reassigned to cross-TIK (",
              round(prop_cross_wins * 100), "% individually better).\n")
          mapping_results <- mapping_results %>%
            anti_join(contested %>% select(region, tik, uik_old),
                      by = c("region", "tik", "uik_old"))
        } else {
          # Block-level: within-TIK wins, drop all contested cross-TIK matches
          cat("  Block-level decision: keeping within-TIK (",
              round(prop_cross_wins * 100), "% cross-TIK better, below threshold).\n")
          dp_cross <- dp_cross %>%
            anti_join(contested %>% select(uik_old), by = "uik_old")
        }
      }
      
      cross_tik_matches <- bind_rows(cross_tik_matches, dp_cross)
    }
    
    cat("Cross-TIK matches found:", nrow(cross_tik_matches), "\n")
  }
  
  # Apply cross-TIK matches — reassign both tik and uik retrospectively
  if (nrow(cross_tik_matches) > 0) {
    
    # Resolve target collisions: when two source UIKs match to the same
    # (tik_new, uik_new), keep only the lower-cost one
    cross_tik_matches <- cross_tik_matches %>%
      group_by(tik_new, uik_new) %>%
      slice_min(cost_cross, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # Resolve source duplicates: when the same source UIK appears with 
    # multiple targets, keep only the lowest-cost target
    cross_tik_matches <- cross_tik_matches %>%
      group_by(region, tik, uik_old) %>%
      slice_min(cost_cross, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    cross_tik_map <- cross_tik_matches %>%
      select(region, tik, uik_old, tik_new, uik_new) %>%
      distinct()
    
    harmonized_2000_2012 <- harmonized_2000_2012 %>%
      left_join(cross_tik_map,
                by = c("region", "tik", "uik" = "uik_old")) %>%
      mutate(
        tik = if_else(!is.na(tik_new) & as.numeric(year) <= as.numeric(old_year),
                      tik_new, tik),
        uik = if_else(!is.na(uik_new) & as.numeric(year) <= as.numeric(old_year),
                      uik_new, uik)
      ) %>%
      select(-tik_new, -uik_new)
  }
  
  cat("Cross-TIK matches this transition:", nrow(cross_tik_matches), "\n")
    
  # Prevent uik duplication by renumbering unmatched uiks 
  if (nrow(mapping_results) > 0) {
    assigned_uiks <- bind_rows(
      mapping_results %>%
        filter(uik_old != uik_new) %>%
        select(region, tik, uik_new),
      if (nrow(cross_tik_matches) > 0)
        cross_tik_matches %>% select(region, tik = tik_new, uik_new)
      else
        tibble(region = character(), tik = character(), uik_new = numeric())
    ) %>% distinct()
    
    # Step 1: Find originally flagged but unmatched uiks
    unmatched_flagged <- harmonized_2000_2012 %>%
      filter(year == old_year, flagged == 1) %>%
      anti_join(mapping_results %>% select(region, tik, uik_old),
                by = c("region", "tik", "uik" = "uik_old"))
    
    # Step 2: Find preexisting uiks that conflict with newly assigned numbers
    # Get all UIKs in old_year that are going to be updated via matching
    will_be_updated <- mapping_results %>%
      mutate(year = old_year) %>%
      select(region, tik, uik = uik_old) %>%
      distinct()
    
    # Only flag as conflicts those that won't be updated themselves
    preexisting_conflict_uiks <- harmonized_2000_2012 %>%
      filter(year == old_year) %>%
      inner_join(assigned_uiks, by = c("region", "tik", "uik" = "uik_new")) %>%
      anti_join(will_be_updated, by = c("region", "tik", "uik"))
    
    # Step 3: Combine both into a conflict pool
    all_conflicting <- bind_rows(unmatched_flagged, preexisting_conflict_uiks) %>%
      select(region, tik, uik) %>%
      distinct()
    
    if (nrow(all_conflicting) > 0) {
      cat("⚠ Resolving", nrow(all_conflicting), "conflicts (with global counter)...\n")
      
      conflict_keys <- harmonized_2000_2012 %>%
        filter(paste(region, tik, uik) %in% paste(all_conflicting$region, all_conflicting$tik, all_conflicting$uik),
               as.numeric(year) <= as.numeric(old_year)) %>%
        select(region, tik, uik) %>%
        distinct()
      
      # Assign unique new_uik values using global counter
      conflict_mapping <- conflict_keys %>%
        mutate(new_uik = conflict_counter + row_number())
      
      # Update counter so next iteration continues from where we left off
      conflict_counter <- conflict_counter + nrow(conflict_mapping)
      
      # Apply the updates
      harmonized_2000_2012 <- harmonized_2000_2012 %>%
        left_join(conflict_mapping, by = c("region", "tik", "uik")) %>%
        mutate(
          uik = if_else(!is.na(new_uik) & as.numeric(year) <= as.numeric(old_year), new_uik, uik)
        ) %>%
        select(-new_uik)
    }
  }
  
  # Proceed with usual update
  if (nrow(mapping_results) > 0) {
    cat("→ Applying updates to harmonized dataset via join...\n")
    
    mapping_results_extended <- mapping_results %>%
      mutate(join_year = as.numeric(old_year))
    
    harmonized_2000_2012 <- harmonized_2000_2012 %>%
      mutate(year_num = as.numeric(year)) %>%
      left_join(mapping_results_extended, by = c("region", "tik", "uik" = "uik_old"), suffix = c("", "_map")) %>%
      mutate(
        uik = case_when(
          year_num <= join_year & !is.na(uik_new) ~ uik_new,
          TRUE ~ uik
        )
      ) %>%
      select(-electorate_old, -electorate_new, -uik_new, -join_year, -year_num)
  }
  
  trans_end <- Sys.time()
  cat("✔ Transition", old_year, "→", new_year, "done in", round(difftime(trans_end, trans_start, units = "secs"), 2), "seconds.\n")
}

total_end <- Sys.time()
cat("\n✅ Full harmonization completed in", round(difftime(total_end, total_start, units = "mins"), 2), "minutes.\n")

check_dupes <- harmonized_2000_2012 %>%
  count(region, tik, year, uik) %>%
  filter(n > 1)

suppressWarnings({
  if (nrow(check_dupes) > 0) {
    warning("❗ Duplicates found after harmonization!")
    print(check_dupes)
  } else {
    cat("✅ No duplicates after harmonization.\n")
  }
})


# harmonized_2000_2012 <- harmonized_2000_2012 %>%
#   select(1:6, uik_old)

# Save final result ----
saveRDS(harmonized_2000_2012, file = "data/03_harmonized_2000_2012.rds")
# write_csv(harmonized_2000_2012, file = "03_harmonized_2000_2012.csv")

cat("Saved data/03_harmonized_2000_2012.rds\n")
cat("Rows:", nrow(harmonized_2000_2012), "Columns:", ncol(harmonized_2000_2012), "\n")


# clean the environment

rm(harmonized_2000_2012, uiks_all_elec,
  transitions, groups_list, df_pair, mapping_results,
  mapping_results_extended, assigned_uiks, unmatched_flagged,
  preexisting_conflict_uiks, all_conflicting, conflict_keys,
  conflict_mapping, conflict_counter,
  check_dupes, pb,
  candidates_new, flagged_old, flagged_tiks,
  nonflag_stable_uiks, tr, will_be_updated,
  new_year, old_year, skip_cost, total_end, total_start,
  trans_end, trans_start, match_cost, match_uiks_dp)

gc()

