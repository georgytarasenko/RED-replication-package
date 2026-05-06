# Harmonize precincts up to year 2024 and save the RED dataset ----

# Load data
uiks_2012_2024_addressed <- readRDS("data/05_address_data.rds")

uiks_2012_2024_addressed <- uiks_2012_2024_addressed %>%
  group_by(year) %>%
  mutate(id_from = row_number(),  # for flagged
         id_to = row_number())    # for matching targets


match_flagged_addresses_tikwise <- function(from_year, to_year, data, max_dist = 0.05, diagnostics = TRUE, n_diagnostics = 5) {


  # Use only pre-normalized and pre-labeled data
  from_data <- data %>%
    filter(year == from_year, flagged == 1)

  # Identify stable (non-flagged) uiks that exist in both years
  nonflag_stable_uiks <- data %>%
    filter(year %in% c(from_year, to_year)) %>%
    group_by(region, tik, uik) %>%
    summarise(years_present = n_distinct(year), .groups = "drop") %>%
    filter(years_present == 2) %>%
    inner_join(
      data %>% filter(year == from_year, flagged == 0),
      by = c("region", "tik", "uik")
    ) %>%
    select(region, tik, uik)
  
  to_data <- data %>%
    filter(year == to_year) %>%
    anti_join(nonflag_stable_uiks, by = c("region", "tik", "uik"))

  # Match by shauiks_harmonized region_tik groups
  tik_keys <- intersect(unique(from_data$region_tik), unique(to_data$region_tik))



  tik_matches <- map_dfr(tik_keys, function(tik_key) {
    block_from <- from_data %>% filter(region_tik == tik_key)
    block_to <- to_data %>% filter(region_tik == tik_key)

    if (nrow(block_from) == 0 || nrow(block_to) == 0) return(NULL)

    dist_mat <- stringdistmatrix(block_from$addr_string, block_to$addr_string, method = "jw")

    matched <- tibble()
    used_to <- c()

    for (i in seq_len(nrow(dist_mat))) {
      dists <- dist_mat[i, ]
      dists[used_to] <- Inf  # prevent reuse
      
      min_dist <- min(dists)
      if (is.infinite(min_dist) || min_dist > max_dist) next
      
      # Electorate tiebreaker: when multiple candidates tie at min distance,
      # prefer the one with closest electorate
      tied <- which(dists == min_dist)
      if (length(tied) == 1) {
        j <- tied
      } else {
        elec_diffs <- abs(block_from$electorate[i] - block_to$electorate[tied])
        j <- tied[which.min(elec_diffs)]
      }
      
      matched <- bind_rows(matched, tibble(
        id_from = block_from$id_from[i],
        addr_from = block_from$addr_string[i],
        addr_to = block_to$addr_string[j],
        id_to = block_to$id_to[j],
        dist = min_dist,
        n_tied = length(tied),
        elec_from = block_from$electorate[i],
        elec_to = block_to$electorate[j],
        elec_diff_pct = round(abs(block_from$electorate[i] - block_to$electorate[j]) /
                                block_from$electorate[i] * 100, 1),
        region_tik = tik_key
      ))
      used_to <- c(used_to, j)
    }

    matched
  })

  # Calculate coverage and exact matches
  n_from <- nrow(from_data)
  n_matched <- n_distinct(tik_matches$id_from)
  n_exact <- sum(tik_matches$dist == 0)

  coverage <- round(n_matched / n_from * 100, 2)
  exact_pct <- round(n_exact / n_from * 100, 2)

  cat("✅ Matched", n_matched, "of", n_from, "flagged addresses from", from_year, "to", to_year,
      "(", coverage, "%)\n")
  cat("🎯 Exact matches (distance == 0):", n_exact, "(", exact_pct, "% of all flagged)\n")
  cat("⚖️  Matches with ties broken by electorate:", sum(tik_matches$n_tied > 1, na.rm = TRUE), "\n")
  cat("📊 Matches with >50% electorate drift:", sum(tik_matches$elec_diff_pct > 50, na.rm = TRUE), "\n")

  # Optional diagnostics
  if (diagnostics) {
    unmatched_ids <- setdiff(from_data$id_from, tik_matches$id_from)
    unmatched <- from_data %>% filter(id_from %in% unmatched_ids)
    if (nrow(unmatched) > 0) {
      cat("🔍 Sampling", min(n_diagnostics, nrow(unmatched)), "unmatched cases:\n")
      print(unmatched %>% select(region, tik, locality, street, number) %>% head(n_diagnostics))
    }
  }

  return(tik_matches)
}


matches_2012_2016 <- match_flagged_addresses_tikwise(2012, 2016, uiks_2012_2024_addressed)
matches_2016_2018 <- match_flagged_addresses_tikwise(2016, 2018, uiks_2012_2024_addressed)
matches_2018_2021 <- match_flagged_addresses_tikwise(2018, 2021, uiks_2012_2024_addressed)
matches_2021_2024 <- match_flagged_addresses_tikwise(2021, 2024, uiks_2012_2024_addressed)

all_matches <- bind_rows(
  matches_2012_2016 %>% mutate(from_year = 2012, to_year = 2016),
  matches_2016_2018 %>% mutate(from_year = 2016, to_year = 2018),
  matches_2018_2021 %>% mutate(from_year = 2018, to_year = 2021),
  matches_2021_2024 %>% mutate(from_year = 2021, to_year = 2024)
)

get_uik_match_panel <- function(matches_df, from_year, to_year, data) {
  data_from <- data %>% filter(year == from_year) %>%
    select(id_from, region, tik, uik_old = uik)

  data_to <- data %>% filter(year == to_year) %>%
    select(id_to, uik_new = uik)

  matches_df %>%
    left_join(data_from, by = "id_from") %>%
    left_join(data_to, by = "id_to") %>%
    mutate(year = from_year) %>%
    select(year, region, tik, uik_new, uik_old)
}


match_panel <- bind_rows(
  get_uik_match_panel(matches_2012_2016, 2012, 2016, uiks_2012_2024_addressed),
  get_uik_match_panel(matches_2016_2018, 2016, 2018, uiks_2012_2024_addressed),
  get_uik_match_panel(matches_2018_2021, 2018, 2021, uiks_2012_2024_addressed),
  get_uik_match_panel(matches_2021_2024, 2021, 2024, uiks_2012_2024_addressed)
)



# Update UIKS from 2012 and earlier to post-2012 numbering ----

# Load the full panel of uik numbers, partially harmonized up to 2012 numbering
harmonized_2000_2012 <- readRDS("data/03_harmonized_2000_2012.rds") #

# Rename the data and ensure year is character
uiks_harmonized <- harmonized_2000_2012 %>% mutate(year = as.character(year))

# Define transitions
transitions <- list(
  list(old = "2012", new = "2016"),
  list(old = "2016", new = "2018"),
  list(old = "2018", new = "2021"),
  list(old = "2021", new = "2024")
)

# Global counter to ensure uniqueness of newly assigned UIKs
conflict_counter <- 200000

# Apply transitions iteratively
for (tr in transitions) {
  old_year <- tr$old
  new_year <- tr$new
  cat("\n------------------------------------------------------------\n")
  cat("Processing address-based transition", old_year, "→", new_year, "...\n")
  
  mapping_results <- match_panel %>% filter(year == old_year)
  cat("→ Found", nrow(mapping_results), "matches.\n")
  
  if (nrow(mapping_results) > 0) {
    assigned_uiks <- mapping_results %>% 
      filter(uik_old != uik_new) %>%
      select(region, tik, uik_new) %>% 
      distinct()
    
    # Find flagged-but-unmatched UIKs
    unmatched_flagged <- uiks_harmonized %>%
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
    preexisting_conflict_uiks <- uiks_harmonized %>%
      filter(year == old_year) %>%
      inner_join(assigned_uiks, by = c("region", "tik", "uik" = "uik_new")) %>%
      anti_join(will_be_updated, by = c("region", "tik", "uik"))
    
    # Step 3: Combine both into a conflict pool
    all_conflicting <- bind_rows(unmatched_flagged, preexisting_conflict_uiks) %>%
      select(region, tik, uik) %>%
      distinct()
    
    if (nrow(all_conflicting) > 0) {
      cat("⚠ Resolving", nrow(all_conflicting), "conflicting unmatched UIKs...\n")
      
      # Identify all uiks that exist in current or previous years that match the conflicting ones
      conflict_keys <- uiks_harmonized %>%
        filter(paste(region, tik, uik) %in% paste(all_conflicting$region, all_conflicting$tik, all_conflicting$uik),
               as.numeric(year) <= as.numeric(old_year)) %>%
        select(region, tik, uik) %>%
        distinct()
      
      # Assign globally unique new uik values
      conflict_mapping <- conflict_keys %>%
        arrange(region, tik, uik) %>%
        mutate(new_uik = conflict_counter + row_number())
      
      # Increment the global counter
      conflict_counter <- conflict_counter + nrow(conflict_mapping)
      
      # Apply update across affected earlier years
      uiks_harmonized <- uiks_harmonized %>%
        left_join(conflict_mapping, by = c("region", "tik", "uik")) %>%
        mutate(
          uik = if_else(!is.na(new_uik) & as.numeric(year) <= as.numeric(old_year), new_uik, uik)
        ) %>%
        select(-new_uik)
    }
    
    
    mapping_results_extended <- mapping_results %>% mutate(join_year = as.numeric(old_year))
    
    uiks_harmonized <- uiks_harmonized %>%
      mutate(year_num = as.numeric(year)) %>%
      left_join(mapping_results_extended, by = c("region", "tik", "uik" = "uik_old"), suffix = c("", "_map")) %>%
      mutate(
        uik = case_when(
          year_num <= join_year & !is.na(uik_new) ~ uik_new,
          TRUE ~ uik
        )
      ) %>%
      select(-uik_new, -join_year, -year_map, -year_num)
  }
  cat("✔ Transition", old_year, "→", new_year, "applied.\n")
}

# Check for duplicates
check_dupes <- uiks_harmonized %>%
  count(region, tik, year, uik) %>%
  filter(n > 1)

if (nrow(check_dupes) > 0) {
  warning("❗ Duplicates found after harmonization!")
  print(check_dupes)
} else {
  cat("✅ No duplicates after harmonization.\n")
}


# # Save intermediate result
# saveRDS(uiks_harmonized, 'data/06_uiks_harmonized.Rds')
# cat("\n📦 Final harmonized dataset with 2000–2024 saved as 'uiks_harmonized'.\n")
# uiks_harmonized <- readRDS('06_uiks_harmonized.rds')

# Merge harmonized uiks with further variables and online voting ----
harmo_keyed <- uiks_harmonized %>%
  mutate(year = as.character(year)) %>%
  select(year, region, tik, uik, uik_old)

# Combine harmonized uiks with further electoral variables and addresses based on original uik keys

## load cleaned electoral variables
el_off <- readRDS('data/01_elec_offline.rds') %>%
  mutate(year = as.character(year))
el_on  <- readRDS('data/01_elec_online.rds') %>%
  mutate(year = as.character(year))

## recover address fields from original addressed dataset
address_info <- uiks_2012_2024_addressed %>%
  select(year, region, tik, uik, lon, lat, address)

## merge harmonized uiks with further electoral variables and addresses
joined_off <- harmo_keyed %>%
  left_join(el_off, by = c("year", "region", "tik", "uik_old" = "uik")) %>%
  left_join(address_info, by = c("year", "region", "tik", "uik_old" = "uik"))

# Append online voting back (no harmonization / no address join for it)
RED <- bind_rows(
  joined_off,
  el_on %>% mutate(uik_old = uik))

# Save final result ----
saveRDS(RED, 'data/red.Rds')
write_csv(RED, "data/red.csv")
cat("📄 RED: Russian Electoral Data saved as 'red.csv'")

