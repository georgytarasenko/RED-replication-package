### 01b_read_2026.R
### Read the 2026 State Duma precinct snapshot (party-list ballot) and return it in the SAME raw layout
### that 01_clean_merge_elections.R builds for the earlier elections, so that 2026 passes through the
### identical cleaning steps (alphabet fix, TIK name normalisation, TIK recodes, region filters, turnout).
###
### Source: Spiceman161/duma-election-2026, release research-2026-09-30 (CSV package, SHA-256 verified).
###         Collected research snapshot of the CEC (cikrf.ru) protocols; NOT certified final results.
### Sourced from 01_clean_merge_elections.R (see the two hook points at the bottom of this file).
### Produces:
###   parl_2026         offline precinct rows, same columns as parl_2021 BEFORE the oik split (needs `oik` as text)
###   parl_2026_online  ДЭГ (remote electronic voting) rows aggregated by single-member district, same layout as 2021 online
###   precinct_aux_2026 one row per precinct with everything RED does not store (uik_id, tik_code, address, coordinates,
###                     protocol status, 2026 district) -> saved to data/01b_2026_precinct_aux.rds for scripts 04 and 06
###
### Design choices (all deliberate, see the notes in the sandbox README):
###   * Party-list ballot only. The single-member ballot is not used, consistent with RED 2003/2007/2011/2016/2021.
###   * Only ordinary precinct protocols (dataset == "uik") go to the offline frame; ДЭГ rows (dataset == "deg")
###     go to the online frame. A ДЭГ protocol is never joined to a physical precinct.
###   * Crimea, Sevastopol and the territory code RU-00 (abroad + Baikonur) are removed by the region filter in
###     script 01 step 6 (RU-00 has no region name and is dropped here).
###   * Single-member districts (SMD) are out of scope for now: offline rows get oik = NA. The 2026 district number
###     (numbering differs from 2016/2021: only ~11% of matched precincts keep their number) is kept only in the
###     auxiliary table and, as oik_old, on the ДЭГ rows, which are reported at district level.
###   * `turnout_onsite` = ballots issued in the polling station, `turnout_offsite` = ballots issued outside,
###     `turnout_early` = early votes; script 01 step 8 then builds turnout = early + offsite + onsite as for all years.

# ---- 0. Settings -------------------------------------------------------------------------------------------
snapshot_dir <- Sys.getenv("RED_2026_DIR", unset = "data/raw_votes/2026_snapshot_2026-09-30")
if (!dir.exists(snapshot_dir)) {
  stop("2026 snapshot directory not found: ", snapshot_dir,
       "\nUnzip duma-2026-csv-2026-09-30.zip there, or set the environment variable RED_2026_DIR.")
}
for (f in c("protocols.csv", "party_results.csv", "precincts.csv")) {
  if (!file.exists(file.path(snapshot_dir, f))) stop("Missing file in snapshot directory: ", f)
}

# Party name in the snapshot -> RED column name (same names as 2016/2021 where the party is the same).
# Matched on party_name, NOT party_id: ordinary-precinct rows carry transliterated ids, but ДЭГ rows carry
# hashed ids ("party-" + SHA-256 prefix of the name, created by the packager), so ids differ between the two.
party_key_2026 <- function(party_name) {
  case_when(
    str_detect(party_name, "ЕДИНАЯ РОССИЯ")              ~ "UR",
    str_detect(party_name, "КПРФ")                       ~ "KPRF",
    str_detect(party_name, "ЛДПР")                       ~ "LDPR",
    str_detect(party_name, "СПРАВЕДЛИВАЯ РОССИЯ")        ~ "SR",
    str_detect(party_name, "НОВЫЕ ЛЮДИ")                 ~ "NovyeLyudi",
    str_detect(party_name, "ЗЕЛ[ЁЕ]НЫЕ")                 ~ "Zelenye",
    str_detect(party_name, "РОДИНА")                     ~ "Rodina",
    str_detect(party_name, "КОММУНИСТЫ РОССИИ")          ~ "KPKR",
    str_detect(party_name, "ПАРТИЯ ПЕНСИОНЕРОВ")         ~ "PartiyaPensionerov",
    str_detect(party_name, "(?i)прямой демократии")      ~ "PPD",   # new column: Партия прямой демократии
    TRUE ~ NA_character_)
}
# NB: PDPS in RED = «Партия Роста» (ex-«Правое дело»), not «Партия прямой демократии» -> hence the new column PPD.
party_cols_2026 <- c("UR", "KPRF", "LDPR", "SR", "NovyeLyudi", "Zelenye", "Rodina", "KPKR", "PartiyaPensionerov", "PPD")

# ---- 1. Read the snapshot -----------------------------------------------------------------------------------
rd <- function(f) read_csv(file.path(snapshot_dir, f), show_col_types = FALSE, progress = FALSE, guess_max = 1e6)
protocols  <- rd("protocols.csv") %>% mutate(region_code = as.character(region_code))
party_res  <- rd("party_results.csv")
precincts  <- rd("precincts.csv") %>% mutate(region_code = as.character(region_code), tik_code = as.character(tik_code),
                                             uik_number = as.integer(uik_number))

# ---- 2. Party-list protocols: ordinary precincts (offline) and ДЭГ (online) --------------------------------
prot_pl <- protocols %>% filter(ballot_type == "party_list")

# internal consistency of the protocol lines (identities of the CEC protocol form)
chk <- prot_pl %>%
  mutate(id_a = (ballots_received + unaccounted_ballots) -
                (early_votes + ballots_issued_stationary + ballots_issued_outside + cancelled_ballots + lost_ballots),
         id_b = (invalid_ballots + valid_ballots) - (portable_box_ballots + stationary_box_ballots))
if (any(chk$id_a != 0, na.rm = TRUE) || any(chk$id_b != 0, na.rm = TRUE)) {
  warning("Protocol identities violated in ", sum(chk$id_a != 0 | chk$id_b != 0, na.rm = TRUE), " rows")
}
rm(chk)

# party votes -> wide, one column per party in RED naming
party_names <- party_res %>% distinct(party_id, party_name) %>% mutate(party = party_key_2026(party_name))
if (any(is.na(party_names$party))) stop("Parties without a RED column name: ",
                                         paste(party_names$party_name[is.na(party_names$party)], collapse = " | "))
if (!setequal(party_names$party, party_cols_2026) || n_distinct(party_names$party_name) != length(party_cols_2026)) {
  stop("Party names do not map one-to-one onto the ten expected parties")
}

party_wide <- party_res %>%
  filter(protocol_key %in% prot_pl$protocol_key) %>%
  mutate(party = party_key_2026(party_name)) %>%
  select(protocol_key, party, votes) %>%
  pivot_wider(names_from = party, values_from = votes)
if (!all(party_cols_2026 %in% names(party_wide))) stop("Some parties missing in party_results")
if (anyDuplicated(party_wide$protocol_key) > 0) stop("Duplicated party rows for one protocol")

n_pl <- nrow(prot_pl)
prot_pl <- prot_pl %>% inner_join(party_wide, by = "protocol_key")
if (nrow(prot_pl) != n_pl) stop("Party results missing for some protocols")
party_cols <- party_cols_2026
bad_sum <- prot_pl %>% filter(rowSums(across(all_of(party_cols))) != valid_ballots)
if (nrow(bad_sum) > 0) stop("Party votes do not add up to valid ballots in ", nrow(bad_sum), " protocols")
rm(bad_sum, party_res, party_wide, party_names, n_pl)

prot_uik <- prot_pl %>% filter(dataset == "uik")
prot_deg <- prot_pl %>% filter(dataset == "deg")
if (anyDuplicated(prot_uik$uik_id) > 0) stop("More than one party-list protocol for some uik_id")

# ---- 3. Offline frame (ordinary precincts) -------------------------------------------------------------------
# Safe TIK-name cleaning only: strips district tags that the CEC site appends to ONE commission that is split
# across several single-member districts. It is applied in script 01 (right after the online block, so tik_old
# keeps the raw name). Residual district-split suffixes ("Брянская - 81" etc.) are NOT stripped here: they are
# handled by the data-driven crosswalk data/crosswalks/tik_recode_2026.csv (script 01, section 6).
clean_tik_2026 <- function(x) {
  x %>%
    str_remove("\\s*\\(\\d{3}\\s*округ\\s*ГД\\)\\s*$") %>%   # Moscow Oblast: "Балашиха (118 округ ГД)"
    str_remove("\\s*\\(ОИК\\s*\\d+\\)\\s*$") %>%               # Khabarovsk:   "Хабаровская (ОИК 73)"
    str_remove("\\s*\\(\\d\\)\\s*$") %>%                       # Ufa:          "Уфимская  (3)"
    str_squish()
}

prec <- precincts %>%
  filter(!is.na(region_name)) %>%                # RU-00: abroad + Baikonur, no region name
  mutate(tik_clean = clean_tik_2026(tik_name))

off <- prot_uik %>%
  select(-any_of(c("region_name", "region_code", "district_number"))) %>%
  inner_join(prec %>% select(uik_id, region_code, region_name, tik_code, tik_name, uik_number, district_number),
             by = "uik_id")

n_lost <- nrow(prot_uik) - nrow(off)
if (n_lost > 0) message("01b: ", n_lost, " ordinary protocols without region name (abroad/Baikonur) dropped")

parl_2026 <- off %>%
  transmute(
    year = 2026, type = "parl",
    region = region_name,
    oik = NA_character_,                         # single-member districts are out of scope for 2026 (see header)
    tik = tik_name,                               # raw CEC name; cleaned with clean_tik_2026() in script 01 (keeps tik_old raw)
    uik = as.numeric(uik_number),
    electorate      = registered_voters,
    turnout_early   = early_votes,
    turnout_offsite = ballots_issued_outside,
    turnout_onsite  = ballots_issued_stationary,
    invalid = invalid_ballots,
    valid   = valid_ballots,
    across(all_of(party_cols))
  )

# ---- 4. Online (ДЭГ) frame: aggregated by single-member district, as in 2021 -------------------------------
# In the CEC data the electronic ballots issued are reported in the "stationary" line; RED stores them in
# turnout_online (script 01 section 4 does the same re-classification for 2021). electorate = persons registered for ДЭГ.
deg_regions <- precincts %>% distinct(region_code, region_name) %>% filter(!is.na(region_name))
parl_2026_online <- prot_deg %>%
  select(-any_of(c("region_name"))) %>%
  left_join(deg_regions, by = "region_code") %>%
  transmute(
    year = 2026, type = "parl",
    region = region_name,
    tik = "online_voting",
    electorate      = registered_voters,
    turnout_early   = early_votes,
    turnout_online  = ballots_issued_stationary + ballots_issued_outside,
    invalid = invalid_ballots,
    valid   = valid_ballots,
    oik_old = as.integer(district_number),
    across(all_of(party_cols))
  )
if (any(is.na(parl_2026_online$region))) stop("ДЭГ rows without a region name")

# ---- 5. Auxiliary table for the address-based steps (04, 06) ----------------------------------------------
precinct_aux_2026 <- prec %>%
  left_join(prot_uik %>% select(uik_id, protocol_status, repeated, registered_voters), by = "uik_id") %>%
  transmute(year = 2026, region = region_name, region_code, tik = tik_clean, tik_raw = tik_name, tik_code,
            uik = as.numeric(uik_number), uik_id, oik_2026 = district_number,
            address, locality, latitude, longitude,
            address_origin, coordinate_source, geography_match_method, coordinate_approximate,
            address_status, geocoding_precision, geocoding_kind,
            protocol_status, repeated, electorate_2026 = registered_voters,
            has_protocol = !is.na(protocol_status))
if (!dir.exists("data")) dir.create("data")
saveRDS(precinct_aux_2026, "data/01b_2026_precinct_aux.rds")

# ---- 6. Report -------------------------------------------------------------------------------------------
cat("\n01b 2026 snapshot:", nrow(parl_2026), "ordinary party-list protocols in",
    n_distinct(parl_2026$region), "regions (incl. Crimea/Sevastopol, dropped later in 01 step 6);",
    nrow(parl_2026_online), "ДЭГ district rows\n")
cat("  precincts with zero electorate:", sum(parl_2026$electorate == 0, na.rm = TRUE),
    "| with address:", sum(!is.na(precinct_aux_2026$address)),
    "| with coordinates:", sum(!is.na(precinct_aux_2026$latitude)), "\n")
cat("  protocol_status:", paste(names(table(precinct_aux_2026$protocol_status)), table(precinct_aux_2026$protocol_status), collapse = "; "), "\n")

rm(protocols, precincts, prot_pl, prot_uik, prot_deg, prec, off, deg_regions, n_lost, rd)

### Hook points in 01_clean_merge_elections.R (the sandbox copy already contains them):
###  (a) section 1 (after parl_2021):       source("scripts/01b_read_2026.R")
###  (b) section 2: add parl_2026 to the bind_rows() that builds elec_all
###  (c) section 4 (after pres_2024_online): elec_all <- bind_rows(elec_all, parl_2026_online)
