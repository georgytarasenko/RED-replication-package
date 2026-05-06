# Script 05: Attach LLM-parsed address fields to new dataset
# Strategy: reuse old05 address→parsed-fields lookup (99.99% coverage)
# Only 26 addresses in the new dataset were not in the old one — left as NA
# No LLM calls needed.

library(dplyr)
library(stringr)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# ---- 1. Load datasets ----
elec_with_address <- readRDS("data/04_elec_with_address.rds")

old05 <- readRDS("../старое/previous dataset version/05_address_data.rds")

cat("New dataset rows :", nrow(elec_with_address), "\n")
cat("Old 05 rows      :", nrow(old05), "\n")

# ---- 2. Build address → parsed fields lookup from old05 ----
# Pull only the columns that came from the LLM parsing step
llm_cols <- c(
  "address",
  "муниципалитет",
  "тип населенного пункта",
  "locality1",       # населенный пункт
  "street1",         # улица
  "number1",         # домовой номер
  "house_name"       # название дома
)

lookup <- old05 %>%
  select(any_of(llm_cols)) %>%
  distinct(address, .keep_all = TRUE)   # one row per unique address

cat("Lookup table rows:", nrow(lookup), "\n")

# ---- 3. Join lookup onto new dataset ----
all_data <- elec_with_address %>%
  left_join(lookup, by = "address")

matched   <- sum(!is.na(all_data$locality1))
unmatched <- sum(is.na(all_data$locality1) & !is.na(all_data$address))
cat("Matched addresses  :", matched, "\n")
cat("Unmatched (→ NA)   :", unmatched, "\n")

# ---- 4. Normalization functions (unchanged from original script 05) ----
normalize_address <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]\\\"]", " ", x)
  x <- gsub("\\b(городское поселение|город|г\\.?|пгт|поселок|п\\.)\\b\\s*", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_street <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]\\\"]", " ", x)
  x <- gsub("\\bв\\. ?о\\.", "васильевского острова", x)
  x <- gsub("\\b(ул|улица)\\b", "", x)
  x <- gsub("\\b1го мая\\b|\\b1ое мая\\b", "1 мая", x)
  x <- gsub("\\b([0-9]+)(-?й|-?я|-?ое|го|ая|ий)\\b", "\\1", x)
  x <- gsub("\\b(дом культуры|сельский дом культуры|сдк)\\b", "дк", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_number <- function(x) {
  x <- tolower(x)
  x <- gsub("нет данных|n/a|не указано", "", x)
  x <- gsub("^\\s*(д\\.?|дом\\.?|номер|№|домик|домовл\\.?|дм\\.?|домовладение)\\s*", "", x)
  x <- gsub("\\bдом\\b", "", x)
  x <- gsub("\\bкорпус\\b", "к", x)
  x <- gsub("\\bкорп\\.", "к", x)
  x <- gsub("\\bстроение\\b", "с", x)
  x <- gsub("\\bстр\\.?", "с", x)
  x <- gsub("-+", "/", x)
  x <- gsub("/+", "/", x)
  x <- gsub("[ ]", " ", x)
  x <- chartr("abekmnopxy", "абекмнорху", x)
  x <- gsub("\\s+", "", x)
  x <- gsub("([0-9]+)/([а-я])$", "\\1/\\U\\2", x, perl = TRUE)
  x <- gsub("([0-9]+)[\\s/-]*([а-я])$", "\\1\\U\\2", x, perl = TRUE)
  x <- gsub("([0-9]+)к([0-9]+)", "\\1к\\2", x)
  x <- gsub("([0-9]+)с([0-9]+)", "\\1с\\2", x)
  x <- gsub("[^0-9а-я/кс]", "", x)
  trimws(x)
}

# ---- 5. Clean and build composite address string ----
all_data <- all_data %>%
  mutate(across(c(locality1, street1, number1), ~na_if(.x, "NA"))) %>%
  mutate(street1 = str_remove_all(street1, "\\s*(улица|ул\\.?)(\\s+|$)")) %>%
  mutate(
    region_tik = paste(region, tik),
    addr_string = paste0(
      normalize_address(locality1), " ",
      normalize_street(street1), " ",
      normalize_number(number1)
    ),
    id_from = row_number()
  )

# ---- 6. Final column selection ----
uiks_2012_2024_addressed <- all_data %>%
  select(
    year, region, tik, uik,
    electorate, flagged, reg_latin, address,
    lat, lon, postcode, raw_address,
    municipality_geo = municipality,   # from geocoding (already in new04)
    locality_geo     = locality,       # from geocoding
    street_geo       = street,         # from geocoding
    municipality_llm = `муниципалитет`,
    locality_type    = `тип населенного пункта`,
    locality         = locality1,
    street           = street1,
    number           = number1,
    building_name    = house_name,
    region_tik,
    addr_string
  )

cat("\nFinal dataset rows :", nrow(uiks_2012_2024_addressed), "\n")
cat("Final dataset cols :", ncol(uiks_2012_2024_addressed), "\n")

# ---- 7. Save ----
saveRDS(uiks_2012_2024_addressed, file = "data/05_address_data.rds")
cat("Saved to data/05_address_data.rds\n")
