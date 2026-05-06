# Merge UIKs in 2012+ with address data

# Load data
uiks_all_elec <- readRDS("data/02_elec_to_harmonize.rds")

# Merge UIKs & addresses ----
# Function to clean the SQLite polling station address data
clean_uiks <- function(uiks_data) {
  uiks_data %>%
    filter(type_ik == "uik", region != "foreign") %>%
    mutate(address_voteroom = coalesce(address_voteroom, address)) %>%
    select(reg_latin = region,
           uik = name,
           address = address_voteroom,
           lat = lat_voteroom,
           lon = lon_voteroom) %>%
    distinct(reg_latin, uik, address, lat, lon) %>%
    mutate(
      lat = readr::parse_number(str_replace(lat, "(?<=\\.)\\s+(?=\\d)", "")), # unlike as.numeric, avoids whitespace→NA warnings
      lon = readr::parse_number(str_replace(lon, "(?<=\\.)\\s+(?=\\d)", "")), # unlike as.numeric, avoids whitespace→NA warnings
      uik = as.numeric(str_extract(uik, "\\d+$")),
      reg_latin = str_replace_all(reg_latin, "-", "_"), # standardize the region names over the years
      address = str_replace_all(address, "([:,.;])\\s*", "\\1 "), # ensure punctuation is followed by a space
      address = str_replace_all(address, "\\s{2,}", " "),         # replace double+ spaces w/ a single space
      address = str_replace(address, "[.\\s]+$", ""),             # remove trailing periods and spaces
      address = str_replace(address, "^[,\\s]+", ""),             # remove leading spaces and commas
      # Remove words "address" from the address (case-insensitive, with optional parentheses)
      address = str_replace_all(address, "(?i)(?:\\(?)(адрес(?: уик(?: и (?:помещ(?:ения|ение) для голосования)?)?)?|
                                (?:(?:здание|помещение))?по адресу|адрес помещения для голосования|
                                (?:здание|помещение)(?: для)? избирательного участка|избирательный участок №\\d+|
                                \\(адресной привязки нет\\)|помещение УИК)(?:\\))?:?\\s*", ""),
      address = str_replace_all(address, regex(",\\s*помещение по.*$", ignore_case = TRUE), ""), # delete trailing repetitions
      postcode = str_extract(address, "^[0-9]{6}"),  # extract a 6-digit postcode, if present
      address = str_remove(address, "^[0-9]{6},\\s*"), # remove the postcode from the address
      address = str_replace(address, "^(Российская Федерация|Россия),\\s*", ""),
      address = str_replace(address, "обл\\.", "область"),
      # insert the region values in Russian like in the electoral data
      region = str_extract(address, "(?<=,\\s)[^,]+(?=,)"),
      region = case_when(
        reg_latin == "adygei"             ~ "Республика Адыгея",
        reg_latin == "altai_rep"          ~ "Республика Алтай",
        reg_latin == "bashkortostan"      ~ "Республика Башкортостан",
        reg_latin == "buriat"             ~ "Республика Бурятия",
        reg_latin == "dagestan"           ~ "Республика Дагестан",
        reg_latin == "ingush"             ~ "Республика Ингушетия",
        reg_latin == "kabardin_balkar"    ~ "Кабардино-Балкарская Республика",
        reg_latin == "kalmyk"             ~ "Республика Калмыкия",
        reg_latin == "karachaev_cherkess" ~ "Карачаево-Черкесская Республика",
        reg_latin == "karel"              ~ "Республика Карелия",
        reg_latin == "komi"               ~ "Республика Коми",
        reg_latin == "mari_el"            ~ "Республика Марий Эл",
        reg_latin == "mordov"             ~ "Республика Мордовия",
        reg_latin == "yakut"              ~ "Республика Саха (Якутия)",
        reg_latin == "n_osset_alania"     ~ "Республика Северная Осетия - Алания",
        reg_latin == "tatarstan"          ~ "Республика Татарстан (Татарстан)",
        reg_latin == "tyva"               ~ "Республика Тыва",
        reg_latin == "udmurt"             ~ "Удмуртская Республика",
        reg_latin == "khakas"             ~ "Республика Хакасия",
        reg_latin == "chechen"            ~ "Чеченская Республика",
        reg_latin == "chuvash"            ~ "Чувашская Республика - Чувашия",
        reg_latin == "altai_terr"         ~ "Алтайский край",
        reg_latin == "zabkray"            ~ "Забайкальский край",
        reg_latin == "kamchatka_krai"     ~ "Камчатский край",
        reg_latin == "krasnodar"          ~ "Краснодарский край",
        reg_latin == "krasnoyarsk"        ~ "Красноярский край",
        reg_latin == "permkrai"           ~ "Пермский край",
        reg_latin == "primorsk"           ~ "Приморский край",
        reg_latin == "stavropol"          ~ "Ставропольский край",
        reg_latin == "khabarovsk"         ~ "Хабаровский край",
        reg_latin == "amur"               ~ "Амурская область",
        reg_latin == "arkhangelsk"        ~ "Архангельская область",
        reg_latin == "astrakhan"          ~ "Астраханская область",
        reg_latin == "belgorod"           ~ "Белгородская область",
        reg_latin == "bryansk"            ~ "Брянская область",
        reg_latin == "vladimir"           ~ "Владимирская область",
        reg_latin == "volgograd"          ~ "Волгоградская область",
        reg_latin == "vologod"            ~ "Вологодская область",
        reg_latin == "voronezh"           ~ "Воронежская область",
        reg_latin == "ivanovo"            ~ "Ивановская область",
        reg_latin == "irkutsk"            ~ "Иркутская область",
        reg_latin == "kaliningrad"        ~ "Калининградская область",
        reg_latin == "kaluga"             ~ "Калужская область",
        reg_latin == "kemerovo"           ~ "Кемеровская область - Кузбасс",
        reg_latin == "kirov"              ~ "Кировская область",
        reg_latin == "kostroma"           ~ "Костромская область",
        reg_latin == "kurgan"             ~ "Курганская область",
        reg_latin == "kursk"              ~ "Курская область",
        reg_latin == "leningrad_reg"      ~ "Ленинградская область",
        reg_latin == "lipetsk"            ~ "Липецкая область",
        reg_latin == "magadan"            ~ "Магаданская область",
        reg_latin == "moscow_reg"         ~ "Московская область",
        reg_latin == "murmansk"           ~ "Мурманская область",
        reg_latin == "nnov"               ~ "Нижегородская область",
        reg_latin == "novgorod"           ~ "Новгородская область",
        reg_latin == "novosibirsk"        ~ "Новосибирская область",
        reg_latin == "omsk"               ~ "Омская область",
        reg_latin == "orenburg"           ~ "Оренбургская область",
        reg_latin == "orel"               ~ "Орловская область",
        reg_latin == "penza"              ~ "Пензенская область",
        reg_latin == "pskov"              ~ "Псковская область",
        reg_latin == "rostov"             ~ "Ростовская область",
        reg_latin == "ryazan"             ~ "Рязанская область",
        reg_latin == "samara"             ~ "Самарская область",
        reg_latin == "saratov"            ~ "Саратовская область",
        reg_latin == "sakhalin"           ~ "Сахалинская область",
        reg_latin == "sverdlovsk"         ~ "Свердловская область",
        reg_latin == "smolensk"           ~ "Смоленская область",
        reg_latin == "tambov"             ~ "Тамбовская область",
        reg_latin == "tver"               ~ "Тверская область",
        reg_latin == "tomsk"              ~ "Томская область",
        reg_latin == "tula"               ~ "Тульская область",
        reg_latin == "tyumen"             ~ "Тюменская область",
        reg_latin == "ulyanovsk"          ~ "Ульяновская область",
        reg_latin == "chelyabinsk"        ~ "Челябинская область",
        reg_latin == "yaroslavl"          ~ "Ярославская область",
        reg_latin == "moscow_city"        ~ "город Москва",
        reg_latin == "st_petersburg"      ~ "город Санкт-Петербург",
        reg_latin == "jewish_aut"         ~ "Еврейская автономная область",
        reg_latin == "nenetsk"            ~ "Ненецкий автономный округ",
        reg_latin == "khantu_mansy"       ~ "Ханты-Мансийский автономный округ - Югра",
        reg_latin == "chukot"             ~ "Чукотский автономный округ",
        reg_latin == "yamal_nenetsk"      ~ "Ямало-Ненецкий автономный округ",
        TRUE ~ region),
      # remove region names from the address
      address = str_replace(address, regex(paste0("^", region, ",\\s*"), ignore_case = TRUE), ""),
      # remove other variants of region names from the address
      address = str_replace(address, regex("^(город\\s+Москва|г\\.\\s*Москва|гор\\.\\s*Москва)\\,\\s*", ignore_case = TRUE), ""),
      address = str_replace(address, regex("^(город\\s+Санкт[ -]?Петербург|г\\.\\s*Санкт[ -]?Петербург|гор\\.\\s*Санкт[ -]?Петербург)\\,\\s*", ignore_case = TRUE), ""),
      address = str_replace(address, regex("^(Республика\\s+[^,]+)\\,\\s*", ignore_case = TRUE), ""),
      address = str_replace(address, regex("^(Кемеровская\\s+область(?:\\s*-\\s*Kузбасс)?)\\,\\s*", ignore_case = TRUE), ""),
      address = str_replace(address, regex("^(Ханты[-\\s]*Мансийский\\s+автономный\\s+округ(?:\\s*-\\s*Югра)?)\\,\\s*", ignore_case = TRUE), "")
      )
  
}

# Function to load the target table "cik_uik" from the SQLite file given its path
load_sqlite_uiks <- function(file_path) {
  con <- dbConnect(SQLite(), file_path)
  on.exit(dbDisconnect(con), add = TRUE)
  suppressWarnings(dbReadTable(con, "cik_uik")) # suppress the warning about mixed-type lat and lon vars
}


## 2024 addresses ----
uiks_2024_raw <- load_sqlite_uiks("data/raw_addresses/uiks_20240314.sqlite")
uiks_2024 <- clean_uiks(uiks_2024_raw)
# Join to election data for 2024 (filter election data to year 2024)
uiks_all_elec_2024 <- uiks_all_elec %>%
  filter(year == "2024") %>%
  left_join(uiks_2024, by = c("region", "uik"))

## 2021 addresses ----
uiks_2021_raw <- load_sqlite_uiks("data/raw_addresses/uiks_20210914.sqlite")
uiks_2021 <- clean_uiks(uiks_2021_raw)
uiks_all_elec_2021 <- uiks_all_elec %>%
  filter(year == "2021") %>%
  left_join(uiks_2021, by = c("region", "uik"))

## 2018 addresses ----
uiks_2018_raw <- load_sqlite_uiks("data/raw_addresses/uiks_20180215.sqlite")
uiks_2018 <- clean_uiks(uiks_2018_raw)
uiks_all_elec_2018 <- uiks_all_elec %>%
  filter(year == "2018") %>%
  left_join(uiks_2018, by = c("region", "uik"))

## 2016 addresses ----
uiks_2016_raw <- load_sqlite_uiks("data/raw_addresses/uiks_20160629.sqlite")
# Manually add missing columns for 2016, setting them to NA
uiks_2016_raw <- uiks_2016_raw %>%
  mutate(address_voteroom = NA_character_,
         lat_voteroom = NA_real_,
         lon_voteroom = NA_real_)
uiks_2016 <- clean_uiks(uiks_2016_raw)
uiks_all_elec_2016 <- uiks_all_elec %>%
  filter(year == "2016") %>%
  left_join(uiks_2016, by = c("region", "uik"))

## 2012 addresses ----
uik2012numbers <- read_csv("data/raw_addresses/uiks_2012/voting_station.csv") %>%
  select(uik = name, address, location_id)

uik2012location <- read.delim("data/raw_addresses/uiks_2012/location.csv", 
                              sep = ";", dec = ".") %>%
  select(location_id = id, 2:9)

uik2012street <- read_csv("data/raw_addresses/uiks_2012/street.csv") %>%
  select(street_id = id, street = name)

uik2012locality <- read_csv("data/raw_addresses/uiks_2012/locality.csv") %>%
  select(locality_id = id, locality = name)

uik2012municipality <- read_csv("data/raw_addresses/uiks_2012/sub_area.csv") %>%
  select(sub_area_id = id, municipality = name)

uik2012region <- read_csv("data/raw_addresses/uiks_2012/area.csv") %>%
  select(area_id = id, region = name)

# Merge the 2012 components into one dataframe
uiks_2012 <- uik2012numbers %>%
  left_join(uik2012location, by = "location_id") %>%
  left_join(uik2012street, by = "street_id") %>%
  left_join(uik2012locality, by = "locality_id") %>%
  left_join(uik2012municipality, by = "sub_area_id") %>%
  left_join(uik2012region, by = "area_id") %>%
  select(uik, lat, lon, address, raw_address, region, municipality, locality, street) %>%
  mutate(region = str_replace_all(region, "республика", "Республика"),
         region = str_replace(region, "^Республика (.*кая)$", "\\1 Республика"),
         region = case_when(
           locality == "Москва" ~ "город Москва",
           locality == "Санкт-Петербург" ~ "город Санкт-Петербург",
           str_detect(region, "^Республика.*-кая$") ~ paste0(str_remove(region, "^Республика\\s*"), " Республика"),
           region == "Ингушская Республика" ~ "Республика Ингушетия",
           region == "Республика Татарстан" ~ "Республика Татарстан (Татарстан)",
           region == "Республика Северная Осетия-Алания" ~ "Республика Северная Осетия - Алания",
           region == "Чувашская Республика" ~ "Чувашская Республика - Чувашия",
           region == "Кемеровская область" ~ "Кемеровская область - Кузбасс",
           region == "Ханты-Мансийский автономный округ" ~ "Ханты-Мансийский автономный округ - Югра",
           TRUE ~ region),
         address = str_replace_all(address, "([:,.;])\\s*", "\\1 "),    # standardize punctuation spacing
         address = str_replace_all(address, "\\s{2,}", " "),            # collapse multiple spaces
         address = str_replace(address, "[.\\s]+$", ""),                # remove trailing dots/spaces
         address = str_replace(address, "^[,\\s]+", ""),                # remove leading commas/spaces
         # Remove words "address" from the address (case-insensitive, with optional parentheses)
         address = str_replace_all(address, "(?i)(?:\\(?)(адрес(?: уик(?: и (?:помещ(?:ения|ение) для голосования)?)?)?|
                                (?:(?:здание|помещение))?по адресу|адрес помещения для голосования|
                                (?:здание|помещение)(?: для)? избирательного участка|избирательный участок №\\d+|
                                \\(адресной привязки нет\\)|помещение УИК)(?:\\))?:?\\s*", ""),
         address = str_replace_all(address, regex(",\\s*помещение по.*$", ignore_case = TRUE), ""), # delete trailing repetitions
         address = str_replace(address, "^(Российская Федерация|Россия),\\s*", ""),
         # Remove the region names from the address
         address = str_replace(address, regex(paste0("^", region, ",\\s*"), ignore_case = TRUE), ""),
          # including variants of these names
         address = str_replace(address, regex("^(город\\s+Москва|г\\.\\s*Москва|гор\\.\\s*Москва)\\,\\s*", ignore_case = TRUE), ""),
         address = str_replace(address, regex("^(город\\s+Санкт[ -]?Петербург|г\\.\\s*Санкт[ -]?Петербург|гор\\.\\s*Санкт[ -]?Петербург)\\,\\s*", ignore_case = TRUE), ""),
         address = str_replace(address, regex("^(Республика\\s+[^,]+)\\,\\s*", ignore_case = TRUE), ""),
         address = str_replace(address, regex("^(Кемеровская\\s+область(?:\\s*-\\s*Kузбасс)?)\\,\\s*", ignore_case = TRUE), ""),
         address = str_replace(address, regex("^(Ханты[-\\s]*Мансийский\\s+автономный\\s+округ(?:\\s*-\\s*Югра)?)\\,\\s*", ignore_case = TRUE), ""),
         # Update region for new Moscow addresses (Щербинка or Троицк cases) and flag them
         new_moscow = if_else(
           region == "Московская область" & (str_starts(coalesce(raw_address, ""), "Московская область, городской округ Щербинка") |
                coalesce(locality, "") == "Троицк"), 1, 0),
         region = if_else(new_moscow == 1, "город Москва", region)
  ) %>% distinct() # remove full duplicates

# Attach 2012 addresses to the 2012 election data 
uiks_all_elec_2012 <- uiks_all_elec %>%
  filter(year == "2012") %>%
  mutate(new_moscow = if_else(tik %in% c("Троицкая", "Новомосковская"), 1, 0)) %>%
  left_join(uiks_2012, by = c("region", "uik", "new_moscow")) %>%
  select(-new_moscow)

# Combine All Election Data with Addresses
elec_with_address <- bind_rows(uiks_all_elec_2024, uiks_all_elec_2021, uiks_all_elec_2018, 
                                uiks_all_elec_2016, uiks_all_elec_2012)

# Save final result ----
saveRDS(elec_with_address, file = "data/04_elec_with_address.rds")
# write_csv(elec_with_address, file = "04_elec_with_address.csv")

cat("Saved data/04_elec_with_address.rds\n")
cat("Rows:", nrow(elec_with_address), "Columns:", ncol(elec_with_address), "\n")