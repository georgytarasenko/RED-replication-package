# ==============================================================================
# Script 08 (Nature-compliant refactor): SPATIAL DENSITY MAPS
# Produces Figure 1 (maps) for the manuscript — two panels assembled into one
# file, replacing the original two separate PNG files.
#
# Outputs → output/figures/
#   fig_maps_density.{tiff,png}
#
# Changes from original scripts/08_paper_maps.r
#   • Sourced centralized style helper (R/figure_style_nature.R)
#   • fig1a and fig1b assembled into a single patchwork file with panel
#     labels a/b (original saved two separate PNGs, violating multi-panel policy)
#   • theme_nature_pub() applied inside theme_void layer for consistent type
#   • Colorblind-safe sequential palettes retained (blue and green — already good)
#   • save_nature_figure() used (TIFF + PNG, 600 dpi)
#   • ggspatial scale bar and north arrow retained from previous compliance pass
#   • No spatial computations, projections, or data filters changed
# ==============================================================================

# ── Style helper ──────────────────────────────────────────────────────────────
source("R/figure_style_nature.R")

# ── Additional packages ───────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(sf)
  library(rnaturalearth)
  library(ggspatial)
})

# ── Paths ─────────────────────────────────────────────────────────────────────
path_data   <- "data/"
path_output <- "output/figures/"
if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# ── Load data (UNCHANGED) ─────────────────────────────────────────────────────
elec_data <- readRDS(paste0(path_data, "red.Rds"))

# ── Data preparation (UNCHANGED) ─────────────────────────────────────────────
elec_data <- elec_data %>%
  arrange(region, tik, uik_old, year) %>%
  group_by(region, tik, uik_old) %>%
  fill(lat, lon, .direction = "downup") %>%
  ungroup()

elec_2024 <- elec_data %>%
  filter(year == 2024, !is.na(lon), !is.na(lat))

cat("Total polling stations in 2024 with coordinates:", nrow(elec_2024), "\n")

# ── Spatial setup (UNCHANGED) ─────────────────────────────────────────────────
elec_sf   <- st_as_sf(elec_2024, coords = c("lon", "lat"), crs = 4326)
elec_proj <- st_transform(
  elec_sf,
  crs = "+proj=aea +lat_1=45 +lat_2=65 +lat_0=55 +lon_0=100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
)

hex_grid    <- st_make_grid(elec_proj, cellsize = 34933.6, square = FALSE)
hex_grid_sf <- st_sf(hex_id = seq_along(hex_grid), geometry = hex_grid)
cat("Total hexagons in grid:", nrow(hex_grid_sf), "\n")

hex_aggregated <- st_join(hex_grid_sf, elec_proj) %>%
  group_by(hex_id) %>%
  summarise(
    n_stations  = n(),
    electorate  = sum(electorate, na.rm = TRUE),
    mean_turnout = mean(turnout_rate, na.rm = TRUE),
    .groups     = "drop"
  )

hex_data <- hex_aggregated %>% st_drop_geometry()

hex_full <- left_join(hex_grid_sf, hex_data, by = "hex_id") %>%
  mutate(
    n_stations = ifelse(is.na(n_stations), 0, n_stations),
    electorate = ifelse(is.na(electorate), 0, electorate)
  )

russia_sf <- ne_countries(scale = "medium", country = "Russia", returnclass = "sf")
russia_sf <- st_transform(russia_sf, st_crs(hex_full))

hex_russia <- hex_full %>%
  filter(st_intersects(geometry, russia_sf, sparse = FALSE))

cat("Hexagons within Russia:", nrow(hex_russia), "\n")
cat("Total polling stations:", sum(hex_russia$n_stations, na.rm = TRUE), "\n")

# ── Shared map theme ──────────────────────────────────────────────────────────
# Merges theme_void with the font/legend defaults from theme_nature_pub
map_theme <- function() {
  theme_void(base_size = NAT_BASE_SIZE, base_family = "sans") +
    theme(
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = NAT_LABEL_SIZE,
                                      hjust = 0.5),
      legend.text      = element_text(size = NAT_LABEL_SIZE - 1),
      legend.key.size  = unit(0.30, "cm"),
      legend.margin    = margin(0, 0, 2, 0),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.tag         = element_text(size = NAT_TAG_SIZE, face = "bold",
                                      colour = "black"),
      plot.tag.position = "topleft",
      plot.margin      = margin(4, 4, 4, 4, "pt")
    )
}

# ── Panel (a): polling station density ───────────────────────────────────────
hex_russia <- hex_russia %>%
  mutate(
    density_bin = case_when(
      n_stations == 0               ~ "0",
      n_stations == 1               ~ "1",
      n_stations >= 2  & n_stations <= 5   ~ "2-5",
      n_stations >= 6  & n_stations <= 20  ~ "6-20",
      n_stations >= 21 & n_stations <= 100 ~ "21-100",
      n_stations > 100              ~ "101+",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(density_bin = factor(density_bin,
                               levels = c("0","1","2-5","6-20","21-100","101+")))

density_colors <- c(
  "0"      = "grey82",
  "1"      = "#deebf7",
  "2-5"    = "#9ecae1",
  "6-20"   = "#4292c6",
  "21-100" = "#2171b5",
  "101+"   = "#08306b"
)

fig_map_a <- ggplot(hex_russia) +
  geom_sf(aes(fill = density_bin), colour = NA) +
  scale_fill_manual(
    name   = "Polling stations per 1,000 sq. km (2024)",
    values = density_colors, drop = FALSE,
    guide  = guide_legend(
      title.position = "top", title.hjust = 0.5,
      direction = "horizontal", nrow = 1
    )
  ) +
  annotation_scale(location = "bl", width_hint = 0.22,
                   style = "ticks", text_cex = 0.65) +
  coord_sf() +
  map_theme()

# ── Panel (b): electorate density ────────────────────────────────────────────
hex_russia <- hex_russia %>%
  mutate(
    electorate_bin = case_when(
      electorate == 0           ~ "0",
      electorate < 1000         ~ "1-999",
      electorate < 10000        ~ "1,000-9,999",
      electorate < 50000        ~ "10,000-49,999",
      electorate < 200000       ~ "50,000-199,999",
      electorate >= 200000      ~ "200,000+",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(electorate_bin = factor(
    electorate_bin,
    levels = c("0","1-999","1,000-9,999","10,000-49,999",
               "50,000-199,999","200,000+")
  ))

electorate_colors <- c(
  "0"              = "grey82",
  "1-999"          = "#e5f5e0",
  "1,000-9,999"    = "#a1d99b",
  "10,000-49,999"  = "#41ab5d",
  "50,000-199,999" = "#238b45",
  "200,000+"       = "#005a32"
)

fig_map_b <- ggplot(hex_russia) +
  geom_sf(aes(fill = electorate_bin), colour = NA) +
  scale_fill_manual(
    name   = "Registered voters per 1,000 sq. km (2024)",
    values = electorate_colors, drop = FALSE,
    guide  = guide_legend(
      title.position = "top", title.hjust = 0.5,
      direction = "horizontal", nrow = 1
    )
  ) +
  annotation_scale(location = "bl", width_hint = 0.22,
                   style = "ticks", text_cex = 0.65) +
  coord_sf() +
  map_theme()

# ── Assemble into one figure ─────────────────────────────────────────────────
fig_maps <- (add_panel_label(fig_map_a, "a") /
             add_panel_label(fig_map_b, "b")) +
  plot_layout(guides = "keep")

save_nature_figure(fig_maps, "fig_maps_density",
                   width  = NAT_WIDTH_DOUBLE,
                   height = NAT_WIDTH_DOUBLE * 1.10)   # slightly taller than square
cat("Maps figure saved\n")

# ── Save processed spatial data (UNCHANGED) ───────────────────────────────────
saveRDS(hex_russia, paste0(path_data, "hex_russia_2024.rds"))
cat("Processed hexagon data saved\n")

# ── Summary statistics (UNCHANGED) ───────────────────────────────────────────
summary_stats <- hex_russia %>%
  st_drop_geometry() %>%
  filter(n_stations > 0) %>%
  summarise(
    total_hexagons           = n(),
    total_stations           = sum(n_stations,   na.rm = TRUE),
    total_electorate         = sum(electorate,    na.rm = TRUE),
    mean_stations_per_hex    = mean(n_stations,   na.rm = TRUE),
    median_stations_per_hex  = median(n_stations, na.rm = TRUE),
    mean_electorate_per_hex  = mean(electorate,   na.rm = TRUE),
    median_electorate_per_hex = median(electorate, na.rm = TRUE)
  )

cat("\n=== Summary Statistics ===\n")
print(summary_stats)

# ── Session info ──────────────────────────────────────────────────────────────
cat("\n=== Session Information ===\n")
print(sessionInfo())
cat("\n=== Maps figure saved to:", path_output, "===\n")
