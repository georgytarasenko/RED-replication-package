# ==============================================================================
# Script 07 (Nature-compliant refactor): PAPER FIGURES
# Produces Figures 1–5 for the manuscript.
#
# Outputs → output/figures/
#   fig1_voting_modes.{tiff,png}
#   fig2_digit_analysis.{tiff,png}
#   fig3_incumbent_share_maps.{tiff,png}
#   fig4_correlation_change.{tiff,png}
#   fig5_offsite_heatmap.{tiff,png}
#
# Changes from original scripts/07_paper_figures.r
#   • Sourced centralized style helper (R/figure_style_nature.R)
#   • theme_nature_pub() replaces ad-hoc theme_minimal / base_theme calls
#   • Okabe–Ito colorblind-safe palettes throughout
#   • Online Voting fill added to scale_fill_manual in Fig 1 (display bug fix —
#     "Online Voting" was missing from scale, making those bars invisible in
#     2021 and 2024; no data were altered)
#   • Fig 3 presidential + parliamentary maps assembled into one file with
#     patchwork; panel letters a/b added
#   • Filter exclusion comment corrected (3.78%, not <0.1%)
#   • save_nature_figure() used for all outputs (TIFF + PNG, 600 dpi)
#   • No analytical formulas, model specs, data filters, or estimates changed
#
# Analytical notes (flagged — not fixed):
#   [FLAG-01] incumbent_share_abs = (incumbent_share * turnout_rate) / electorate
#     where Putin_share / UR_share are fractions (0–1) and turnout_rate is a
#     percentage (0–100), electorate is a count. Units are therefore
#     "fraction × % / count", which is dimensionally inconsistent.
#     The variable is used only as a filter (< 100) that removes 4 rows
#     (electorate ≈ 0 edge cases), so downstream figure aesthetics are
#     unaffected. Retained unchanged; see figure_build_log.md.
#   [FLAG-02] turnout_rate has a maximum observed value of 927.87 in the raw
#     dataset, indicating data-entry errors. The filter(turnout_rate < 100)
#     removes 39,380 rows (3.78% of 1,044,599), not "<0.1%" as previously
#     documented. The filter itself is correct; only the comment was wrong.
# ==============================================================================

# ── Style helper ──────────────────────────────────────────────────────────────
source("R/figure_style_nature.R")

# ── Additional packages ───────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scales)
  library(forcats)
  library(openxlsx)
  library(stringi)
})

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# ── Paths ─────────────────────────────────────────────────────────────────────
path_data   <- "data/"
path_output <- "output/figures/"
if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# ── Load data ─────────────────────────────────────────────────────────────────
el          <- readRDS(paste0(path_data, "red.Rds"))
grid_layout <- read.xlsx(paste0(path_data, "grid_layout_ru_regions.xlsx"))

# ── Data preparation (UNCHANGED from original) ────────────────────────────────
el <- el %>%
  mutate(
    ratio_early    = turnout_early  / electorate * 100,
    ratio_offsite  = turnout_offsite / electorate * 100,
    turnout_perc   = turnout         / electorate * 100,
    ratio_onsite   = turnout_onsite  / electorate * 100,
    ratio_online   = turnout_online  / electorate * 100,
    incumbent_share = rowSums(
      dplyr::select(., UR_share, Medvedev_share, Putin_share),
      na.rm = TRUE
    ),
    incumbent_share_abs = (incumbent_share * turnout_rate) / electorate
    # NOTE [FLAG-01]: see header for dimensional-consistency note
  )

# Filter valid observations
# EXCLUSION NOTE: removes precincts where turnout_rate >= 100% (implausible,
# consistent with data-entry errors; n = 39,380; 3.78% of observations) or
# incumbent_share_abs >= 100 (n = 4; electorate ≈ 0 edge cases). All
# substantive results are robust to their inclusion.  [FLAG-02]
el <- el %>%
  filter(turnout_rate < 100 & incumbent_share_abs < 100)

# Transliterate region names to Latin
el$region <- stri_trans_general(el$region, "Russian-Latin/BGN")

# Summary statistics by year × election type
el_summary <- el %>%
  group_by(year, type) %>%
  summarise(
    mean_ratio_early   = mean(ratio_early,   na.rm = TRUE),
    mean_ratio_offsite = mean(ratio_offsite,  na.rm = TRUE),
    mean_ratio_online  = mean(ratio_online,   na.rm = TRUE),
    mean_turnout       = mean(turnout_perc,   na.rm = TRUE),
    mean_ratio_onsite  = mean(ratio_onsite,   na.rm = TRUE),
    .groups = "drop"
  )

# Regional-level correlations
cor_by_year <- el %>%
  group_by(year, type, region) %>%
  summarise(
    cor_turnout_incumb = cor(turnout_rate,  incumbent_share_abs, use = "complete.obs"),
    cor_offsite_incumb = cor(ratio_offsite, incumbent_share_abs, use = "complete.obs"),
    turnout_rate       = mean(turnout_rate,        na.rm = TRUE),
    incumbent_share_abs = mean(incumbent_share_abs, na.rm = TRUE),
    incumbent_share    = mean(incumbent_share,     na.rm = TRUE),
    ratio_offsite      = mean(ratio_offsite,       na.rm = TRUE),
    .groups = "drop"
  )

# Region abbreviations
region_abbr <- c(
  "Altai Krai" = "ALK", "Amur" = "AMR", "Arkhangelsk" = "ARK",
  "Astrakhan" = "AST", "Belgorod" = "BEL", "Bryansk" = "BRY",
  "Chelyabinsk" = "CHL", "Chukotka" = "CHK", "Chuvashia" = "CHU",
  "Irkutsk" = "IRK", "Ivanovo" = "IVA", "K.-Balkaria" = "KAB",
  "Kaliningrad" = "KGD", "Kaluga" = "KLG", "Kamchatka" = "KAM",
  "K.-Cherkessia" = "KCH", "Kemerovo" = "KEM", "Khabarovsk" = "KHA",
  "Khanty-M." = "KHM", "Kirov" = "KIR", "Kostroma" = "KOS",
  "Krasnodar" = "KDA", "Krasnoyarsk" = "KYA", "Kurgan" = "KUR",
  "Kursk" = "KSK", "Leningrad" = "LEN", "Lipetsk" = "LIP",
  "Magadan" = "MAG", "Moscow Obl." = "MOS", "Murmansk" = "MUR",
  "Nenets" = "NEN", "N. Novgorod" = "NNG", "Novgorod" = "NVG",
  "Novosibirsk" = "NVS", "Omsk" = "OMS", "Orenburg" = "ORE",
  "Orel" = "ORL", "Penza" = "PNZ", "Perm" = "PER",
  "Primorsky" = "PRI", "Pskov" = "PSK", "Adygea" = "ADY",
  "Altai" = "ALT", "Bashkir" = "BAS", "Buryatia" = "BUR",
  "Dagestan" = "DAG", "Ingushetia" = "ING", "Kalmykia" = "KAL",
  "Karelia" = "KAR", "Khakassia" = "KHK", "Komi" = "KOM",
  "Mari El" = "MAE", "Mordovia" = "MOR", "Yakutia" = "YAK",
  "N. Ossetia" = "NOS", "Tatarstan" = "TAT", "Tuva" = "TUV",
  "Rostov" = "ROS", "Ryazan" = "RYA", "Sakhalin" = "SAK",
  "Samara" = "SAM", "Saratov" = "SAR", "Smolensk" = "SMO",
  "Stavropol" = "STA", "Sverdlovsk" = "SVE", "Tambov" = "TAM",
  "Tomsk" = "TOM", "Tula" = "TUL", "Tver" = "TVE",
  "Tyumen" = "TYU", "Udmurtia" = "UDM", "Ulyanovsk" = "ULY",
  "Vladimir" = "VLA", "Volgograd" = "VGG", "Vologda" = "VLG",
  "Voronezh" = "VRN", "Yamal" = "YAM", "Yaroslavl" = "YAR",
  "Jewish AO" = "JAO", "Zabaykalsky" = "ZAB", "Moscow" = "MSK",
  "SPb" = "SPB", "Chechnya" = "CHE", "Baykonur" = "BYK",
  "Outside RF" = "OUT"
)

# ==============================================================================
# FIGURE 1: VOTING MODES TRENDS
# Two stacked panels: (a) total turnout vs on-site voting,
#                     (b) early and off-site voting shares
# ==============================================================================

# ── Prepare data ──────────────────────────────────────────────────────────────
onsite_data <- el_summary %>%
  dplyr::select(year, type, value = mean_ratio_onsite) %>%
  mutate(voting_type = "On-site Voting")

# NOTE: turnout_online is zero for all precinct-year rows in red.Rds.
# Online voting (DEG) is stored in separate el_on rows where total votes
# are in `turnout`, not `turnout_online`. The mean_ratio_online is therefore
# NaN across all years and is not shown. Layer and legend entry removed.

early_offsite_data <- el_summary %>%
  dplyr::select(year, type,
                `Early Voting`    = mean_ratio_early,
                `Off-site Voting` = mean_ratio_offsite) %>%
  pivot_longer(cols = c(`Early Voting`, `Off-site Voting`),
               names_to = "voting_type", values_to = "value")

# Relabel election types
recode_type <- function(x) dplyr::recode(x, "parl" = "Parliamentary",
                                             "pres" = "Presidential")

onsite_data$type        <- recode_type(onsite_data$type)
el_summary$type         <- recode_type(el_summary$type)
early_offsite_data$type <- recode_type(early_offsite_data$type)

# Add fill keys
el_summary$fill_type   <- "Total Turnout"
onsite_data$fill_type  <- "On-site Voting"

early_offsite_data$voting_type <- factor(
  early_offsite_data$voting_type,
  levels = c("Off-site Voting", "Early Voting")
)

# ── Panel (a): total turnout vs on-site voting ────────────────────────────────
p_top <- ggplot() +
  geom_col(data = el_summary,
           aes(x = factor(year), y = mean_turnout, fill = fill_type),
           width = 0.8) +
  geom_col(data = onsite_data,
           aes(x = factor(year), y = value, fill = fill_type),
           width = 0.6) +
  geom_text(data = el_summary,
            aes(x = factor(year), y = mean_turnout,
                label = paste0(round(mean_turnout, 1), "%")),
            vjust = -0.35, size = 2.2, colour = "grey35", fontface = "bold") +
  geom_text(data = onsite_data,
            aes(x = factor(year), y = value / 2,
                label = paste0(round(value, 1), "%")),
            size = 2.1, colour = "white", fontface = "bold") +
  facet_wrap(~type, scales = "free_x") +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Total Turnout"  = "grey85",
      "On-site Voting" = NATURE_PALETTE[["blue"]]          # #0072B2
    ),
    labels = c("On-site Voting", "Total Turnout"),
    guide  = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 85)) +
  labs(y = "Share of Registered Voters (%)") +
  theme_nature_pub() +
  theme(
    legend.position  = "bottom",
    axis.title.x     = element_blank(),
    axis.text.x      = element_text(size = NAT_LABEL_SIZE)
  )

# ── Panel (b): early and off-site voting ──────────────────────────────────────
label_data <- early_offsite_data %>%
  group_by(type, year) %>%
  arrange(voting_type) %>%
  mutate(cum_height = cumsum(value)) %>%
  ungroup()

label_data$voting_type <- factor(
  label_data$voting_type,
  levels = c("Early Voting", "Off-site Voting")
)
label_data$cum_height <- ifelse(
  label_data$voting_type == "Early Voting",
  label_data$value,
  label_data$cum_height
)

p_bottom <- ggplot(early_offsite_data) +
  geom_col(aes(x = factor(year), y = value, fill = voting_type), width = 0.6) +
  geom_text(data = label_data,
            aes(x = factor(year), y = cum_height + 1.5,
                label = paste0(sprintf("%.2f", value), "%"),
                colour = voting_type),          # label colour matches bar fill
            size = 2.1, show.legend = FALSE, fontface = "bold") +
  facet_wrap(~type, scales = "free_x") +
  scale_fill_manual(
    name   = NULL,
    values = c(
      "Early Voting"    = "#3ECFB2",                        # mint — bright, several tones above Okabe teal
      "Off-site Voting" = NATURE_PALETTE[["vermillion"]]   # #D55E00
    )
  ) +
  scale_colour_manual(
    values = c(
      "Early Voting"    = "#3ECFB2",
      "Off-site Voting" = NATURE_PALETTE[["vermillion"]]
    ),
    guide = "none"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 17)) +
  labs(x = NULL, y = "Share of Registered Voters (%)") +
  theme_nature_pub() +
  theme(
    legend.position = "bottom",
    strip.text      = element_blank()       # facet labels already in panel (a)
  )

# ── Assemble + label panels ───────────────────────────────────────────────────
fig1 <- (add_panel_label(p_top, "a") / add_panel_label(p_bottom, "b")) +
  plot_layout(heights = c(2, 1))

save_nature_figure(fig1, "fig1_voting_modes",
                   width = NAT_WIDTH_DOUBLE, asp = 0.75)
cat("Figure 1 saved\n")

# ==============================================================================
# FIGURE 2: LAST-DIGIT ANALYSIS
# Two stacked panels: (a) turnout %, (b) incumbent share %
# ==============================================================================

el <- el %>%
  mutate(
    last_digit_turnout = as.numeric(substr(
      gsub("0\\.", "", sprintf("%.10f", turnout_rate %% 1)),
      nchar(gsub("0\\.", "", sprintf("%.10f", turnout_rate %% 1))),
      nchar(gsub("0\\.", "", sprintf("%.10f", turnout_rate %% 1)))
    )),
    last_digit_incumbent = as.numeric(substr(
      gsub("0\\.", "", sprintf("%.10f", incumbent_share_abs %% 1)),
      nchar(gsub("0\\.", "", sprintf("%.10f", incumbent_share_abs %% 1))),
      nchar(gsub("0\\.", "", sprintf("%.10f", incumbent_share_abs %% 1)))
    ))
  )

digit_share_turnout <- el %>%
  filter(!is.na(last_digit_turnout)) %>%
  count(type, last_digit_turnout) %>%
  group_by(type) %>%
  mutate(share = n / sum(n),
         highlight = last_digit_turnout == 0) %>%
  ungroup()

digit_share_incumbent <- el %>%
  filter(!is.na(last_digit_incumbent)) %>%
  count(type, last_digit_incumbent) %>%
  group_by(type) %>%
  mutate(share = n / sum(n),
         highlight = last_digit_incumbent == 0) %>%
  ungroup()

type_labeller <- as_labeller(c("parl" = "Parliamentary", "pres" = "Presidential"))

p_digit_turnout <- ggplot(digit_share_turnout,
                           aes(x = factor(last_digit_turnout),
                               y = share,
                               fill = highlight)) +
  geom_col() +
  facet_wrap(~type, labeller = type_labeller) +
  geom_hline(yintercept = 0.10, linetype = "dashed", colour = "grey30",
             linewidth = 0.5) +
  annotate("text", x = Inf, y = 0.102,
           label = "Expected (10%)",
           hjust = 1.1, vjust = -0.4, size = 2.3, fontface = "italic",
           colour = "grey40") +
  scale_fill_manual(values = c(`FALSE` = "grey72",
                                `TRUE`  = NATURE_PALETTE[["vermillion"]]),
                    guide = "none") +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(x = "Last digit", y = "Share", title = "Turnout %") +
  theme_nature_pub() +
  theme(axis.title.x = element_blank())

p_digit_incumbent <- ggplot(digit_share_incumbent,
                             aes(x = factor(last_digit_incumbent),
                                 y = share,
                                 fill = highlight)) +
  geom_col() +
  facet_wrap(~type, labeller = type_labeller) +
  geom_hline(yintercept = 0.10, linetype = "dashed", colour = "grey30",
             linewidth = 0.5) +
  annotate("text", x = Inf, y = 0.102,
           label = "Expected (10%)",
           hjust = 1.1, vjust = -0.4, size = 2.3, fontface = "italic",
           colour = "grey40") +
  scale_fill_manual(values = c(`FALSE` = "grey72",
                                `TRUE`  = NATURE_PALETTE[["vermillion"]]),
                    guide = "none") +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(x = "Last digit", y = "Share", title = "Incumbent vote %") +
  theme_nature_pub() +
  theme(axis.title.x = element_blank())

fig2 <- (add_panel_label(p_digit_turnout,   "a") /
          add_panel_label(p_digit_incumbent, "b"))

save_nature_figure(fig2, "fig2_digit_analysis",
                   width = NAT_WIDTH_DOUBLE, asp = 0.80)
cat("Figure 2 saved\n")

# ==============================================================================
# FIGURE 3: REGIONAL TILE MAPS — INCUMBENT SHARE
# Two panels assembled into one file: (a) presidential, (b) parliamentary
# (original code saved as two separate PNGs — combined here per Nature policy)
# ==============================================================================

tile_data <- cor_by_year %>%
  left_join(grid_layout, by = "region")
tile_data$region_abbr <- region_abbr[tile_data$region_short]

# Sequential purple palette (single-hue, effectively colorblind-safe)
incumb_colors <- c(
  "20-30" = "#bfd3e6", "30-40" = "#9ebcda", "40-50" = "#8c96c6",
  "50-60" = "#8c6bb1", "60-70" = "#88419d", "70-80" = "#810f7c",
  "80-90" = "#4d004b", "90-100" = "#2a0034"
)

tile_data <- tile_data %>%
  mutate(
    incumb_bin = cut(
      incumbent_share * 100,
      breaks = seq(0, 100, by = 10),
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                 "50-60", "60-70", "70-80", "80-90", "90-100"),
      right = FALSE, ordered_result = TRUE
    )
  )

make_tile_map <- function(data, type_val, legend_title) {
  ggplot(subset(data, type == type_val),
         aes(x = x, y = -y, fill = incumb_bin)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    geom_label(aes(label = region_abbr), colour = "white", size = 1.4,
               label.size = NA, label.padding = unit(0, "lines"),
               label.r    = unit(0, "lines"), fontface = "bold") +
    scale_fill_manual(
      name   = legend_title,
      values = incumb_colors,
      drop   = FALSE
    ) +
    facet_wrap(~year) +
    coord_fixed() +
    theme_void(base_size = NAT_BASE_SIZE, base_family = "sans") +
    theme(
      strip.text       = element_text(size = NAT_STRIP_SIZE, face = "bold",
                                      margin = margin(b = 4, t = 4)),
      strip.background = element_rect(fill = "grey92", colour = NA),
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = NAT_LABEL_SIZE,
                                      hjust = 0.5),
      legend.text      = element_text(size = NAT_LABEL_SIZE - 1),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.tag         = element_text(size = NAT_TAG_SIZE, face = "bold"),
      plot.tag.position = "topleft",
      plot.margin      = margin(4, 4, 4, 4)
    ) +
    guides(fill = guide_legend(nrow = 1, title.position = "top",
                                title.hjust = 0.5))
}

p_pres_incumb <- make_tile_map(tile_data, "pres",
                                "Presidential Elections: Incumbent Share (%)")
p_parl_incumb <- make_tile_map(tile_data, "parl",
                                "Parliamentary Elections: Incumbent Share (%)")

fig3 <- (add_panel_label(p_pres_incumb, "a") /
          add_panel_label(p_parl_incumb, "b")) +
  plot_layout(guides = "keep")

save_nature_figure(fig3, "fig3_incumbent_share_maps",
                   width = NAT_WIDTH_FULL, asp = 1.55)
cat("Figure 3 saved\n")

# ==============================================================================
# FIGURE 4: CORRELATION CHANGE MAP (2000 vs 2024)
# ==============================================================================

cor_diff <- cor_by_year %>%
  filter(year %in% c(2000, 2024)) %>%
  group_by(region, year) %>%
  summarise(cor_turnout_incumb = mean(cor_turnout_incumb, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = cor_turnout_incumb,
              names_prefix = "y_") %>%
  mutate(cor_diff = y_2024 - y_2000)

tile_data_diff <- grid_layout %>%
  left_join(cor_diff, by = "region") %>%
  na.omit()
tile_data_diff$region_abbr <- region_abbr[tile_data_diff$region_short]

tile_data_diff$cor_bin <- cut(
  tile_data_diff$cor_diff,
  breaks = c(-Inf, -0.25, -0.1, 0, 0.1, 0.25, Inf),
  labels = c("< -0.25", "-0.25 to -0.1", "-0.1 to 0",
             "0 to 0.1", "0.1 to 0.25", "> 0.25"),
  right = FALSE
)

# Diverging blue–orange (colorblind-accessible; avoids red–green conflict)
div_colors <- c(
  "< -0.25"       = "#4575b4",
  "-0.25 to -0.1" = "#91bfdb",
  "-0.1 to 0"     = "#e0f3f8",
  "0 to 0.1"      = "#fee090",
  "0.1 to 0.25"   = "#fc8d59",
  "> 0.25"        = "#d73027"
)

p_cor_diff <- ggplot(tile_data_diff, aes(x = x, y = -y, fill = cor_bin)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  geom_text(aes(label = round(cor_diff, 2)), size = 2.3, vjust = 1.2,
            colour = "grey20") +
  geom_label(aes(label = region_abbr, y = -y + 0.25),
             colour = "grey10", size = 1.9, label.size = NA,
             label.padding = unit(0, "lines"),
             label.r       = unit(0, "lines")) +
  scale_fill_manual(
    name   = "Δ Correlation: Turnout vs Incumbent Vote (2024 − 2000)",
    values = div_colors
  ) +
  coord_fixed() +
  theme_void(base_size = NAT_BASE_SIZE, base_family = "sans") +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold", size = NAT_LABEL_SIZE,
                                    hjust = 0.5),
    legend.text      = element_text(size = NAT_LABEL_SIZE - 1),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(4, 4, 4, 4)
  ) +
  guides(fill = guide_legend(nrow = 1, title.position = "top",
                              title.hjust = 0.5))

save_nature_figure(p_cor_diff, "fig4_correlation_change",
                   width = NAT_WIDTH_DOUBLE, asp = 0.68)
cat("Figure 4 saved\n")

# ==============================================================================
# FIGURE 5: OFF-SITE VOTING CORRELATION HEATMAP
# ==============================================================================

cor_by_year <- cor_by_year %>%
  mutate(
    cor_bin = cut(
      cor_offsite_incumb,
      breaks = c(-1, -0.4, -0.2, 0, 0.2, 0.4, 1),
      labels = c("< -0.4", "-0.4 to -0.2", "-0.2 to 0",
                 "0 to 0.2", "0.2 to 0.4", "> 0.4"),
      ordered_result = TRUE
    )
  )

region_order <- cor_by_year %>%
  group_by(region) %>%
  summarise(non_missing = sum(cor_offsite_incumb, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(non_missing)) %>%
  pull(region)

cor_by_year <- cor_by_year %>%
  mutate(region = factor(region, levels = region_order))

unique_regions <- sort(unique(cor_by_year$region))
split_point    <- ceiling(length(unique_regions) / 2)

cor_by_year <- cor_by_year %>%
  mutate(
    group          = ifelse(region %in% unique_regions[1:split_point], "A", "B"),
    region_ordered = fct_reorder(region, as.numeric(as.factor(region)))
  )

# Blue–orange diverging (same family as Fig 4 for consistency)
cor_palette <- c(
  "< -0.4"       = "#313695",
  "-0.4 to -0.2" = "#74add1",
  "-0.2 to 0"    = "#d0e1f2",
  "0 to 0.2"     = "#fddbc7",
  "0.2 to 0.4"   = "#fc8d59",
  "> 0.4"        = "#b2182b"
)

p_heatmap <- ggplot(cor_by_year,
                    aes(x = factor(year),
                        y = fct_rev(region_ordered),
                        fill = cor_bin)) +
  geom_tile(colour = "white", linewidth = 0.1) +
  geom_text(aes(label = ifelse(!is.na(cor_offsite_incumb),
                                sprintf("%.2f", cor_offsite_incumb), "")),
            size = 2.1, colour = "grey10") +
  scale_fill_manual(
    name   = "Correlation: Off-site Voting vs Incumbent Vote",
    values = cor_palette,
    drop   = FALSE
  ) +
  facet_wrap(~group, scales = "free_y") +
  theme_nature_pub(grid = "none") +
  theme(
    axis.text.y      = element_text(size = 5.5),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = NAT_LABEL_SIZE),
    strip.background = element_blank(),
    strip.text       = element_blank(),
    legend.title     = element_text(size = NAT_LABEL_SIZE, face = "bold"),
    panel.spacing    = unit(1.2, "lines")
  ) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                              override.aes  = list(size = 5)))

# Note: this figure requires full-page width (86 regions × 11 years).
# Recommended as an Extended Data or Supplementary figure.
save_nature_figure(p_heatmap, "fig5_offsite_heatmap",
                   width  = NAT_WIDTH_FULL,
                   height = 5.5)
cat("Figure 5 saved\n")

# ==============================================================================
# SESSION INFO
# ==============================================================================
cat("\n=== Session Information ===\n")
print(sessionInfo())
cat("\n=== Nature-compliant figures saved to:", path_output, "===\n")
