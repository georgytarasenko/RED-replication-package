# ==============================================================================
# R/figure_style_nature.R
# Centralized publication style for Nature Portfolio / Scientific Data
#
# SOURCE this file at the top of every figure-generation script:
#   source("R/figure_style_nature.R")
#
# Exports
#   theme_nature_pub()       — ggplot2 theme
#   scale_color_nature()     — discrete colour scale (Okabe–Ito)
#   scale_fill_nature()      — discrete fill scale (Okabe–Ito)
#   save_nature_figure()     — writes TIFF + PNG at 600 dpi
#   add_panel_label()        — attaches bold lower-case tag to a ggplot
#   NAT_WIDTH_*              — column-width constants (inches)
#   NATURE_PALETTE           — named colour vector
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

# ── Okabe–Ito 8-colour palette (fully colorblind-safe) ────────────────────────
NATURE_PALETTE <- c(
  orange       = "#E69F00",
  sky_blue     = "#56B4E9",
  green        = "#009E73",
  yellow       = "#F0E442",
  blue         = "#0072B2",
  vermillion   = "#D55E00",
  pink         = "#CC79A7",
  black        = "#000000"
)

# ── Sequential single-hue palette (blue) ─────────────────────────────────────
NAT_SEQ_BLUE  <- c("#deebf7","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")

# ── Sequential single-hue palette (green) ─────────────────────────────────────
NAT_SEQ_GREEN <- c("#e5f5e0","#c7e9c0","#a1d99b","#74c476","#31a354","#006d2c")

# ── Diverging palette (purple–white–orange, accessible) ───────────────────────
NAT_DIV_PuOr  <- c("#7b3294","#c2a5cf","#f7f7f7","#a6dba0","#008837")

# ── Typography constants ──────────────────────────────────────────────────────
NAT_BASE_SIZE    <-  9   # pt — all other sizes are relative to this
NAT_TITLE_SIZE   <- 10
NAT_STRIP_SIZE   <-  9
NAT_LABEL_SIZE   <-  8
NAT_CAPTION_SIZE <-  7
NAT_TAG_SIZE     <- 10   # panel letter (a, b, c …)

# ── Layout width constants (inches; Nature column widths) ─────────────────────
NAT_WIDTH_SINGLE <- 3.54   #  89 mm — one column
NAT_WIDTH_1H     <- 5.51   # 140 mm — 1.5 columns
NAT_WIDTH_DOUBLE <- 7.20   # 183 mm — two columns (most common)
NAT_WIDTH_FULL   <- 8.86   # 225 mm — full page width

# ── Core publication theme ────────────────────────────────────────────────────
#' @param base_size   Base font size in pt (default 9)
#' @param base_family Font family string (default "sans" → Helvetica on most systems)
#' @param grid        Which major grid lines to show: "x", "y", "xy", or "none"
theme_nature_pub <- function(base_size   = NAT_BASE_SIZE,
                              base_family = "sans",
                              grid        = "y") {
  t <- theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # ── backgrounds
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      strip.background  = element_rect(fill = "grey93", colour = NA),

      # ── grid lines
      panel.grid.major.x = if (grid %in% c("x", "xy"))
                             element_line(colour = "grey88", linewidth = 0.25)
                           else element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "xy"))
                             element_line(colour = "grey88", linewidth = 0.25)
                           else element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(colour = "grey65", linewidth = 0.35,
                                        fill = NA),

      # ── text hierarchy
      plot.title    = element_text(size = NAT_TITLE_SIZE, face = "bold",
                                   hjust = 0, colour = "grey10",
                                   margin = margin(b = 3)),
      plot.subtitle = element_text(size = NAT_LABEL_SIZE, colour = "grey40",
                                   hjust = 0, margin = margin(b = 3)),
      plot.caption  = element_text(size = NAT_CAPTION_SIZE, colour = "grey50",
                                   hjust = 0, margin = margin(t = 4)),
      axis.title    = element_text(size = NAT_LABEL_SIZE, colour = "grey10"),
      axis.title.x  = element_text(margin = margin(t = 4)),
      axis.title.y  = element_text(margin = margin(r = 4)),
      axis.text     = element_text(size = NAT_LABEL_SIZE, colour = "grey20"),
      axis.ticks    = element_line(colour = "grey65", linewidth = 0.3),
      strip.text    = element_text(size = NAT_STRIP_SIZE, face = "bold",
                                   colour = "grey10",
                                   margin = margin(t = 3, b = 3)),

      # ── panel labels (a, b, c …)
      plot.tag      = element_text(size = NAT_TAG_SIZE, face = "bold",
                                   colour = "black"),
      plot.tag.position = "topleft",

      # ── legend
      legend.position    = "top",
      legend.key.size    = unit(0.32, "cm"),
      legend.text        = element_text(size = NAT_LABEL_SIZE),
      legend.title       = element_text(size = NAT_LABEL_SIZE, face = "bold"),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.margin      = margin(0, 0, 2, 0),
      legend.box.spacing = unit(2, "pt"),
      legend.spacing.x   = unit(4, "pt"),

      # ── facet spacing & outer margins
      panel.spacing = unit(0.55, "lines"),
      plot.margin   = margin(5, 5, 5, 5, "pt")
    )
  t
}

# ── Discrete colour scales using Okabe–Ito ────────────────────────────────────
scale_color_nature <- function(...) {
  scale_color_manual(values = unname(NATURE_PALETTE), ...)
}
scale_fill_nature <- function(...) {
  scale_fill_manual(values = unname(NATURE_PALETTE), ...)
}

# ── Panel label helper ────────────────────────────────────────────────────────
#' Add a bold lower-case panel letter (a, b, c …) to a ggplot
#' @param p     A ggplot object
#' @param label Character, e.g. "a"
add_panel_label <- function(p, label) {
  p + labs(tag = label) +
    theme(plot.tag = element_text(size = NAT_TAG_SIZE, face = "bold",
                                  colour = "black"))
}

# ── Save helper ───────────────────────────────────────────────────────────────
#' Save a figure as TIFF (LZW-compressed) and PNG at publication resolution.
#'
#' @param plot          A ggplot or patchwork object
#' @param filename_stem Stem without extension, e.g. "fig1_voting_modes"
#' @param path          Output directory (created if absent)
#' @param width         Width in inches (default NAT_WIDTH_DOUBLE = 7.20)
#' @param height        Height in inches; if NULL computed from asp
#' @param asp           Aspect ratio height/width (default 0.618 ≈ golden ratio)
#' @param dpi           Resolution for raster exports (default 600)
#' @param formats       Character vector of formats: "tiff", "png"
save_nature_figure <- function(plot,
                                filename_stem,
                                path    = "outputs/figures_nature",
                                width   = NAT_WIDTH_DOUBLE,
                                height  = NULL,
                                asp     = 0.618,
                                dpi     = 600,
                                formats = c("tiff", "png")) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  if (is.null(height)) height <- width * asp

  for (fmt in formats) {
    outfile <- file.path(path, paste0(filename_stem, ".", fmt))
    if (fmt == "tiff") {
      ggsave(outfile, plot = plot, width = width, height = height,
             dpi = dpi, bg = "white", device = "tiff", compression = "lzw")
    } else {
      ggsave(outfile, plot = plot, width = width, height = height,
             dpi = dpi, bg = "white")
    }
    sz <- round(file.info(outfile)$size / 1024)
    message(sprintf("  Saved %-55s [%5d KB]", outfile, sz))
  }
  invisible(plot)
}
