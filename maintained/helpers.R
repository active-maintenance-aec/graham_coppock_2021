# graham_coppock_2021/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, paths, colours, theme and number formatting shared by every
#   script in the rewrite. Each script sources this file first.

library(here)
library(tidyverse)
library(estimatr)
library(gridExtra)
library(ggh4x)
library(rsample)

here::i_am("maintained/helpers.R")

data_dir <- here::here("original")
out_dir <- here::here("maintained", "output")

# Colours ----
# Blue / purple / red partisan ramp, taken from the archive's functions.R.
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")
bpr_colors_noI <- c("#1F3A93", "#D91E18")

# Theme ----
theme_poself <- function() {
  theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      plot.title = element_text(size = 10, hjust = .5, margin = margin(0, 0, 10, 0)),
      strip.text = element_text(size = 8, vjust = 0),
      strip.background = element_blank(),
      strip.switch.pad.grid = unit(0, "cm"),
      strip.placement = "outside"
    )
}

# Formatting ----
format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

# The grand comparison panel ----
# Figures 2, 4, 5 and E.2 are the same panel drawn over different subsets, which is
# how the archive built them too, from a single plotting function. The panel puts a
# stacked bar of self-reported change categories on the [0, 1] range and overlays the
# CATE estimates, which are on each study's own outcome scale and so are mapped onto
# the same range; lab_lo and lab_hi print the endpoints of that scale so the reader
# can recover the original units. rescale_box widens or narrows the CATE box relative
# to the bars, and keep_axis_label picks the one row per facet that prints endpoints.
#
# The archive drew this with coord_flip() and its own facet_nested(), which calls
# ggplot2 internals that no longer exist. The layout here is native horizontal
# (Format on y, estimate on x) and the faceting is ggh4x::facet_nested().
make_grand_plot <- function(
    the_study = 1,
    topic_string = "",
    x,
    vjust_val = 1,
    point_size = 1,
    outside_pos = 1.6,
    outside_lab_size = 2.3,
    strip_text_size = 8,
    rescale_box = 2,
    keep_axis_label = "Diff. in means"
) {
  x <- x |>
    mutate(
      study_scale = case_when(study == "1" ~ 6, study == "2a" ~ 5, study == "2b" ~ 1),
      lab_lo = if_else(Estimator == "More-less", NA_real_, -study_scale / rescale_box),
      lab_hi = if_else(Estimator == "More-less", NA_real_, study_scale / rescale_box),
      estimate = if_else(Estimator == "More-less", estimate,
                         rescale_box * .5 * (estimate / study_scale) + .5),
      conf.low = if_else(Estimator == "More-less", conf.low,
                         rescale_box * .5 * (conf.low / study_scale) + .5),
      conf.high = if_else(Estimator == "More-less", conf.high,
                          rescale_box * .5 * (conf.high / study_scale) + .5),
      lab_lo = if_else(Estimator == "CATE" & format == keep_axis_label, lab_lo, NA_real_),
      lab_hi = if_else(Estimator == "CATE" & format == keep_axis_label, lab_hi, NA_real_)
    )

  filtered <- x |>
    filter(
      category != "Diff" | is.na(category),
      str_detect(topic, topic_string) | str_detect(as.character(Topic), topic_string),
      str_detect(study, as.character(the_study)),
      Party != ""
    )

  cate_sub <- filtered |> filter(Estimator == "CATE")

  filtered |>
    ggplot(aes(y = Format, x = estimate, xmin = conf.low, xmax = conf.high,
               fill = category, color = Estimator)) +
    geom_vline(aes(xintercept = if_else(Estimator == "CATE", 0.5, NA_real_)),
               linewidth = .4, lty = 2, color = "gray65") +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf),
              fill = "transparent", linewidth = .3, color = "transparent") +
    scale_color_manual(values = c("transparent", "gray5")) +
    geom_bar(aes(x = value), stat = "identity", alpha = .6, orientation = "y") +
    geom_linerange(data = cate_sub, aes(xmin = conf.low, xmax = conf.high), linewidth = .3) +
    geom_point(data = cate_sub, fill = "white", size = point_size, color = "gray5") +
    geom_text(aes(x = .02, label = lab_lo), size = 2.3, hjust = 0, vjust = vjust_val) +
    geom_text(aes(x = .98, label = lab_hi), size = 2.3, hjust = 1, vjust = vjust_val) +
    geom_text(aes(x = outside_pos, label = lab_outside), size = outside_lab_size,
              hjust = 1, color = "gray5") +
    facet_nested(Topic ~ Party + Estimator, switch = "x", scales = "free_x", space = "free_x") +
    scale_x_continuous(expand = c(.02, .02), limits = c(0, outside_pos)) +
    theme_minimal() +
    theme(
      text = element_text(color = "gray5"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(size = strip_text_size, margin = margin(0, 0, 1, 0)),
      strip.text.x = element_text(size = strip_text_size - 1, margin = margin(0, 0, 3, 0)),
      legend.position = "none",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(.2, "cm"),
      plot.title = element_text(size = 9, hjust = .5),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.ticks = element_line(color = "transparent"),
      panel.spacing.x = unit(.1, "cm"),
      panel.spacing.y = unit(0, "cm"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(
      values = c("transparent", "forestgreen", "gray80", "firebrick4"),
      labels = c("", "More (1)", "Same (0)", "Less (-1)")
    ) +
    labs(y = "Self-reported change (percentage points)", x = "Format",
         fill = "Self-reported change")
}
