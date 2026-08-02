# graham_coppock_2021/maintained/figure_4_grand_comparison.R
# Output: output/figure_4_grand_comparison.pdf, output/figure_4_grand_comparison.png,
#   output/figure_4_grand_comparison.csv
# Depends on: estimates_grand_comparison.R output, helpers.R
# Description: Reproduces Figure 4, the full comparison of the change format, the
#   counterfactual format and the experimental benchmark across the ten treatments in
#   Studies 1 and 2a that subjects had not seen before answering.

source(here::here("maintained", "helpers.R"))

grand_plot_df <- read_rds(here::here(out_dir, "estimates_grand_comparison.rds"))

# Three stacked panels ----
g1 <- make_grand_plot(1, "Blocked|Death|Disputed|Endorsed", x = grand_plot_df)
g2 <- make_grand_plot(1, "Immi|Supports|Tax|Undisputed", x = grand_plot_df)
g3 <- make_grand_plot("2a", "Obama|Coal", x = grand_plot_df) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1), size = "none",
         shape = "none", color = "none") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.margin = margin(0, -25, 0, 10))

# Assembling the grob needs a graphics device for its text metrics, and under Rscript
# that means the default device writes an Rplots.pdf. pdf(NULL) gives it a null device.
pdf(NULL)
g <- gridExtra::arrangeGrob(
  g1 + theme(plot.margin = margin(5, 0, 5, 0)),
  g2 + theme(plot.margin = margin(5, 0, 5, 0)),
  g3 + theme(plot.margin = margin(5, 0, 5, 0)),
  heights = c(1, 1, .95)
)
dev.off()

ggsave(here::here(out_dir, "figure_4_grand_comparison.pdf"), plot = g, width = 7, height = 6.25)
ggsave(here::here(out_dir, "figure_4_grand_comparison.png"), plot = g, width = 7, height = 6.25, dpi = 300)

# The ten treatments the figure draws, unrescaled: the bar shares, the point estimates
# and the labels printed to the right of each facet.
fig4_values <-
  grand_plot_df |>
  filter(study == "1" | (study == "2a" & str_detect(topic, "Obama|Coal")),
         Party %in% c("Democrat", "Republican")) |>
  select(study, Topic, Party, Estimator, Format, category, share = value,
         estimate, std.error, conf.low, conf.high, label = lab_outside) |>
  arrange(study, Topic, Party, Estimator, Format, category)

write_csv(fig4_values, here::here(out_dir, "figure_4_grand_comparison.csv"))
print(fig4_values |> filter(!is.na(label)), n = 20)
