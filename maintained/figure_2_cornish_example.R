# graham_coppock_2021/maintained/figure_2_cornish_example.R
# Output: output/figure_2_cornish_example.pdf, output/figure_2_cornish_example.png,
#   output/figure_2_cornish_example.csv
# Depends on: estimates_grand_comparison.R output, helpers.R
# Description: Reproduces Figure 2, the disputed accusation (Cornish) example from
#   Study 1, comparing the change, change-plus-level and counterfactual formats
#   against the experimental difference in means, separately by party.

source(here::here("maintained", "helpers.R"))

grand_plot_df <- read_rds(here::here(out_dir, "estimates_grand_comparison.rds"))

# One topic from Study 1, laid out by estimator and party rather than by topic. The
# shared panel suppresses the legend; this figure restores it on the right.
g <- make_grand_plot(
  1, "Disputed",
  x = grand_plot_df,
  vjust_val = 1.8,
  point_size = 1.5,
  outside_pos = 1.55,
  outside_lab_size = 2.8,
  strip_text_size = 9
) +
  facet_grid(Estimator ~ Party, scales = "free_x", switch = "y", space = "free") +
  theme(legend.position = "right",
        strip.text.y = element_text(size = 8, margin = margin(0, 3, 0, 0)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        panel.spacing.x = unit(0, "cm"),
        panel.spacing.y = unit(.1, "cm")) +
  guides(fill = guide_legend(reverse = TRUE, title = "Self-reported\nchange"),
         size = "none", shape = "none", color = "none")

ggsave(here::here(out_dir, "figure_2_cornish_example.pdf"), plot = g, width = 6, height = 1.6)
ggsave(here::here(out_dir, "figure_2_cornish_example.png"), plot = g, width = 6, height = 1.6, dpi = 300)

# The labels printed beside each row are the figure's published content, and the bar
# lengths are the category shares. Both are written out unrescaled so the figure can
# be checked against numbers without opening the PDF.
fig2_values <-
  grand_plot_df |>
  filter(str_detect(topic, "Disputed"), str_detect(study, "1")) |>
  select(study, Topic, Party, Estimator, Format, category, share = value,
         estimate, std.error, conf.low, conf.high, label = lab_outside) |>
  arrange(Party, Estimator, Format, category, .locale = "en")

write_csv(fig2_values, here::here(out_dir, "figure_2_cornish_example.csv"))
print(fig2_values |> filter(!is.na(label)), n = 20)
