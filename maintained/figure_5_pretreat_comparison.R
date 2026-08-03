# graham_coppock_2021/maintained/figure_5_pretreat_comparison.R
# Output: output/figure_5_pretreat_comparison.pdf, output/figure_5_pretreat_comparison.png,
#   output/figure_5_pretreat_comparison.csv
# Depends on: estimates_pretreated.R output, helpers.R
# Description: Reproduces Figure 5: the change format against the nonrandomized
#   counterfactual format for the pretreated Study 2a cases (Biden/Hill, DREAM Act,
#   Kavanaugh). There is no experimental benchmark for these treatments, so the
#   difference-in-means row is dropped and the counterfactual row carries the scale.

source(here::here("maintained", "helpers.R"))

grand_plot_df_treat_only <- read_rds(here::here(out_dir, "estimates_pretreated.rds"))

pretreat_df <- grand_plot_df_treat_only |> filter(Format != "Diff. in means")

# Top panel: Biden/Hill and the DREAM Act ----
g1 <- make_grand_plot(
  "2a", "DREAM|Biden",
  x = pretreat_df,
  vjust_val = 1.4,
  outside_pos = 1.45,
  rescale_box = 5,
  keep_axis_label = "Counterfactual"
) +
  theme(axis.text.x = element_blank(), plot.margin = margin(0, 97.5, 0, 0))

# Bottom panel: Kavanaugh ----
g2 <- make_grand_plot(
  "2a", "Kav",
  x = pretreat_df,
  vjust_val = 1.4,
  outside_pos = 1.45,
  rescale_box = 5,
  keep_axis_label = "Counterfactual"
) +
  guides(fill = guide_legend(reverse = TRUE), size = "none", shape = "none", color = "none") +
  theme(axis.text.x = element_blank(),
        legend.position = "right",
        legend.direction = "vertical")

pdf(NULL)
g <- gridExtra::arrangeGrob(
  g1 + theme(plot.margin = margin(5, 100, 5, 0)),
  g2 + theme(plot.margin = margin(5, 0, 5, 0))
)
dev.off()

ggsave(here::here(out_dir, "figure_5_pretreat_comparison.pdf"), plot = g,
       width = 6, height = 3.625)
ggsave(here::here(out_dir, "figure_5_pretreat_comparison.png"), plot = g,
       width = 6, height = 3.625, dpi = 300)

fig5_values <-
  pretreat_df |>
  filter(study == "2a", str_detect(topic, "DREAM|Biden|Kav")) |>
  select(study, Topic, Party, Estimator, Format, category, share = value,
         estimate, std.error, conf.low, conf.high, label = lab_outside) |>
  arrange(Topic, Party, Estimator, Format, category, .locale = "en")

write_csv(fig5_values, here::here(out_dir, "figure_5_pretreat_comparison.csv"))
print(fig5_values |> filter(!is.na(label)), n = 40)
