# graham_coppock_2021/maintained/figure_e2_study2b_simultaneous.R
# Output: output/figure_e2_study2b_simultaneous.pdf, output/figure_e2_study2b_simultaneous.png,
#   output/figure_e2_study2b_simultaneous.csv
# Depends on: estimates_grand_comparison.R output, estimates_pretreated.R output, helpers.R
# Description: Reproduces Appendix Figure E.2, the Study 2b results, which set the
#   simultaneous outcomes format against the change and counterfactual formats. Panel
#   (a) covers the two treatments with no pretreatment problem and takes the
#   experimental benchmark; panel (b) covers the two where subjects may have been
#   pretreated and drops it.

source(here::here("maintained", "helpers.R"))

grand_plot_df <- read_rds(here::here(out_dir, "estimates_grand_comparison.rds"))
grand_plot_df_treat_only <- read_rds(here::here(out_dir, "estimates_pretreated.rds"))

pretreat_df <- grand_plot_df_treat_only |> filter(Format != "Diff. in means")

# Panel (a): no pretreatment ----
g1 <- make_grand_plot(
  "2b", "Obama|Coal",
  x = grand_plot_df,
  vjust_val = 1.35,
  outside_pos = 1.45
) +
  guides(fill = guide_legend(reverse = TRUE), size = "none", shape = "none", color = "none") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.margin = margin(0, 0, 0, 0))

# Panel (b): possible pretreatment ----
g2 <- make_grand_plot(
  "2b", "DREAM|Biden",
  x = pretreat_df,
  vjust_val = 1.4,
  outside_pos = 1.45,
  keep_axis_label = "Counterfactual"
) +
  guides(fill = guide_legend(reverse = TRUE), size = "none", shape = "none", color = "none") +
  theme(axis.text.x = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box.margin = margin(0, 0, 0, 0))

pdf(NULL)
g <- gridExtra::arrangeGrob(
  g1 + labs(title = "(a)  No pretreatment") + theme(plot.margin = margin(5, 0, 5, 0)),
  g2 + labs(title = "(b)  Possible pretreatment") + theme(plot.margin = margin(5, 0, 5, 0))
)
dev.off()

ggsave(here::here(out_dir, "figure_e2_study2b_simultaneous.pdf"), plot = g,
       width = 6, height = 5.15)
ggsave(here::here(out_dir, "figure_e2_study2b_simultaneous.png"), plot = g,
       width = 6, height = 5.15, dpi = 300)

fig_e2_values <-
  bind_rows(
    grand_plot_df |>
      filter(study == "2b", str_detect(topic, "Obama|Coal")) |>
      mutate(panel = "(a) No pretreatment"),
    pretreat_df |>
      filter(study == "2b", str_detect(topic, "DREAM|Biden")) |>
      mutate(panel = "(b) Possible pretreatment")
  ) |>
  select(panel, study, Topic, Party, Estimator, Format, category, share = value,
         estimate, std.error, conf.low, conf.high, label = lab_outside) |>
  arrange(panel, Topic, Party, Estimator, Format, category)

write_csv(fig_e2_values, here::here(out_dir, "figure_e2_study2b_simultaneous.csv"))
print(fig_e2_values |> filter(!is.na(label)), n = 40)
