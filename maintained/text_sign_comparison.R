# graham_coppock_2021/maintained/text_sign_comparison.R
# Output: output/text_sign_comparison.csv
# Depends on: estimates_grand_comparison.R output, helpers.R
# Description: Counts how often each self-report format gets the sign of the effect
#   wrong relative to the experimental benchmark, for the ten treatments in Figure 4.
#   The article states 12 of 20 for the change format and 3 of 20 for the
#   counterfactual format; both counts are computed here rather than transcribed.

source(here::here("maintained", "helpers.R"))

grand_plot_df <- read_rds(here::here(out_dir, "estimates_grand_comparison.rds"))

# The ten Figure 4 treatments: the eight in Study 1 plus Obama Torture and Trump Coal
# in Study 2a, each split by party. Study 2a's other topics are the pretreated cases
# of Figure 5 and are not part of this comparison.
fig4_cells <-
  grand_plot_df |>
  filter(study == "1" | (study == "2a" & str_detect(topic, "Obama|Coal")),
         Party %in% c("Democrat", "Republican"))

comparison <-
  fig4_cells |>
  filter(
    (Estimator == "More-less" & category == "  Diff" & Format %in% c("Change", "Counterfactual")) |
      (Estimator == "CATE" & Format %in% c("Counterfactual", "Diff. in means"))
  ) |>
  mutate(
    quantity = case_when(
      Estimator == "More-less" & Format == "Change" ~ "more_less_change",
      Estimator == "More-less" & Format == "Counterfactual" ~ "more_less_counterfactual",
      Estimator == "CATE" & Format == "Counterfactual" ~ "cate_counterfactual",
      Estimator == "CATE" & Format == "Diff. in means" ~ "benchmark"
    )
  ) |>
  select(study, topic, Party, quantity, estimate) |>
  pivot_wider(names_from = quantity, values_from = estimate) |>
  mutate(
    change_wrong_sign = sign(more_less_change) != sign(benchmark),
    counterfactual_wrong_sign = sign(cate_counterfactual) != sign(benchmark)
  )

counts <- tibble(
  stat = c(
    "Cells compared (10 treatments x 2 parties)",
    "Change format: sign opposite the difference in means",
    "Counterfactual format: sign opposite the difference in means"
  ),
  value = c(
    nrow(comparison),
    sum(comparison$change_wrong_sign),
    sum(comparison$counterfactual_wrong_sign)
  )
)

write_csv(comparison, here::here(out_dir, "text_sign_comparison.csv"))
print(comparison, n = 25)
print(counts)
