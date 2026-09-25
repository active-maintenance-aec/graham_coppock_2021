# graham_coppock_2021/maintained/text_sign_comparison.R
# Output: output/text_sign_comparison.csv
# Depends on: estimates_grand_comparison.R output, helpers.R
# Description: Counts how often each self-report format gets the sign of the effect
#   wrong relative to the experimental benchmark, for the ten treatments in Figure 4.
#   The article states 12 of 20 for the change format and 3 of 20 for the
#   counterfactual format; both counts are computed here rather than transcribed.
#   A cell whose estimate is zero has no sign to be opposite, and is excluded rather
#   than counted: see the tolerance below.

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
  pivot_wider(names_from = quantity, values_from = estimate)

# An estimate of zero has no sign, so it cannot have the opposite sign of anything, and
# sign() is the wrong predicate for it. Two of the twenty counterfactual CATEs are zero:
# Trump coal ash among Republicans lands on exactly -0, and the Tax Cuts and Jobs Act
# among Republicans on a residue near 1e-17 whose sign bit differs between estimatr 1.0.6
# and 2.0. Comparing sign() directly counts the first always and the second only under
# 2.0, so the published count of 3 is reproducible only by accident of the solver: the
# same code gives 4 under estimatr 2.0.0.9000. The tolerance sits nine orders of magnitude
# above those residues and six below the smallest estimate that is genuinely nonzero
# (0.0028 in the change format, 0.0139 in the counterfactual), so no cell is near it.
zero_tol <- 1e-8

comparison <-
  comparison |>
  mutate(
    change_sign_determinate = abs(more_less_change) > zero_tol & abs(benchmark) > zero_tol,
    counterfactual_sign_determinate =
      abs(cate_counterfactual) > zero_tol & abs(benchmark) > zero_tol,
    change_wrong_sign =
      change_sign_determinate & sign(more_less_change) != sign(benchmark),
    counterfactual_wrong_sign =
      counterfactual_sign_determinate & sign(cate_counterfactual) != sign(benchmark)
  )

counts <- tibble(
  stat = c(
    "Cells compared (10 treatments x 2 parties)",
    "Change format: sign opposite the difference in means",
    "Counterfactual format: sign opposite the difference in means",
    "Change format: estimate zero, so no sign to compare",
    "Counterfactual format: estimate zero, so no sign to compare"
  ),
  value = c(
    nrow(comparison),
    sum(comparison$change_wrong_sign),
    sum(comparison$counterfactual_wrong_sign),
    sum(!comparison$change_sign_determinate),
    sum(!comparison$counterfactual_sign_determinate)
  )
)

write_csv(comparison, here::here(out_dir, "text_sign_comparison.csv"))
print(comparison, n = 25)
print(counts)
