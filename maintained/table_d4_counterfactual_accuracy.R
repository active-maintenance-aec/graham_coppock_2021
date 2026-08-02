# graham_coppock_2021/maintained/table_d4_counterfactual_accuracy.R
# Output: output/table_d4_counterfactual_accuracy.csv,
#   output/table_d4_counterfactual_accuracy_estimates.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Reproduces Appendix Table D.4, the accuracy of counterfactual guesses:
#   actual against guessed potential outcomes, with a difference in means test for each
#   topic by party cell. The first output is the table as the appendix lays it out, one
#   formatted cell per entry; the second is the same numbers unrounded. The deposited
#   script formats these cells to three decimals and the published table prints two, so
#   parsing the formatted cell and re-rounding it would round twice.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
state_both_no_inds <- clean$state_both_no_inds

# Difference-in-means tests for Y0_tilde and Y1_tilde by Z_label
tab_dim <-
  state_both_no_inds |>
  pivot_longer(cols = c(Y0_tilde, Y1_tilde), names_to = "variable", values_to = "value") |>
  group_by(study, topic, variable, pid_3) |>
  reframe(tidy(lm_robust(value ~ Z_label, data = pick(everything()))))

# Means by Z_label arm
tab_means <-
  state_both_no_inds |>
  filter(!is.na(Z_label)) |>
  pivot_longer(cols = c(Y0_tilde, Y1_tilde), names_to = "variable", values_to = "value") |>
  group_by(study, topic, variable, pid_3, Z_label) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything()))))

tab_n <-
  state_both_no_inds |>
  filter(!is.na(Z_label)) |>
  group_by(study, topic, pid_3) |>
  summarize(N = sum(!is.na(Y)), .groups = "drop")

tab_assump1 <-
  tab_means |>
  mutate(
    Estimate = paste0(format_num(estimate), " (", format_num(std.error), ")"),
    variable = str_remove(variable, "_tilde"),
    Which = case_when(
      Z_label == "Control"   & variable == "Y0" ~ "Actual",
      Z_label == "Control"   & variable == "Y1" ~ "Guess",
      Z_label == "Treatment" & variable == "Y1" ~ "Actual",
      Z_label == "Treatment" & variable == "Y0" ~ "Guess"
    )
  ) |>
  select(study, topic, variable, Which, pid_3, Estimate) |>
  pivot_wider(names_from = Which, values_from = Estimate) |>
  left_join(
    tab_dim |>
      filter(term != "(Intercept)") |>
      mutate(
        variable = str_remove(variable, "_.+"),
        Difference = paste0(format_num(estimate), " (", format_num(std.error), ")"),
        p          = format_num(p.value, 3)
      ) |>
      select(study, topic, variable, pid_3, Difference, p),
    by = c("study", "topic", "variable", "pid_3")
  ) |>
  left_join(tab_n, by = c("study", "topic", "pid_3")) |>
  rename(Study = study, Topic = topic, Outcome = variable, Party = pid_3) |>
  select(Study, Topic, Outcome, Party, N, everything())

write_csv(tab_assump1, here::here(out_dir, "table_d4_counterfactual_accuracy.csv"))

# The same cells unrounded ----
tab_estimates <-
  bind_rows(
    tab_means |>
      mutate(
        outcome_var = str_remove(variable, "_tilde"),
        quantity = case_when(
          Z_label == "Control"   & outcome_var == "Y0" ~ "Actual",
          Z_label == "Control"   & outcome_var == "Y1" ~ "Guess",
          Z_label == "Treatment" & outcome_var == "Y1" ~ "Actual",
          Z_label == "Treatment" & outcome_var == "Y0" ~ "Guess"
        )
      ) |>
      transmute(study, topic, outcome = outcome_var, pid_3, quantity, estimate, std.error,
                p.value = NA_real_),
    tab_dim |>
      filter(term != "(Intercept)") |>
      transmute(study, topic, outcome = str_remove(variable, "_.+"), pid_3,
                quantity = "Difference", estimate, std.error, p.value)
  ) |>
  left_join(tab_n, by = c("study", "topic", "pid_3")) |>
  transmute(Study = study, Topic = topic, Outcome = outcome, Party = pid_3, N,
            quantity, estimate, std.error, p.value) |>
  arrange(Study, Topic, Outcome, Party, quantity)

write_csv(tab_estimates, here::here(out_dir, "table_d4_counterfactual_accuracy_estimates.csv"))

# The article counts the tests over the ten Figure 4 treatments only: the eight in
# Study 1 plus Obama Torture and Trump Coal in Study 2a, each split by party and by
# potential outcome, which is the 40 it reports.
fig4_cells <-
  tab_assump1 |>
  filter(Study == 1 | (Study == "2a" & str_detect(Topic, "Obama|Trump")))

print(tibble(
  stat = c("Opportunities to evaluate a counterfactual guess",
           "Difference in means tests rejecting at p < 0.05"),
  value = c(nrow(fig4_cells), sum(as.numeric(fig4_cells$p) < 0.05))
))
