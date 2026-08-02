# graham_coppock_2021/maintained/table_d5_ate_vs_self.R
# Output: output/table_d5_ate_vs_self.csv, output/table_d5_ate_vs_self_estimates.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Reproduces Appendix Table D.5, the experimental effect against the
#   self-reported effect with percentile bootstrap intervals over 10,000 resamples.
#   Seed 0, matching the archive. The first output is the table as the appendix lays it
#   out; the second is the same numbers unrounded.

source(here::here("maintained", "helpers.R"))

set.seed(0)

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
state_both_no_inds <- clean$state_both_no_inds

# Filter to study 1 and study 2a Obama/Trump topics (20 cells)
dat_d5 <-
  state_both_no_inds |>
  filter(study == "1" | (study == "2a" & str_detect(topic, "Obama|Trump")))

# Point estimates (no bootstrap)
one_difference <- function(d) {
  exper      <- mean(d$Y1, na.rm = TRUE) - mean(d$Y0, na.rm = TRUE)
  guess_val  <- mean(d$tau_i_tilde, na.rm = TRUE)
  tibble(
    Experiment = exper,
    Guess      = guess_val,
    Difference = exper - guess_val
  )
}

tab_point <-
  dat_d5 |>
  group_by(study, topic, Topic, pid_3) |>
  reframe(one_difference(pick(everything()))) |>
  pivot_longer(cols = c(Experiment, Guess, Difference),
               names_to = "estimator", values_to = "estimate")

# Bootstrap SEs using rsample (Nsim = 10000 matching original)
Nsim <- 10000

boot_summary <-
  dat_d5 |>
  group_by(study, topic, pid_3) |>
  group_modify(\(d, k) {
    boots <- bootstraps(d, times = Nsim)
    map_dfr(boots$splits, \(split) {
      s <- analysis(split)
      one_difference(s)
    }) |>
      pivot_longer(everything(), names_to = "estimator", values_to = "boot_est") |>
      group_by(estimator) |>
      summarize(
        std.error = sd(boot_est),
        conf.low  = quantile(boot_est, .025),
        conf.high = quantile(boot_est, .975),
        .groups   = "drop"
      )
  })

tab_ate_vs_self <- left_join(tab_point, boot_summary,
                              by = c("study", "topic", "pid_3", "estimator"))

# Counts the article states ----
n_diff <- tab_ate_vs_self |> filter(estimator == "Difference") |> nrow()
n_sig <- tab_ate_vs_self |>
  filter(estimator == "Difference", sign(conf.low) == sign(conf.high)) |> nrow()

print(tibble(
  stat = c("Comparisons of the experiment against the counterfactual guess",
           "Differences whose 95 percent bootstrap interval excludes zero"),
  value = c(n_diff, n_sig)
))

# N per cell ----
tab_n <-
  dat_d5 |>
  group_by(study, topic, pid_3) |>
  summarize(N = n(), .groups = "drop")

tab_out <-
  tab_ate_vs_self |>
  left_join(tab_n, by = c("study", "topic", "pid_3")) |>
  mutate(
    CI        = paste0("(", format_num(conf.low, 2), ", ", format_num(conf.high, 2), ")"),
    estimate  = format_num(estimate, 2),
    std.error = format_num(std.error, 2),
    estimator = factor(estimator, c("Experiment", "Guess", "Difference"))
  ) |>
  arrange(study, topic, pid_3, estimator) |>
  select(
    Study = study, Topic = topic, Party = pid_3, N,
    Estimator = estimator, Estimate = estimate, SE = std.error, CI
  )

write_csv(tab_out, here::here(out_dir, "table_d5_ate_vs_self.csv"))

# The same cells unrounded ----
tab_estimates <-
  tab_ate_vs_self |>
  left_join(tab_n, by = c("study", "topic", "pid_3")) |>
  transmute(Study = study, Topic = topic, Party = pid_3, N,
            Estimator = factor(estimator, c("Experiment", "Guess", "Difference")),
            estimate, std.error, conf.low, conf.high) |>
  arrange(Study, Topic, Party, Estimator)

write_csv(tab_estimates, here::here(out_dir, "table_d5_ate_vs_self_estimates.csv"))

print(tab_out, n = 60)
