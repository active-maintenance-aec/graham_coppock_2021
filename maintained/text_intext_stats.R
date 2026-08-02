# graham_coppock_2021/maintained/text_intext_stats.R
# Output: output/text_intext_stats.csv
# Depends on: clean_studies.R output, figure_3_response_substitution.R output, helpers.R
# Description: Computes the statistics the article states in the body text: the
#   impeachment illustration, the four sample sizes, and the response substitution
#   effects reported alongside Figure 3. The last of those are the pooled rows of
#   Figure 3's own estimates, read back rather than re-estimated. Values are written at
#   full precision; the report rounds them to the precision the article uses.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
all <- clean$all

dat_impeach <- read_rds(here::here(out_dir, "data_impeach_clean.rds"))

# Impeachment illustration ----
# The archive divides a typed-in 0.16 by the standard deviation of Y1. Here the
# numerator is the fitted mean, so the ratio traces to the data.
n_impeach <- sum(!is.na(dat_impeach$impeach_tau_i))
cor_y1_y0 <- cor(dat_impeach$impeach_Y1, dat_impeach$impeach_Y0, use = "complete.obs")
pct_zero <- mean(dat_impeach$impeach_tau_i == 0, na.rm = TRUE)
mean_tau <- tidy(lm_robust(impeach_tau_i ~ 1, data = dat_impeach))
effect_size <- mean_tau$estimate[1] / sd(dat_impeach$impeach_Y1, na.rm = TRUE)

# Sample sizes ----
# Each study's N is the number of distinct respondents assigned to one of its
# question format conditions, which is the count the archive prints. For Studies 1,
# 2a and 2b it is also the N the article reports. For Study 3 it is not: the deposited
# data hold 1,023 distinct respondents and the article reports 1,074, so the deposit
# is short of the full survey and the article's figure cannot be recovered from it.
n_study1 <- all |>
  filter(study == "1", format %in% c("Counterfactual", "Change + level", "Change")) |>
  distinct(id) |> nrow()
n_study2a <- all |>
  filter(study == "2a", format %in% c("Counterfactual", "Change")) |>
  distinct(id) |> nrow()
n_study2b <- all |>
  filter(study == "2b", format %in% c("Counterfactual", "Change", "Simultaneous")) |>
  distinct(id) |> nrow()
n_study3_formats <- all |>
  filter(study == "3", format %in% c("Counterfactual", "Change", "Change + level")) |>
  distinct(id) |> nrow()
n_study3_deposit <- all |> filter(study == "3") |> distinct(id) |> nrow()

# Response substitution, the effects quoted beside Figure 3 ----
# Figure 3's panel (a) estimates these; the pooled rows are what the text quotes.
fig3a <- read_csv(here::here(out_dir, "figure_3a_reporting_any_change.csv"),
                  show_col_types = FALSE)

fit_overall <- fig3a |> filter(Topic == "Pooled", Party == "All")

fig3_party <-
  fig3a |>
  filter(Topic == "Pooled", Party != "All") |>
  arrange(factor(Party, c("Democrat", "Republican", "Independent"))) |>
  pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value") |>
  mutate(stat = paste0("Fig 3a ", Party, " ",
                       if_else(quantity == "estimate", "estimate", "SE"))) |>
  select(stat, value)

stats <- bind_rows(tibble(
  stat = c(
    "N impeachment survey",
    "cor(Y1, Y0) impeachment",
    "Share reporting exactly zero change",
    "Mean tau_i impeachment",
    "SE of mean tau_i impeachment",
    "Mean tau_i in SD units of Y1",
    "N study 1 (format conditions)",
    "N study 2a (format conditions)",
    "N study 2b (format conditions)",
    "N study 3 (format conditions)",
    "N study 3 (respondents in the deposit)",
    "Fig 3a overall estimate",
    "Fig 3a overall SE"
  ),
  value = c(
    n_impeach,
    cor_y1_y0,
    pct_zero,
    mean_tau$estimate[1],
    mean_tau$std.error[1],
    effect_size,
    n_study1, n_study2a, n_study2b, n_study3_formats, n_study3_deposit,
    fit_overall$estimate[1],
    fit_overall$std.error[1]
  )
), fig3_party)

write_csv(stats, here::here(out_dir, "text_intext_stats.csv"))
print(stats, n = 30)
