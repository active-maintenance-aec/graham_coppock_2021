# graham_coppock_2021/maintained/estimates_pretreated.R
# Output: output/estimates_pretreated.rds, output/estimates_pretreated.csv
# Depends on: clean_studies.R output, helpers.R
# Description: The same three quantities as estimates_grand_comparison.R, but grouped
#   by the format a subject actually saw first rather than by the format condition.
#   That is the split the nonrandomized counterfactual analysis needs, because the
#   pretreated cases have no experimental benchmark to compare against. Figure 5 and
#   Figure E.2's lower panel both read this object.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
all <- clean$all
format_levels <- clean$format_levels

# Self-reported change shares and the more-minus-less statistic ----
tab_selfpct_treat_only <-
  all |>
  filter(format_whichFirst %in% c("Counterfactual", "Change", "Change + level", "Simultaneous"),
         pid_3 != "Independent") |>
  group_by(study, topic, Topic, pid_3, format) |>
  reframe(bind_rows(
    tidy(lm_robust((YC == -1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Less"),
    tidy(lm_robust((YC == 0) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Same"),
    tidy(lm_robust((YC == 1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "More"),
    tidy(lm_robust(YC ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Diff")
  )) |>
  mutate(Estimator = "More-less", value = estimate)

# Counterfactual-format CATE ----
tab_cate_treat_only <-
  all |>
  filter(format_whichFirst == "Counterfactual", pid_3 != "Independent") |>
  group_by(study, topic, Topic, pid_3) |>
  reframe(
    tidy(lm_robust(tau_i_tilde ~ 1, data = pick(everything()))) |>
      mutate(format = "Counterfactual")
  ) |>
  mutate(Estimator = "CATE")

# Simultaneous format, Study 2b ----
tab_cate_simultaneous <-
  all |>
  filter(format_whichFirst == "Simultaneous", pid_3 != "Independent", study == "2b") |>
  group_by(study, topic, Topic, pid_3) |>
  reframe(
    tidy(lm_robust(tau_i_tilde ~ 1, data = pick(everything()))) |>
      mutate(format = "Simultaneous")
  ) |>
  mutate(Estimator = "CATE")

# Assemble ----
# The category shares are drawn as stacked bars and carry no interval, so the
# estimate, interval and outside label are blanked for them.
grand_plot_df_treat_only <-
  bind_rows(tab_selfpct_treat_only, tab_cate_treat_only, tab_cate_simultaneous) |>
  mutate(
    Party = pid_3,
    Format = format,
    Estimator = relevel(factor(Estimator), "More-less"),
    category = recode_values(category, "Diff" ~ "  Diff", "More" ~ " More", "Same" ~ " Same",
                             default = category),
    Topic = str_replace_all(Topic, "\\n", " "),
    Format = factor(Format, format_levels)
  ) |>
  mutate(
    Topic = factor(Topic, unique(Topic)),
    lab_outside = paste0(format_num(estimate, 2), " (", format_num(std.error, 2), ")")
  ) |>
  mutate(
    is_share = str_detect(outcome, "0|1"),
    estimate = if_else(is_share, NA_real_, estimate),
    conf.low = if_else(is_share, NA_real_, conf.low),
    conf.high = if_else(is_share, NA_real_, conf.high),
    lab_outside = if_else(is_share, NA_character_, lab_outside)
  ) |>
  select(-is_share)

write_rds(grand_plot_df_treat_only, here::here(out_dir, "estimates_pretreated.rds"))
write_csv(grand_plot_df_treat_only, here::here(out_dir, "estimates_pretreated.csv"))

print(grand_plot_df_treat_only |> count(Estimator, Format), n = 20)
