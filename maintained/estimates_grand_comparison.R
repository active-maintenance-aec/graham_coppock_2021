# graham_coppock_2021/maintained/estimates_grand_comparison.R
# Output: output/estimates_grand_comparison.rds, output/estimates_grand_comparison.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Estimates every quantity the grand comparison figures display: the
#   share of respondents in each self-reported change category and the more-minus-less
#   statistic (clustered by respondent), the counterfactual-format CATE, and the
#   experimental difference in means. Figures 2 and 4 and the sign comparison all read
#   this one object, so no estimate in the paper is computed twice.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
all <- clean$all
format_levels <- clean$format_levels

# Self-reported change shares and the more-minus-less statistic ----
tab_selfpct <-
  all |>
  filter(format %in% c("Counterfactual", "Change", "Change + level", "Simultaneous"),
         pid_3 != "Independent") |>
  group_by(study, topic, Topic, pid_3, format) |>
  reframe(bind_rows(
    tidy(lm_robust((YC == -1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Less"),
    tidy(lm_robust((YC == 0) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Same"),
    tidy(lm_robust((YC == 1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "More"),
    tidy(lm_robust(YC ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Diff")
  )) |>
  mutate(Estimator = "More-less", value = estimate)

# Counterfactual-format CATE and the experimental benchmark ----
tab_cate <-
  all |>
  filter(format == "Counterfactual", pid_3 != "Independent") |>
  group_by(study, topic, Topic, pid_3) |>
  reframe(bind_rows(
    tidy(lm_robust(tau_i_tilde ~ 1, data = pick(everything()))) |>
      mutate(format = "Counterfactual"),
    tidy(lm_robust(Y ~ Z_label, data = pick(everything()))) |>
      mutate(format = "Diff. in means") |>
      filter(str_detect(term, "Z_label"))
  )) |>
  mutate(Estimator = "CATE")

# Simultaneous format, study 2b, appendix only ----
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
# estimate, interval and outside label are blanked for them; the more-minus-less
# and CATE rows keep theirs.
grand_plot_df <-
  bind_rows(tab_selfpct, tab_cate, tab_cate_simultaneous) |>
  mutate(
    Party = pid_3,
    Format = format,
    Estimator = relevel(factor(Estimator), "More-less"),
    category = recode_values(category, "Diff" ~ "  Diff", "More" ~ " More", "Same" ~ " Same",
                             default = category),
    Topic = str_replace_all(Topic, "\\n", " "),
    format = factor(format, format_levels),
    Format = factor(Format, format_levels)
  ) |>
  # Facet order. The deposited script sorts on factor levels c(1, 4, 2, 3), which do not
  # include "2a" or "2b", so those two studies sort last as NA. That ordering is what
  # produced the published facet sequence, so it is kept; the sort key is dropped again
  # below rather than carried into the output, where an all-NA study column for Studies
  # 2a and 2b would be a broken join key for anything reading the file.
  mutate(study_order = factor(study, c(1, 4, 2, 3))) |>
  arrange(as.numeric(str_detect(Topic, "Kav")), study_order) |>
  select(-study_order) |>
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

write_rds(grand_plot_df, here::here(out_dir, "estimates_grand_comparison.rds"))
write_csv(grand_plot_df, here::here(out_dir, "estimates_grand_comparison.csv"))

print(grand_plot_df |> count(Estimator, Format), n = 20)
