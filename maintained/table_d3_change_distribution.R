# graham_coppock_2021/maintained/table_d3_change_distribution.R
# Output: output/table_d3_change_distribution.csv,
#   output/table_d3_change_distribution_estimates.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Reproduces Appendix Table D.3, the distribution of self-reported change
#   (more, less, same, and the more-minus-less difference) by study, topic, party and
#   format. The first output is the table as the appendix lays it out, one formatted
#   cell per entry; the second is the same numbers unrounded, so a comparison against
#   the published table does not have to parse and re-round a formatted string.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
substitution_dat <- clean$substitution_dat

# Percent distribution of YC responses by group
tab_selfpct_appendix <-
  substitution_dat |>
  group_by(Study = study, Topic = topic, Party, Format = format) |>
  reframe(rbind(
    tidy(lm_robust((YC == -1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Less"),
    tidy(lm_robust((YC ==  0) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Same"),
    tidy(lm_robust((YC ==  1) ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "More"),
    tidy(lm_robust( YC        ~ 1, data = pick(everything()), clusters = id)) |> mutate(category = "Diff")
  ))

tab_selfpct_n <-
  substitution_dat |>
  group_by(Study = study, Topic = topic, Party, Format = format) |>
  summarize(N = sum(!is.na(YC)), .groups = "drop")

tab_out <-
  tab_selfpct_appendix |>
  mutate(
    entry = paste0(format_num(100 * estimate, 1), " (", format_num(100 * std.error, 1), ")")
  ) |>
  select(Study, Topic, Format, Party, category, entry) |>
  pivot_wider(names_from = category, values_from = entry) |>
  arrange(Study, Topic, Party, Format) |>
  left_join(tab_selfpct_n, by = c("Study", "Topic", "Party", "Format")) |>
  mutate(`Y1?` = recode_values(Format, "Change" ~ "No", "Change + level" ~ "Yes")) |>
  select(Study, Topic, Party, `Y1?`, N, More, Less, Same, Diff)

write_csv(tab_out, here::here(out_dir, "table_d3_change_distribution.csv"))

# The same cells unrounded, in the percentage points the table prints ----
tab_estimates <-
  tab_selfpct_appendix |>
  left_join(tab_selfpct_n, by = c("Study", "Topic", "Party", "Format")) |>
  transmute(Study, Topic, Party, Format,
            `Y1?` = recode_values(Format, "Change" ~ "No", "Change + level" ~ "Yes"),
            N, category, estimate_pct = 100 * estimate, se_pct = 100 * std.error) |>
  arrange(Study, Topic, Party, Format, category)

write_csv(tab_estimates, here::here(out_dir, "table_d3_change_distribution_estimates.csv"))

print(tab_out, n = 40)
