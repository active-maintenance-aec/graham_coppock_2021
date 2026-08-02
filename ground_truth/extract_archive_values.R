# graham_coppock_2021/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: original/ (fetched by download_original.R), maintained/output/ (a completed
#   run of run_all.R)
# Description: Run the deposited analysis scripts and record, for every quantity the
#   ground truth reports, how the deposited scripts' own output compares with the
#   rewrite's. The result is the committed archive_values.csv, which build_ground_truth.R
#   reads to fill value_script. Without it, value_script would be an assertion that the
#   two agree rather than a measurement of whether they do.
#
#   This script is deliberately NOT part of run_all.R. It runs the deposit, which needs
#   two edits that cannot be made to original/ itself, so it works in a copy:
#     - the deposit's custom facet_nested() calls ggplot2 internals that no longer exist,
#       and is replaced by ggh4x::facet_nested()
#     - two ggsave() calls write to drafts/ directories the deposit does not ship
#   Running the deposit also leaves an Rplots.pdf and a stray JPEG behind, which is the
#   reason the copy exists: original/ is the deposit and must stay byte-identical to it.
#
#   ARCHIVE_RUN_DIR sets where the copy is made; the default is a temporary directory,
#   so the script is runnable by anyone who has cloned the repo and fetched original/.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_archive_values.R")

run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                      unset = file.path(tempdir(), "graham_coppock_2021_archive_run"))

out_dir <- here::here("maintained", "output")
stopifnot(dir.exists(out_dir))
stopifnot(dir.exists(here::here("original")))

# Build the scratch copy ----
dir.create(file.path(run_dir, "drafts", "POQ_RR", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "drafts", "POQ_final", "final figures"), recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(here::here("original"), full.names = TRUE), run_dir, overwrite = TRUE)

write_lines(
  c("", "# Added by extract_archive_values.R; not part of the deposit.",
    "library(ggh4x)", "facet_nested <- ggh4x::facet_nested"),
  file.path(run_dir, "functions.R"), append = TRUE
)

# Run the deposit ----
# Each deposited script opens with rm(list = ls()), so each gets its own environment and
# the second cannot see the first's objects.
archive_main <- new.env()
archive_impeach <- new.env()

# analysis_impeach.R builds Figure 1's panel (b) into an object called summary_df and
# then reuses the same name for a supplementary figure further down, so the panel (b)
# means are gone by the time the script ends. It is sourced in two pieces, split at the
# section header that follows panel (b), to capture them.
impeach_lines <- read_lines(file.path(run_dir, "analysis_impeach.R"))
combine_section <- which(str_detect(impeach_lines, "^#  COMBINE AND EXPORT"))
stopifnot(length(combine_section) == 1)

old_wd <- getwd()
setwd(run_dir)
pdf(NULL)
source("analysis_main.R", local = archive_main, echo = FALSE)
source(textConnection(impeach_lines[seq_len(combine_section - 1)]),
       local = archive_impeach, echo = FALSE)
archive_fig1b <- archive_impeach$summary_df
source(textConnection(impeach_lines[combine_section:length(impeach_lines)]),
       local = archive_impeach, echo = FALSE)
dev.off()
setwd(old_wd)

stopifnot("pid_7" %in% names(archive_fig1b))

# Compare each archive object against the rewrite output it corresponds to ----
# Each comparison joins on a key that is unique on both sides, and records the row count
# on each side alongside the largest absolute difference, so a join that silently drops
# rows shows up as a row count that does not agree rather than as a small maximum.
compare <- function(label, archive_tbl, rewrite_tbl, keys, values) {
  a <- archive_tbl |> as_tibble() |> mutate(across(all_of(keys), as.character))
  r <- rewrite_tbl |> as_tibble() |> mutate(across(all_of(keys), as.character))
  stopifnot(!any(duplicated(a[keys])), !any(duplicated(r[keys])))
  j <- inner_join(a |> select(all_of(c(keys, values))),
                  r |> select(all_of(c(keys, values))),
                  by = keys, suffix = c("_a", "_r"))
  diffs <- map_dbl(values, \(v) max(abs(j[[str_c(v, "_a")]] - j[[str_c(v, "_r")]]), na.rm = TRUE))
  tibble(
    object = label,
    rows_archive = nrow(a),
    rows_rewrite = nrow(r),
    rows_joined = nrow(j),
    max_abs_diff = max(diffs)
  )
}

grand <- compare(
  "grand comparison estimates",
  archive_main$grand_plot_df |> mutate(Topic = as.character(Topic), Format = as.character(Format)),
  read_rds(file.path(out_dir, "estimates_grand_comparison.rds")) |>
    mutate(Topic = as.character(Topic), Format = as.character(Format)),
  c("study", "topic", "Party", "Estimator", "Format", "category", "outcome", "term"),
  c("estimate", "std.error", "conf.low", "conf.high", "value")
)

pretreated <- compare(
  "pretreated estimates",
  archive_main$grand_plot_df_treatOnly |> mutate(Topic = as.character(Topic), Format = as.character(Format)),
  read_rds(file.path(out_dir, "estimates_pretreated.rds")) |>
    mutate(Topic = as.character(Topic), Format = as.character(Format)),
  c("study", "topic", "Party", "Estimator", "Format", "category", "outcome", "term"),
  c("estimate", "std.error", "conf.low", "conf.high", "value")
)

fig1 <- compare(
  "Figure 1b group means",
  archive_fig1b,
  read_csv(file.path(out_dir, "figure_1_impeach_counterfactual.csv"), show_col_types = FALSE),
  "pid_7",
  c("estimate", "std.error", "conf.low", "conf.high")
)

fig3a <- compare(
  "Figure 3a estimates",
  archive_main$tab_reduceChange |> mutate(Topic = as.character(Topic)),
  read_csv(file.path(out_dir, "figure_3a_reporting_any_change.csv"), show_col_types = FALSE),
  c("topic", "Party"),
  c("estimate", "std.error", "conf.low", "conf.high", "p.value")
)

d3 <- compare(
  "Table D.3 cells",
  archive_main$tab_selfPct_appendix |>
    transmute(Study, Topic, Party, Format, category,
              estimate_pct = 100 * estimate, se_pct = 100 * std.error),
  read_csv(file.path(out_dir, "table_d3_change_distribution_estimates.csv"), show_col_types = FALSE),
  c("Study", "Topic", "Party", "Format", "category"),
  c("estimate_pct", "se_pct")
)

archive_d4 <-
  bind_rows(
    archive_main$tab_means |>
      mutate(
        outcome_var = str_remove(variable, "_tilde"),
        quantity = case_when(
          Z_label == "Control"   & outcome_var == "Y0" ~ "Actual",
          Z_label == "Control"   & outcome_var == "Y1" ~ "Guess",
          Z_label == "Treatment" & outcome_var == "Y1" ~ "Actual",
          Z_label == "Treatment" & outcome_var == "Y0" ~ "Guess"
        )
      ) |>
      transmute(Study = study, Topic = topic, Outcome = outcome_var, Party = pid_3,
                quantity, estimate, std.error),
    archive_main$tab_DIM |>
      filter(term != "(Intercept)") |>
      transmute(Study = study, Topic = topic, Outcome = str_remove(variable, "_.+"),
                Party = pid_3, quantity = "Difference", estimate, std.error)
  )

d4 <- compare(
  "Table D.4 cells",
  archive_d4,
  read_csv(file.path(out_dir, "table_d4_counterfactual_accuracy_estimates.csv"), show_col_types = FALSE),
  c("Study", "Topic", "Outcome", "Party", "quantity"),
  c("estimate", "std.error")
)

d5_point <- compare(
  "Table D.5 point estimates",
  archive_main$tab_ATE_vs_self |>
    transmute(Study = study, Topic = topic, Party = pid_3, Estimator = estimator, estimate),
  read_csv(file.path(out_dir, "table_d5_ate_vs_self_estimates.csv"), show_col_types = FALSE) |>
    select(Study, Topic, Party, Estimator, estimate),
  c("Study", "Topic", "Party", "Estimator"),
  "estimate"
)

d5_boot <- compare(
  "Table D.5 bootstrap quantities",
  archive_main$tab_ATE_vs_self |>
    transmute(Study = study, Topic = topic, Party = pid_3, Estimator = estimator,
              std.error, conf.low, conf.high),
  read_csv(file.path(out_dir, "table_d5_ate_vs_self_estimates.csv"), show_col_types = FALSE) |>
    select(Study, Topic, Party, Estimator, std.error, conf.low, conf.high),
  c("Study", "Topic", "Party", "Estimator"),
  c("std.error", "conf.low", "conf.high")
)

# How many published Table D.5 rows the deposited script reproduces ----
# The published table prints two decimals, so a row counts as reproduced when all three
# bootstrap quantities agree at two decimals.
published_d5 <- read_csv(here::here("ground_truth", "published_table_d5.csv"),
                         col_types = cols(.default = col_character()))

archive_d5_rows <-
  archive_main$tab_ATE_vs_self |>
  transmute(study = as.character(study), topic = as.character(topic),
            party = as.character(pid_3), estimator = as.character(estimator),
            se_a = std.error, lo_a = conf.low, hi_a = conf.high) |>
  inner_join(published_d5, by = c("study", "topic", "party", "estimator"))

stopifnot(nrow(archive_d5_rows) == nrow(published_d5))

n_d5_boot_reproduced <- sum(
  sprintf("%.2f", archive_d5_rows$se_a) == archive_d5_rows$se &
    sprintf("%.2f", archive_d5_rows$lo_a) == archive_d5_rows$ci_low &
    sprintf("%.2f", archive_d5_rows$hi_a) == archive_d5_rows$ci_high
)

# Write ----
archive_values <-
  bind_rows(grand, pretreated, fig1, fig3a, d3, d4, d5_point, d5_boot) |>
  mutate(published_rows_reproduced = if_else(
    object == "Table D.5 bootstrap quantities", n_d5_boot_reproduced, NA_integer_
  ))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(archive_values, width = 200)
