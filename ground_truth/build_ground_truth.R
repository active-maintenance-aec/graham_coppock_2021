# graham_coppock_2021/ground_truth/build_ground_truth.R
# Output: ground_truth/graham_coppock_2021_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first), published_claims.csv, the
#   published_*.csv transcriptions and archive_values.csv in this folder, and
#   maintained/in_text_claims.R
# Description: Assemble the ground truth table and run the gates over it.
#
#   Provenance, which is the whole point of the file:
#     - value_paper comes only from the published article and its supplementary
#       materials. It is either a string in a tribble below or a cell of one of the
#       published_*.csv files beside this script, which transcribe the three appendix
#       tables and the printed labels of Figures 2, 4, 5 and E.2 in the layout the
#       pages print them in. Published values are comparison targets: no number typed
#       here is an input to any computation, in this file or anywhere in maintained/.
#     - value_rewrite is read out of maintained/output/ and is never typed.
#     - value_script comes from ground_truth/archive_values.csv, which
#       extract_archive_values.R writes by running the deposited scripts and comparing
#       their output with the rewrite's, object by object.
#     - match, match_rewrite and holds are computed, never asserted.
#
#   Published values are kept as strings so the precision the page prints survives, and
#   a value agrees when the rewrite's number, printed to that precision, gives the same
#   digits. Reading them as numbers would silently turn a standard error printed as
#   0.20 into a target good to one decimal place.
#
#   published_claims.csv is the extraction: every numeric token in the article and the
#   supplementary materials, classified by hand. Every pipeline and descriptive row of it
#   must have a row here and a block in maintained/in_text_claims.R, and that file is run
#   below rather than read, so a block that errors or prints nothing fails the gate.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)
pub <- function(f) read_csv(here::here("ground_truth", f), col_types = cols(.default = col_character()))

txt <- out("text_intext_stats.csv")
fig1 <- out("figure_1_impeach_counterfactual.csv")
fig2 <- out("figure_2_cornish_example.csv")
fig3a <- out("figure_3a_reporting_any_change.csv")
fig3b_means <- out("figure_3b_self_reported_means.csv")
fig3b_eff <- out("figure_3b_level_question_effects.csv")
fig4 <- out("figure_4_grand_comparison.csv")
fig5 <- out("figure_5_pretreat_comparison.csv")
fige2 <- out("figure_e2_study2b_simultaneous.csv")
d3 <- out("table_d3_change_distribution_estimates.csv")
d4 <- out("table_d4_counterfactual_accuracy_estimates.csv")
d5 <- out("table_d5_ate_vs_self_estimates.csv")
signs <- out("text_sign_comparison.csv")
grand <- out("estimates_grand_comparison.csv")

clean <- read_rds(here::here("maintained", "output", "data_study1-3_clean.rds"))

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)
published_claims <- pub("published_claims.csv")

stat <- function(s) txt$value[txt$stat == s]
norm <- function(x) str_squish(str_replace_all(x, "\n", " "))

# Agreement at the precision the page prints ----
decimals <- function(s) if_else(str_detect(s, "\\."), nchar(str_remove(s, "^.*\\.")), 0L)

# A value rounded to zero prints as "-0.00" when it is very slightly negative, and no
# page prints that, so the sign is dropped from an all-zero result before comparing.
fmt <- function(x, digits) {
  digits <- rep_len(digits, length(x))
  s <- rep(NA_character_, length(x))
  ok <- !is.na(x) & !is.na(digits)
  s[ok] <- sprintf("%.*f", digits[ok], x[ok])
  if_else(!is.na(s) & str_detect(s, "^-0\\.?0*$"), str_remove(s, "^-"), s)
}

# Most claims are compared at the page's own precision. A claim the article hedges needs
# more decimals than the page carries, or the evidence for the hedge disappears: a
# distance of 0.51 printed to a page that says "half a point" reads as 0.5 and says
# nothing about whether the hedge holds.
format_to_page <- function(value, target, digits = NA_integer_) {
  digits <- rep_len(digits, length(target))
  fmt(value, if_else(is.na(digits), decimals(target), digits))
}

agrees <- function(value, target, digits = NA_integer_) {
  as.numeric(format_to_page(value, target, digits) ==
               format_to_page(as.numeric(target), target, digits))
}

# Joins between a published table and a pipeline output ----
# Both sides must be unique on the key and every row must find a partner, so a label
# that does not line up is an error here rather than a silent NA that then reads as a
# reproduction failure.
join_published <- function(published, rewritten, by, label) {
  dup_p <- published |> count(pick(all_of(by))) |> filter(n > 1)
  dup_r <- rewritten |> count(pick(all_of(by))) |> filter(n > 1)
  if (nrow(dup_p) > 0 || nrow(dup_r) > 0) {
    stop(label, ": join key is not unique (", nrow(dup_p), " duplicated published keys, ",
         nrow(dup_r), " duplicated rewrite keys).")
  }
  joined <- full_join(published, rewritten, by = by)
  if (nrow(joined) != nrow(published) || nrow(joined) != nrow(rewritten)) {
    stop(label, ": join is not one to one. Published rows ", nrow(published),
         ", rewrite rows ", nrow(rewritten), ", joined rows ", nrow(joined), ".")
  }
  joined
}

# The deposited scripts against the rewrite ----
# extract_archive_values.R compared each deposited object with the maintained output it
# corresponds to. Everything except the Table D.5 bootstrap agreed to machine precision,
# which is what lets value_script carry the rewrite's numbers below.
agreeing_objects <- archive |> filter(object != "Table D.5 bootstrap quantities")
stopifnot(all(agreeing_objects$max_abs_diff < 1e-10))
stopifnot(all(agreeing_objects$rows_joined == agreeing_objects$rows_archive))

archive_d5_boot_reproduced <-
  archive$published_rows_reproduced[archive$object == "Table D.5 bootstrap quantities"]

# Printed figure labels ----
# Figures 2, 4, 5 and E.2 print an estimate and a standard error beside every row they
# draw. A label agrees only if both halves do.
labels_rewrite <-
  bind_rows(
    fig2 |> filter(!is.na(label)) |> mutate(figure = "Figure 2", study = as.character(study)),
    fig4 |> filter(!is.na(label)) |> mutate(figure = "Figure 4", study = as.character(study)),
    fig5 |> filter(!is.na(label)) |> mutate(figure = "Figure 5", study = as.character(study)),
    fige2 |> filter(!is.na(label)) |>
      mutate(figure = if_else(str_detect(panel, "^\\(a\\)"), "Figure E.2a", "Figure E.2b"),
             study = as.character(study))
  ) |>
  transmute(figure, study, Topic = norm(Topic), Party,
            Estimator, Format = as.character(Format),
            estimate_rewrite = estimate, se_rewrite = std.error)

labels_published <- pub("published_figure_labels.csv") |> mutate(Topic = norm(Topic))

labels <- join_published(
  labels_published, labels_rewrite,
  by = c("figure", "study", "Topic", "Party", "Estimator", "Format"),
  label = "Printed figure labels"
) |>
  mutate(ok = agrees(estimate_rewrite, estimate) == 1 & agrees(se_rewrite, se) == 1)

label_summary <- labels |>
  group_by(figure) |>
  summarize(n_printed = n(), n_agree = sum(ok), .groups = "drop")

# The one label the article names in the errata gets its own row further down, built from
# the panel rather than from this table, so it exists whether or not it agrees.
claimed_label <- labels$figure == "Figure 4" & labels$Topic == "Undisputed accusation" &
  labels$Party == "Republican" & labels$Estimator == "CATE" &
  labels$Format == "Counterfactual"
stopifnot(sum(claimed_label) == 1)

label_misses <- labels |>
  filter(!ok, !claimed_label[match(str_c(figure, Topic, Party, Estimator, Format),
                                   str_c(labels$figure, labels$Topic, labels$Party,
                                         labels$Estimator, labels$Format))]) |>
  transmute(
    claim_id = NA_character_,
    table_figure = figure,
    claim = paste0(Topic, ", ", Party, ", ", Estimator, ", ", Format),
    value_rewrite = estimate_rewrite,
    value_paper = estimate,
    notes = paste0("Printed label disagrees. Rewrite estimate ",
                   sprintf("%.17g", estimate_rewrite), " and standard error ",
                   sprintf("%.4f", se_rewrite), " against a published ", estimate, " (", se, ").")
  )

# Table D.3 ----
# The published table's second and third value columns are headed "No change" and
# "Less". Its own Difference column is More minus the column headed "No change" in every
# row, to within the one unit in the last printed digit that rounding each cell
# separately allows, so the two headings are transposed relative to the body: the column
# headed "No change" holds the share reporting less. The comparison below reads the
# columns in that corrected order, and one claim row records how many cells agree if the
# headings are taken at face value instead. errata.qmd computes both counts.
d3_rewrite <- d3 |>
  transmute(study = as.character(Study), topic = norm(Topic), party = Party,
            level_q = `Y1?`, N_rewrite = N, category,
            estimate_rewrite = estimate_pct, se_rewrite = se_pct)

d3_published_long <- pub("published_table_d3.csv") |>
  pivot_longer(c(more, no_change, less, difference), names_to = "printed", values_to = "estimate") |>
  mutate(se = case_when(printed == "more" ~ more_se, printed == "no_change" ~ no_change_se,
                        printed == "less" ~ less_se, .default = difference_se)) |>
  select(study, topic, party, level_q, N, printed, estimate, se)

# The evidence for the transposition, computed rather than asserted in a note.
d3_difference_source <- pub("published_table_d3.csv") |>
  transmute(from_no_change = abs(as.numeric(difference) -
                                   (as.numeric(more) - as.numeric(no_change))) < 0.15,
            from_less = abs(as.numeric(difference) -
                              (as.numeric(more) - as.numeric(less))) < 0.15)

d3_corrected <- d3_published_long |>
  mutate(category = recode_values(printed, "more" ~ "More", "no_change" ~ "Less",
                                  "less" ~ "Same", "difference" ~ "Diff"))

d3_literal <- d3_published_long |>
  mutate(category = recode_values(printed, "more" ~ "More", "no_change" ~ "Same",
                                  "less" ~ "Less", "difference" ~ "Diff"))

d3_cmp <- join_published(
  d3_corrected |> select(-printed), d3_rewrite,
  by = c("study", "topic", "party", "level_q", "category"),
  label = "Table D.3 (corrected column order)"
) |>
  mutate(ok = agrees(estimate_rewrite, estimate) == 1 & agrees(se_rewrite, se) == 1,
         n_ok = as.integer(N) == N_rewrite)

d3_cmp_literal <- join_published(
  d3_literal |> select(-printed), d3_rewrite,
  by = c("study", "topic", "party", "level_q", "category"),
  label = "Table D.3 (printed column order)"
) |>
  mutate(ok = agrees(estimate_rewrite, estimate) == 1 & agrees(se_rewrite, se) == 1)

# Table D.4 ----
d4_rewrite <- d4 |>
  transmute(study = as.character(Study), topic = norm(Topic), outcome = Outcome,
            party = Party, N_rewrite = N, quantity,
            estimate_rewrite = estimate, se_rewrite = std.error, p_rewrite = p.value)

d4_published <- pub("published_table_d4.csv") |>
  pivot_longer(c(actual, guess, difference), names_to = "printed", values_to = "estimate") |>
  mutate(se = case_when(printed == "actual" ~ actual_se, printed == "guess" ~ guess_se,
                        .default = difference_se),
         quantity = str_to_title(printed)) |>
  select(study, topic, outcome, party, N, p, quantity, estimate, se)

d4_cmp <- join_published(
  d4_published, d4_rewrite,
  by = c("study", "topic", "outcome", "party", "quantity"),
  label = "Table D.4"
) |>
  mutate(ok = agrees(estimate_rewrite, estimate) == 1 & agrees(se_rewrite, se) == 1,
         n_ok = as.integer(N) == N_rewrite,
         p_ok = agrees(p_rewrite, p) == 1)

# Table D.5 ----
d5_rewrite <- d5 |>
  transmute(study = as.character(Study), topic = norm(Topic), party = Party,
            estimator = as.character(Estimator), N_rewrite = N,
            estimate_rewrite = estimate, se_rewrite = std.error,
            ci_low_rewrite = conf.low, ci_high_rewrite = conf.high)

d5_cmp <- join_published(
  pub("published_table_d5.csv"), d5_rewrite,
  by = c("study", "topic", "party", "estimator"),
  label = "Table D.5"
) |>
  mutate(est_ok = agrees(estimate_rewrite, estimate) == 1,
         n_ok = as.integer(N) == N_rewrite,
         interval_ok = agrees(se_rewrite, se) == 1 &
           agrees(ci_low_rewrite, ci_low) == 1 &
           agrees(ci_high_rewrite, ci_high) == 1)

# Quantities the body text quotes ----
cornish_share <- function(party, category) {
  r <- fig2 |> filter(Party == party, Format == "Change", category == !!category)
  stopifnot(nrow(r) == 1)
  100 * r$share
}

cornish_effect <- function(party, what) {
  r <- fig3b_eff |> filter(norm(topic) == "Disputed accusation", Party == party)
  stopifnot(nrow(r) == 1)
  if (what == "estimate") r$estimate else r$std.error
}

fig3a_pooled <- function(party) {
  r <- fig3a |> filter(Topic == "Pooled", Party == party)
  stopifnot(nrow(r) == 1)
  r$estimate
}

fig3b_sign <- function(topic_label, party) {
  r <- fig3b_eff |> filter(norm(Topic) == topic_label, Party == party)
  stopifnot(nrow(r) == 1)
  sign(r$estimate)
}

# One row of the grand comparison panel, which Figures 2, 4, 5 and E.2 all share.
grand_cell <- function(panel, topic_label, party, estimator, format, column) {
  r <- panel |>
    filter(norm(Topic) == topic_label, Party == party, Estimator == estimator,
           Format == format, is.na(category) | category == "Diff")
  stopifnot(nrow(r) == 1)
  r[[column]]
}

grand_share <- function(panel, topic_label, party, format, which_category) {
  r <- panel |>
    filter(norm(Topic) == topic_label, Party == party, Format == format,
           !is.na(category), category == which_category)
  stopifnot(nrow(r) == 1)
  r$share
}

d4_fig4_cells <- d4 |>
  filter(quantity == "Difference",
         Study == "1" | (Study == "2a" & str_detect(Topic, "Obama|Trump")))

d5_difference <- d5 |> filter(Estimator == "Difference")
d5_excludes_zero <- sign(d5_difference$conf.low) == sign(d5_difference$conf.high)

# The three cells in which the counterfactual format gets the sign wrong, which the
# article says are all cells where neither estimate can be told from zero.
topic_label <- function(key) {
  labels <- unique(norm(grand$Topic[grand$topic == key]))
  stopifnot(length(labels) == 1)
  labels
}

wrong_sign <- signs |>
  filter(counterfactual_wrong_sign) |>
  mutate(topic_label = map_chr(topic, topic_label)) |>
  rowwise() |>
  mutate(
    cf_low = grand_cell(fig4, topic_label, Party, "CATE", "Counterfactual", "conf.low"),
    cf_high = grand_cell(fig4, topic_label, Party, "CATE", "Counterfactual", "conf.high"),
    dim_low = grand_cell(fig4, topic_label, Party, "CATE", "Diff. in means", "conf.low"),
    dim_high = grand_cell(fig4, topic_label, Party, "CATE", "Diff. in means", "conf.high"),
    both_null = cf_low <= 0 & cf_high >= 0 & dim_low <= 0 & dim_high >= 0
  ) |>
  ungroup()

kavanaugh <- expand_grid(topic_label = c("Senator opposed Kavanaugh",
                                         "Senator supported Kavanaugh"),
                         Party = c("Democrat", "Republican")) |>
  rowwise() |>
  mutate(any_change = 1 - grand_share(fig5, topic_label, Party, "Change", "Same"),
         counterfactual = grand_cell(fig5, topic_label, Party, "CATE",
                                     "Counterfactual", "estimate")) |>
  ungroup()

e2a_accuracy <- expand_grid(topic_label = c("Obama torture exec. order",
                                            "Trump coal ash exec. order"),
                            Party = c("Democrat", "Republican")) |>
  rowwise() |>
  mutate(benchmark = grand_cell(fige2, topic_label, Party, "CATE", "Diff. in means", "estimate"),
         cf_gap = abs(grand_cell(fige2, topic_label, Party, "CATE",
                                 "Counterfactual", "estimate") - benchmark),
         simultaneous_gap = abs(grand_cell(fige2, topic_label, Party, "CATE",
                                           "Simultaneous", "estimate") - benchmark)) |>
  ungroup()

# Assemble ----
gt <- bind_rows(
  tribble(
    ~claim_id, ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "intro_impeach_n", "Text, p. 31", "Survey responses obtained from Lucid",
      stat("N impeachment survey"), "4034", "",
    "intro_impeach_correlation", "Text, p. 32", "Correlation of the two impeachment questions",
      stat("cor(Y1, Y0) impeachment"), "0.82",
      "The article gives 0.82; the data give 0.828, which prints as 0.83",
    "intro_impeach_zero_change", "Text, p. 32",
      "Share reporting exactly zero change (per cent)",
      100 * stat("Share reporting exactly zero change"), "72", "",
    "intro_impeach_mean_tau", "Text, p. 32", "Average difference between the two questions",
      stat("Mean tau_i impeachment"), "0.16", "",
    "intro_impeach_mean_tau_se", "Text, p. 32", "Standard error of that average",
      stat("SE of mean tau_i impeachment"), "0.02", "",
    "cornish_democrat_less_share", "Text, p. 38",
      "Cornish, Democrats reporting less likely (per cent)",
      cornish_share("Democrat", "Less"), "87", "",
    "cornish_republican_same_share", "Text, p. 38",
      "Cornish, Republicans reporting no effect (per cent)",
      cornish_share("Republican", "Same"), "57", "",
    "cornish_democrat_change_score", "Text, p. 39",
      "Cornish, Democrats' more-minus-less score, change format",
      grand_cell(fig2, "Disputed accusation", "Democrat", "More-less", "Change", "estimate"),
      "-0.85", "",
    "cornish_democrat_change_level_score", "Text, p. 39",
      "The same score with the level question asked first",
      grand_cell(fig2, "Disputed accusation", "Democrat", "More-less", "Change + level",
                 "estimate"),
      "-0.61", "",
    "cornish_democrat_level_effect", "Text, p. 39",
      "Cornish, effect of the level question on Democrats' more-minus-less score",
      cornish_effect("Democrat", "estimate"), "0.24", "",
    "cornish_democrat_level_effect_se", "Text, p. 39",
      "Standard error of the level question's effect, Democrats",
      cornish_effect("Democrat", "se"), "0.10", "",
    "cornish_republican_level_effect", "Text, p. 39", "Cornish, same effect among Republicans",
      cornish_effect("Republican", "estimate"), "-0.24", "",
    "cornish_republican_level_effect_se", "Text, p. 39",
      "Standard error of the level question's effect, Republicans",
      cornish_effect("Republican", "se"), "0.14", "",
    "cornish_democrat_dim", "Text, p. 39",
      "Cornish, difference in means among Democrats",
      grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Diff. in means", "estimate"),
      "0.02", "",
    "cornish_democrat_dim_se", "Text, p. 39",
      "Standard error of that difference in means, Democrats",
      grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Diff. in means", "std.error"),
      "0.32", "",
    "cornish_republican_dim", "Text, p. 39",
      "Cornish, difference in means among Republicans",
      grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Diff. in means", "estimate"),
      "-2.01", "",
    "cornish_republican_dim_se", "Text, p. 39",
      "Standard error of that difference in means, Republicans",
      grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Diff. in means", "std.error"),
      "0.33", "",
    "cornish_experiment_direction", "Text, p. 39",
      "Parties of two whose experimental estimate is negative with an interval excluding zero",
      sum(c(grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Diff. in means",
                       "conf.high"),
            grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Diff. in means",
                       "conf.high")) < 0),
      "1",
      paste0("The article says Cornish lost support among Republicans, not Democrats, so ",
             "exactly one of the two experimental estimates should be negative and ",
             "distinguishable from zero"),
    "cornish_democrat_counterfactual", "Text, p. 39",
      "Cornish, counterfactual format estimate among Democrats",
      grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Counterfactual", "estimate"),
      "-0.49", "",
    "cornish_democrat_counterfactual_se", "Text, p. 39",
      "Standard error of the counterfactual estimate, Democrats",
      grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Counterfactual", "std.error"),
      "0.11", "",
    "cornish_republican_counterfactual", "Text, p. 40",
      "Cornish, counterfactual format estimate among Republicans",
      grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Counterfactual", "estimate"),
      "-1.07", "",
    "cornish_republican_counterfactual_se", "Text, p. 40",
      "Standard error of the counterfactual estimate, Republicans",
      grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Counterfactual", "std.error"),
      "0.17", "",
    "cornish_democrat_overstatement", "Text, p. 40",
      "Democrats overstate negative change by approximately half a point",
      abs(grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Counterfactual",
                     "estimate") -
            grand_cell(fig2, "Disputed accusation", "Democrat", "CATE", "Diff. in means",
                       "estimate")),
      "0.5",
      "The article hedges with 'approximately', so the two numbers are recorded and no verdict is returned",
    "cornish_republican_understatement", "Text, p. 40",
      "Republicans understate negative change by almost a full point",
      abs(grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Counterfactual",
                     "estimate") -
            grand_cell(fig2, "Disputed accusation", "Republican", "CATE", "Diff. in means",
                       "estimate")),
      "1",
      "The article hedges with 'almost', so the two numbers are recorded and no verdict is returned",
    "design_study1_n", "Text, p. 40", "Study 1 N",
      stat("N study 1 (format conditions)"), "417", "",
    "design_study2a_n", "Text, p. 40", "Study 2a N",
      stat("N study 2a (format conditions)"), "2475", "",
    "design_study2b_n", "Text, p. 44", "Study 2b N",
      stat("N study 2b (format conditions)"), "1110", "",
    "design_study3_n", "Text, p. 44", "Study 3 N",
      stat("N study 3 (change responses)"), "1074",
      paste0("The Study 3 extract carries ", stat("N study 3 (rows in the deposit)"),
             " rows against ", stat("N study 3 (distinct id values in the deposit)"),
             " distinct id values, so a count of distinct ids is not the survey's N. The ",
             "quantity the analysis uses is the number of answers to the change question, ",
             "and the published Table D.3's six Study 3 sample sizes sum to the same figure"),
    # The article states these four as magnitudes of a decrease, without a sign, so the
    # comparison is on the magnitude.
    "results_level_first_overall", "Text, p. 44",
      "Decrease in reporting any change, all respondents (points)",
      abs(100 * stat("Fig 3a overall estimate")), "10", "",
    "results_level_first_overall_se", "Text, p. 44",
      "Standard error of that effect (points)", 100 * stat("Fig 3a overall SE"), "2", "",
    "results_level_first_democrat", "Text, p. 44", "Same decrease among Democrats (points)",
      abs(100 * stat("Fig 3a Democrat estimate")), "14",
      "The article gives 14 points. The estimate is 13.5, which prints as 13",
    "results_level_first_democrat_se", "Text, p. 44", "Standard error, Democrats (points)",
      100 * stat("Fig 3a Democrat SE"), "3", "",
    "results_level_first_republican", "Text, p. 44", "Same decrease among Republicans (points)",
      abs(100 * stat("Fig 3a Republican estimate")), "8", "",
    "results_level_first_republican_se", "Text, p. 44", "Standard error, Republicans (points)",
      100 * stat("Fig 3a Republican SE"), "4", "",
    "results_level_first_independent", "Text, p. 44",
      "Same decrease among pure independents (points)",
      abs(100 * stat("Fig 3a Independent estimate")), "2", "",
    "results_level_first_independent_se", "Text, p. 44",
      "Standard error, independents (points)", 100 * stat("Fig 3a Independent SE"), "6", "",
    "results_level_first_ordering", "Text, p. 44",
      "Party groups of two whose decrease exceeds the independents'",
      sum(abs(c(fig3a_pooled("Democrat"), fig3a_pooled("Republican"))) >
            abs(fig3a_pooled("Independent"))), "2", "",
    "results_sign_opportunities", "Text, p. 46",
      "Cells compared, change format against the experiment", nrow(signs), "20", "",
    "results_change_wrong_sign", "Text, p. 46", "Change format has the opposite sign",
      sum(signs$change_wrong_sign), "12", "",
    "results_counterfactual_wrong_sign", "Text, p. 46",
      "Counterfactual format has the opposite sign", sum(signs$counterfactual_wrong_sign),
      "3", "",
    "results_wrong_sign_cases_null", "Text, p. 46",
      "Sign-miss cells in which neither estimate is distinguishable from zero",
      sum(wrong_sign$both_null), "3",
      paste0("The article says all three. The counterfactual estimate for the disputed ",
             "accusation among Democrats is ",
             sprintf("%.2f", grand_cell(fig4, "Disputed accusation", "Democrat", "CATE",
                                        "Counterfactual", "estimate")),
             " with a 95 per cent interval of (",
             sprintf("%.2f", grand_cell(fig4, "Disputed accusation", "Democrat", "CATE",
                                        "Counterfactual", "conf.low")), ", ",
             sprintf("%.2f", grand_cell(fig4, "Disputed accusation", "Democrat", "CATE",
                                        "Counterfactual", "conf.high")),
             "), which excludes zero, as the article's own Table D.5 also shows"),
    "results_tcja_experiment_null", "Text, p. 46",
      "Tax Cuts and Jobs Act parties of two whose experimental estimate contains zero",
      sum(map_dbl(c("Democrat", "Republican"),
                  ~ grand_cell(fig4, "Tax Cuts and Jobs Act", .x, "CATE",
                               "Diff. in means", "conf.low")) <= 0 &
            map_dbl(c("Democrat", "Republican"),
                    ~ grand_cell(fig4, "Tax Cuts and Jobs Act", .x, "CATE",
                                 "Diff. in means", "conf.high")) >= 0), "2", "",
    "results_outcome_opportunities", "Text, p. 46",
      "Opportunities to evaluate a counterfactual guess", nrow(d4_fig4_cells), "40", "",
    "results_dim_rejections", "Text, p. 47",
      "Difference in means tests rejecting at p < 0.05",
      sum(d4_fig4_cells$p.value < 0.05), "12", "",
    "results_dim_rejection_share", "Text, p. 47",
      "The same count as a percentage of the forty tests",
      100 * mean(d4_fig4_cells$p.value < 0.05), "30.0", "",
    "results_ate_opportunities", "Text, p. 46",
      "Comparisons of the experiment against the counterfactual guess",
      nrow(d5_difference), "20", "",
    "results_significant_differences", "Text, p. 46",
      "Differences whose bootstrap interval excludes zero", sum(d5_excludes_zero), "6", "",
    "results_significant_share", "Text, p. 46",
      "The same count as a percentage of the twenty comparisons",
      100 * mean(d5_excludes_zero), "30", "",
    "nonrand_kavanaugh_change_majorities", "Text, p. 49",
      "Kavanaugh cells of four in which most respondents report the vote changed their preference",
      sum(kavanaugh$any_change > 0.5), "4", "",
    "nonrand_kavanaugh_counterfactual_small", "Text, p. 49",
      "Sign of the counterfactual estimate for Democrats whose Senator opposed Kavanaugh",
      sign(kavanaugh$counterfactual[kavanaugh$topic_label == "Senator opposed Kavanaugh" &
                                      kavanaugh$Party == "Democrat"]), "1", "",
    "nonrand_biden_pattern", "Text, p. 49",
      "Parties of two whose counterfactual estimate of Biden's loss excludes zero",
      sum(map_dbl(c("Democrat", "Republican"),
                  ~ grand_cell(fig5, "Biden skeptical of Anita Hill", .x, "CATE",
                               "Counterfactual", "conf.high")) < 0), "1", "",
    "nonrand_dream_pattern", "Text, p. 49",
      "Of one: whether the DREAM Act counterfactual boost is larger for Republicans",
      sum(grand_cell(fig5, "DREAM Act helps economy", "Republican", "CATE",
                     "Counterfactual", "estimate") >
            grand_cell(fig5, "DREAM Act helps economy", "Democrat", "CATE",
                       "Counterfactual", "estimate")), "1", ""
  ),

  # Figure 1, panel (b): the seven printed group means
  fig1 |> transmute(
    claim_id = paste0("fig1b_pid", pid_7),
    table_figure = "Figure 1b",
    claim = paste0("Average change, party ID ", pid_7),
    value_rewrite = estimate,
    value_paper = c("0.17", "0.19", "0.29", "0.19", "0.14", "0.17", "0.06"),
    notes = "Printed beside each point in the published panel"
  ),

  # Figure 3, which prints no numbers: its shape, its pooled facet and the two facets
  # the surrounding text describes in words
  tribble(
    ~claim_id, ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "intro_impeach_all_but_strong_republicans", "Figure 1b",
      "Partisan groups of seven whose average change is positive with an interval excluding zero",
      sum(fig1$estimate > 0 & fig1$conf.low > 0), "6",
      paste0("The article says every group except strong Republicans. Group 7's estimate ",
             "is ", sprintf("%.3f", fig1$estimate[fig1$pid_7 == 7]), " with an interval of (",
             sprintf("%.3f", fig1$conf.low[fig1$pid_7 == 7]), ", ",
             sprintf("%.3f", fig1$conf.high[fig1$pid_7 == 7]), ")"),
    "fig3a_facets", "Figure 3a",
      "Estimates plotted (ten facets by All, Democrat, Republican)",
      sum(fig3a$Party %in% c("All", "Democrat", "Republican")), "30",
      "The panel draws a Pooled facet and one per treatment in Studies 1 and 3",
    NA, "Figure 3a", "Pooled decrease in reporting any change, all respondents (points)",
      abs(100 * fig3a_pooled("All")), "10",
      "The panel's Pooled facet plots what the surrounding text quotes as a 10 point decrease",
    "fig3b_facets", "Figure 3b",
      "Means plotted (nine facets by two formats by two parties)", nrow(fig3b_means), "36", "",
    "results_endorsed_trump_republican", "Figure 3b",
      "Endorsed Trump, Republicans: sign of the level question's effect",
      fig3b_sign("Endorsed Trump", "Republican"), "-1",
      paste0("The text says asking the level question first reduced Republican claims that their ",
             "support increased, so the effect on the more-minus-less score is negative"),
    "results_mueller_democrat", "Figure 3b",
      "Mueller comments, Democrats: sign of the level question's effect",
      fig3b_sign("Mueller comments", "Democrat"), "-1",
      paste0("The text says Democrats became less likely to claim the comments moved them, so the ",
             "effect on the more-minus-less score is negative")
  ),

  # Printed figure labels: one summary row per figure, the cell the errata names, plus
  # any other disagreement
  label_summary |> transmute(
    claim_id = recode_values(figure,
                             "Figure 2" ~ "fig2_labels_agreeing",
                             "Figure 4" ~ "fig4_labels_agreeing",
                             "Figure 5" ~ "fig5_labels_agreeing",
                             "Figure E.2a" ~ "fige2a_labels_agreeing",
                             "Figure E.2b" ~ "fige2b_labels_agreeing"),
    table_figure = figure,
    claim = "Printed labels agreeing with the published figure",
    value_rewrite = n_agree,
    value_paper = as.character(n_printed),
    notes = paste0("Each label is an estimate and a standard error; a label counts as agreeing ",
                   "only if both halves do at the two decimals the figure prints")
  ),
  tribble(
    ~claim_id, ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "fig4_undisputed_republican_counterfactual", "Figure 4",
      "Undisputed accusation, Republican, CATE, Counterfactual",
      grand_cell(fig4, "Undisputed accusation", "Republican", "CATE", "Counterfactual",
                 "estimate"),
      "-1.12",
      paste0("The quantity is a mean of 72 values that comes to -1.125 exactly, which is a tie ",
             "at the second decimal. lm_robust returns it one unit in the last place away from ",
             "that tie, so it prints as -1.13; the mean of the same 72 values, which the ",
             "appendix's Table D.5 reports for the same cell, lands on the tie and prints as ",
             "-1.12, which is what the published figure shows. The deposited script produces ",
             "the same bits as the rewrite and prints -1.13 too.")
  ),
  label_misses,

  # Appendix tables
  tribble(
    ~claim_id, ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "appendix_d3_cells", "Table D.3", "Cells agreeing with the published table",
      sum(d3_cmp$ok), as.character(nrow(d3_cmp)),
      paste0("Each cell is a percentage and its standard error at one decimal. Compared with the ",
             "table's 'No change' and 'Less' headings read in the transposed order its own ",
             "Difference column implies"),
    "appendix_d3_cells_as_printed", "Table D.3",
      "Cells agreeing if the column headings are read as printed",
      sum(d3_cmp_literal$ok), as.character(nrow(d3_cmp_literal)),
      paste0("The same comparison with 'No change' taken to mean the share reporting no change. ",
             "The shortfall is the size of the transposition: in ",
             sum(d3_difference_source$from_no_change), " of the table's ",
             nrow(d3_difference_source), " rows the printed Difference equals More minus the ",
             "column headed 'No change', against ", sum(d3_difference_source$from_less),
             " for the column headed 'Less'"),
    "appendix_d3_sample_sizes", "Table D.3", "Sample sizes agreeing",
      sum(d3_cmp$n_ok) / 4, as.character(nrow(d3_cmp) / 4), "",
    "appendix_d4_cells", "Table D.4", "Cells agreeing with the published table",
      sum(d4_cmp$ok), as.character(nrow(d4_cmp)),
      paste0("Each cell is an estimate and its standard error at the two decimals the published ",
             "table prints. The deposited script formats these cells to three decimals instead"),
    "appendix_d4_p_values", "Table D.4", "p-values agreeing",
      sum(d4_cmp$p_ok, na.rm = TRUE), as.character(nrow(d4_cmp) / 3),
      "One p-value per difference in means test, printed to three decimals",
    "appendix_d4_sample_sizes", "Table D.4", "Sample sizes agreeing",
      sum(d4_cmp$n_ok) / 3, as.character(nrow(d4_cmp) / 3), "",
    "appendix_d5_estimates", "Table D.5", "Point estimates agreeing with the published table",
      sum(d5_cmp$est_ok), as.character(nrow(d5_cmp)), "",
    "appendix_d5_sample_sizes", "Table D.5", "Sample sizes agreeing",
      sum(d5_cmp$n_ok), as.character(nrow(d5_cmp)), "",
    "appendix_d5_intervals", "Table D.5",
      "Bootstrap standard errors and interval endpoints agreeing",
      sum(d5_cmp$interval_ok), as.character(nrow(d5_cmp)),
      paste0("The deposited script resamples with replicate() and sample() over every cell in the ",
             "counterfactual data; the rewrite uses rsample::bootstraps() at the same seed and the ",
             "same 10,000 draws over the twenty cells the table reports, so the two walk different ",
             "random streams. The deposited script reproduces all sixty published rows exactly; the ",
             "rewrite differs on seven of them by a unit in the second decimal, which is Monte Carlo ",
             "variation rather than a difference in the estimator"),
    "appendix_e_simultaneous_less_accurate", "Figure E.2a",
      "No-pretreatment cells of four in which the simultaneous format sits further from the experiment",
      sum(e2a_accuracy$simultaneous_gap > e2a_accuracy$cf_gap), "3",
      paste0("The article claims this for both Democratic cells and for Republicans on the Obama ",
             "torture treatment, and reports no advantage either way on Trump coal ash"),
    "appendix_e_biden_doubles", "Figure E.2b",
      "Ratio of the simultaneous to the change format share of Democrats reporting more support",
      grand_share(fige2, "Biden skeptical of Anita Hill", "Democrat", "Simultaneous", "More") /
        grand_share(fige2, "Biden skeptical of Anita Hill", "Democrat", "Change", "More"),
      "2",
      "The article says the simultaneous format doubles the share, so the ratio is recorded and no verdict is returned"
  )
)

# How each row is compared ----
# Most published values are matched at the precision the page prints. A hedged claim is
# not: it records both numbers and returns no verdict, at a precision the page does not
# carry, because rounding the evidence to the page destroys it.
comparison_overrides <- tribble(
  ~claim_id, ~comparison, ~digits,
  "cornish_democrat_overstatement", "approximate", 2L,
  "cornish_republican_understatement", "approximate", 2L,
  "appendix_e_biden_doubles", "approximate", 2L
)

gt <- gt |>
  left_join(comparison_overrides, by = "claim_id") |>
  mutate(paper_id = "graham_coppock_2021",
         comparison = replace_na(comparison, "=="),
         digits = if_else(is.na(digits), decimals(value_paper), digits))

stopifnot(nrow(gt) == nrow(distinct(gt, table_figure, claim)))
stopifnot(!anyDuplicated(na.omit(gt$claim_id)))
stopifnot(all(na.omit(gt$claim_id) %in% published_claims$claim_id))

# value_script ----
# Everything the deposited scripts and the rewrite both produce agreed to machine
# precision, checked above against archive_values.csv, so the two columns carry the same
# numbers everywhere except the one row where the bootstrap streams part company.
gt <- gt |>
  mutate(
    value_script = value_rewrite,
    match = if_else(comparison == "approximate", NA_real_,
                    agrees(value_script, value_paper, digits)),
    match_rewrite = if_else(comparison == "approximate", NA_real_,
                            agrees(value_rewrite, value_paper, digits))
  )

boot_row <- gt$claim_id == "appendix_d5_intervals"
boot_row[is.na(boot_row)] <- FALSE
stopifnot(sum(boot_row) == 1)
gt$value_script[boot_row] <- archive_d5_boot_reproduced
gt$match[boot_row] <- agrees(archive_d5_boot_reproduced, gt$value_paper[boot_row])

# A descriptive claim carries its verdict in holds, not in match ----
# It has no printed number of its own: the count beside it is this file's rendering of a
# sentence about shape, so a value comparison is the wrong instrument even where a count
# happens to be comparable. The five-case table's third state is what the column is for.
descriptive_ids <- published_claims$claim_id[published_claims$claim_type == "descriptive"]

gt <- gt |>
  mutate(
    is_descriptive = !is.na(claim_id) & claim_id %in% descriptive_ids,
    holds = case_when(!is_descriptive ~ NA,
                      comparison == "approximate" ~ NA,
                      .default = match_rewrite == 1),
    match = if_else(is_descriptive, NA_real_, match),
    match_rewrite = if_else(is_descriptive, NA_real_, match_rewrite)
  )

# Where the rewrite and the article disagree, name the locus ----
gt <- gt |>
  mutate(defect_locus = case_when(
    !is.na(match_rewrite) & match_rewrite == 0 | !is.na(holds) & !holds ~ case_when(
      claim_id == "intro_impeach_correlation" ~ "paper_internal",
      claim_id == "results_level_first_democrat" ~ "paper_internal",
      claim_id == "results_wrong_sign_cases_null" ~ "paper_internal",
      claim_id == "appendix_d3_cells_as_printed" ~ "paper_internal",
      claim_id == "appendix_d5_intervals" ~ "rewrite",
      claim_id == "fig4_undisputed_republican_counterfactual" ~ "environment",
      claim_id == "fig4_labels_agreeing" ~ "environment",
      .default = "unresolved"
    ),
    .default = NA_character_
  ))

gt$notes[!is.na(gt$claim_id) & gt$claim_id == "fig4_labels_agreeing"] <-
  paste0(gt$notes[!is.na(gt$claim_id) & gt$claim_id == "fig4_labels_agreeing"],
         ". The one label that disagrees has its own row below.")

gt <- gt |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, holds, comparison, digits, defect_locus, notes)

# Gate: every published float has coverage ----
published_float_inventory <- c(
  "Figure 1b", "Figure 2", "Figure 3a", "Figure 3b", "Figure 4", "Figure 5",
  "Figure E.2a", "Figure E.2b", "Table D.3", "Table D.4", "Table D.5"
)

float_rows <- filter(gt, str_starts(table_figure, "Table|Figure"))
uncovered_floats <- setdiff(published_float_inventory, float_rows$table_figure)
unlisted_floats <- setdiff(float_rows$table_figure, published_float_inventory)

if (length(uncovered_floats) > 0 || length(unlisted_floats) > 0) {
  stop(str_glue(
    "Published floats with no ground truth row: {str_c(uncovered_floats, collapse = ', ')}. ",
    "Ground truth rows for floats the article does not have: ",
    "{str_c(unlisted_floats, collapse = ', ')}."
  ))
}

# Gate: the second instrument ran, printed, and agrees ----
# maintained/in_text_claims.R reaches the same claimed numbers out of the same outputs by
# its own path, doing its own selection, unit conversion and rounding. It is run here
# rather than read, and what it printed is counted: a block that errors at its first line,
# or that ends in a bare expression and so prints nothing under source(), passes a scan for
# markers while checking nothing at all.
claims_output <- capture.output(source(here::here("maintained", "in_text_claims.R")))

printed_claims <-
  tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM ")) |>
  transmute(
    claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
    value_in_text = str_match(line, "^CLAIM [^ ]+ = (.*?) \\|\\| ")[, 2]
  )

stopifnot(!anyDuplicated(printed_claims$claim_id), !any(is.na(printed_claims$claim_id)),
          !any(is.na(printed_claims$value_in_text)))

needs_block <- filter(published_claims, needs_block == "TRUE")
needs_row <- filter(published_claims, claim_type %in% c("pipeline", "descriptive"))

blockless <- setdiff(needs_block$claim_id, printed_claims$claim_id)
rowless <- setdiff(needs_row$claim_id, na.omit(gt$claim_id))
invented <- setdiff(printed_claims$claim_id, needs_block$claim_id)

if (length(blockless) > 0 || length(rowless) > 0 || length(invented) > 0) {
  stop(str_glue(
    "Coverage gate failed. ",
    "Claims with no block in maintained/in_text_claims.R ({length(blockless)}): ",
    "{str_c(head(blockless, 40), collapse = ', ')}. ",
    "Claims with no ground truth row ({length(rowless)}): ",
    "{str_c(head(rowless, 40), collapse = ', ')}. ",
    "Blocks naming a claim the extraction does not require ({length(invented)}): ",
    "{str_c(head(invented, 40), collapse = ', ')}."
  ))
}

stopifnot(nrow(printed_claims) == nrow(needs_block))

# The two instruments must land on the same number. in_text_claims.R prints in the
# article's units and rounding and never sees this file, so a disagreement is one of the
# two being wrong and it stops the build.
instrument_disagreements <-
  gt |>
  drop_na(claim_id) |>
  inner_join(printed_claims, by = "claim_id") |>
  filter(!is.na(value_rewrite),
         format_to_page(value_rewrite, value_paper, digits) != value_in_text)

if (nrow(instrument_disagreements) > 0) {
  print(select(instrument_disagreements, claim_id, value_paper, value_rewrite, value_in_text),
        n = 40)
  stop(str_glue(
    "The ground truth and maintained/in_text_claims.R disagree on ",
    "{nrow(instrument_disagreements)} claims."
  ))
}

# Gate: a verdict and a locus go together ----
# A failing verdict without a locus reads as a failure of the rewrite, which it almost
# never is. A locus without a failing verdict is a verdict nothing supports.
failing <- (!is.na(gt$match_rewrite) & gt$match_rewrite == 0) | (!is.na(gt$holds) & !gt$holds)
locus_gate <- gt[xor(failing, !is.na(gt$defect_locus)), ]

if (nrow(locus_gate) > 0) {
  print(select(locus_gate, claim_id, table_figure, claim, match_rewrite, holds, defect_locus),
        n = 40)
  stop("Rows carrying a failing verdict with no locus, or a locus with no failing verdict.")
}

# Gate: every claim id the errata cites exists ----
# errata.qmd names, for each published entry, the ground truth rows that entry corrects. An
# id that no longer exists is a typo or a renamed claim, and a dangling reference in a
# document whose whole purpose is correcting the record is worse than a build that refuses.
errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_ids <- read_csv(errata_path, show_col_types = FALSE) |>
    pull(claim_ids) |>
    str_split(";") |>
    unlist() |>
    str_trim()
  errata_ids <- errata_ids[!is.na(errata_ids) & errata_ids != ""]
  dangling_errata_ids <- setdiff(errata_ids, gt$claim_id)
  if (length(dangling_errata_ids) > 0) {
    stop("errata_entries.csv lists claim ids absent from the ground truth: ",
         paste(dangling_errata_ids, collapse = ", "))
  }
  print(str_glue("Errata spine: {length(unique(errata_ids))} distinct claim ids listed, ",
                 "all present in the ground truth."))
}

write_csv(gt, here::here("ground_truth", "graham_coppock_2021_ground_truth.csv"))

print(gt |> select(claim_id, table_figure, claim, value_paper, value_rewrite,
                   match_rewrite, holds, defect_locus),
      n = nrow(gt), width = 200)
print(tibble(
  rows = nrow(gt),
  claims_extracted = nrow(published_claims),
  blocks_printed = nrow(printed_claims),
  match_1 = sum(gt$match == 1, na.rm = TRUE),
  match_0 = sum(gt$match == 0, na.rm = TRUE),
  rewrite_1 = sum(gt$match_rewrite == 1, na.rm = TRUE),
  rewrite_0 = sum(gt$match_rewrite == 0, na.rm = TRUE),
  holds_true = sum(gt$holds, na.rm = TRUE),
  holds_false = sum(!gt$holds, na.rm = TRUE),
  no_verdict = sum(is.na(gt$match_rewrite) & is.na(gt$holds))
))
