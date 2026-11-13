# graham_coppock_2021/maintained/in_text_claims.R
# Output: printed CLAIM lines and a CLAIM SUMMARY line; no file
# Depends on: helpers.R, excheckr, errata_entries.csv, output/text_intext_stats.csv, output/figure_1_impeach_counterfactual.csv,
#   output/figure_2_cornish_example.csv, output/figure_3a_reporting_any_change.csv,
#   output/figure_3b_self_reported_means.csv, output/figure_3b_level_question_effects.csv,
#   output/figure_4_grand_comparison.csv, output/figure_5_pretreat_comparison.csv,
#   output/figure_e2_study2b_simultaneous.csv, output/table_d3_change_distribution_estimates.csv,
#   output/table_d4_counterfactual_accuracy_estimates.csv,
#   output/table_d5_ate_vs_self_estimates.csv, output/text_sign_comparison.csv,
#   output/data_study1-3_clean.rds, output/data_impeach_clean.rds,
#   ground_truth/published_claims.csv and the four ground_truth/published_*.csv
#   transcriptions of the published tables and figure labels
# Description: Every number the article and its supplementary materials print, beside the
#   sentence that prints it, in the order a reader meets them.
#
#   This file recomputes. It reads the same maintained/output/ files the ground truth
#   reads and does its own selection, unit conversion and rounding, so the two instruments
#   arrive at each number by separate paths and a disagreement between them is a finding
#   rather than a coincidence. ground_truth/build_ground_truth.R runs this file
#   non-interactively, counts the CLAIM lines it printed, and stops if any of them
#   disagrees with its own value. It never reads the ground truth itself, and it never
#   refits: estimation happens once, in the analysis scripts, and only derivation happens
#   twice. Where a claim is about how many published cells reproduce, the published side
#   comes from the transcriptions beside build_ground_truth.R, which are the extraction
#   rather than the comparison, and the cells are matched here by a composite key rather
#   than by that script's joins.
#
#   Every claim prints one line, CLAIM <id> = <value> || [verdict] <label>, through
#   excheckr's claim(). That printed id is the only link between a block and the claim it
#   covers and it is load bearing: the gate reads this file as a program and matches on
#   what it printed. The "# covers:" comments are for a reader and nothing reads them,
#   because a comment is a second copy of the link that can go stale independently of the
#   code beside it.
#
#   Every verdict is asserted. [match] and [corrected] are the article's number or the
#   erratum's; [hedged] is a descriptive claim that does not agree, scored as holds in the
#   ground truth. Two blocks declare their expectation at the call site: the Table D.3
#   count with the headings read as printed is [derived], because the article prints no
#   such count and the block exists to show the headings are transposed, and the Table D.5
#   interval count is [range], because it depends on which bootstrap stream drew it.
#   assert_claims() stops the run on any MISMATCH, STALE, DRIFT, or MISSING, and
#   claim_summary() prints the CLAIM SUMMARY line last.

source(here::here("maintained", "helpers.R"))
library(excheckr)

options(width = 200)

out <- function(f) read_csv(here::here(out_dir, f), show_col_types = FALSE)
pub <- function(f) read_csv(here::here("ground_truth", f),
                            col_types = cols(.default = col_character()))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
impeach <- read_rds(here::here(out_dir, "data_impeach_clean.rds"))

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

published_claims <- pub("published_claims.csv")
d3_published <- pub("published_table_d3.csv")

# Reporting ----
# The scoring comes from excheckr. claim_start() takes the extraction and the errata spine
# and derives each claim's expectation from them: published_claims.csv says what the
# article prints, and a quantity entry in errata_entries.csv says which of those a
# published note corrects and to what. Both are read with every column as character, so
# the precision the page prints survives (0.80 and 0.8 are the same double). format = "id"
# prints the CLAIM <id> = <value> || [verdict] <label> line build_ground_truth.R parses.
errata <- read_csv(here::here("errata_entries.csv"),
                   col_types = cols(.default = col_character()))

claim_start(published = published_claims, errata = errata, format = "id")

# Entries 3 and 4 correct a count but are filed as definitions, so claim_start() does not
# register them as corrections. The corrected count is read from the marked span of the
# entry's own corrected sentence instead, where it is a numeral or a spelled number.
spelled_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                     "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                     "seventeen", "eighteen", "nineteen", "twenty")

span_value <- function(id, pattern) {
  corrected <- errata$corrected[map_lgl(str_split(errata$claim_ids, ";"), \(ids) id %in% ids)]
  stopifnot(length(corrected) == 1)
  token <- str_match(corrected, pattern)[, 2]
  stopifnot(!is.na(token))
  if (token %in% spelled_numbers) as.character(match(token, spelled_numbers)) else token
}

# Accessors ----
# Each stops unless the filter selects exactly one row, so a claim cannot quietly read a
# row that is not there and print a plausible number from the wrong place.

norm <- function(x) str_squish(str_replace_all(x, "\n", " "))

# The sign comparison carries the deposit's lowercase topic key and the figures carry the
# display label, so one is translated into the other rather than typed out as a lookup.
topic_label <- function(key) {
  labels <- unique(norm(grand$Topic[grand$topic == key]))
  stopifnot(length(labels) == 1)
  labels
}

stat <- function(which_stat) {
  value <- txt$value[txt$stat == which_stat]
  stopifnot(length(value) == 1)
  value
}

# One row of a grand comparison panel: Figures 2, 4, 5 and E.2 all carry the same columns.
panel_row <- function(panel, the_topic, the_party, the_estimator, the_format) {
  rows <- panel[norm(panel$Topic) == the_topic & panel$Party == the_party &
                  panel$Estimator == the_estimator & panel$Format == the_format &
                  (is.na(panel$category) | panel$category == "Diff"), ]
  stopifnot(nrow(rows) == 1)
  rows
}

# A stacked bar share from the same panels, which is a category row rather than a Diff row.
panel_share <- function(panel, the_topic, the_party, the_format, the_category) {
  rows <- panel[norm(panel$Topic) == the_topic & panel$Party == the_party &
                  panel$Format == the_format & !is.na(panel$category) &
                  panel$category == the_category, ]
  stopifnot(nrow(rows) == 1)
  rows$share
}

fig3a_pooled <- function(the_party, column) {
  rows <- fig3a[fig3a$Topic == "Pooled" & fig3a$Party == the_party, ]
  stopifnot(nrow(rows) == 1)
  rows[[column]]
}

level_effect <- function(the_topic, the_party, column) {
  rows <- fig3b_eff[norm(fig3b_eff$Topic) == the_topic & fig3b_eff$Party == the_party, ]
  stopifnot(nrow(rows) == 1)
  rows[[column]]
}

# A value printed to the precision the page uses. A quantity that rounds to zero from below
# prints as "-0.00" and no page prints that, so the sign goes when nothing but zeros remain.
printed_to_page <- function(value, digits) {
  text <- sprintf("%.*f", digits, value)
  if_else(str_detect(text, "^-0[.]?0*$"), str_remove(text, "^-"), text)
}

# Published cells against the rewrite's own, matched on a composite key rather than joined,
# so this file and build_ground_truth.R do not share a derivation.
positions <- function(published_key, rewrite_key) {
  stopifnot(!anyDuplicated(published_key), !anyDuplicated(rewrite_key))
  position <- match(published_key, rewrite_key)
  stopifnot(!anyNA(position))
  position
}

# A lone published number: a sample size, a p-value.
values_agreeing <- function(published_key, published_value, digits,
                            rewrite_key, rewrite_value) {
  position <- positions(published_key, rewrite_key)
  sum(printed_to_page(rewrite_value[position], digits) == published_value)
}

# Everywhere else the page prints an estimate and its standard error in one cell, so the
# cell agrees only when both halves do.
cells_agreeing <- function(published_key, published_value, published_error, digits,
                           rewrite_key, rewrite_value, rewrite_error) {
  position <- positions(published_key, rewrite_key)
  sum(printed_to_page(rewrite_value[position], digits) == published_value &
        printed_to_page(rewrite_error[position], digits) == published_error)
}

# The four figures that print an estimate and a standard error beside every row they draw.
# A label agrees only if both halves do at the two decimals the figures print, after the
# 10-digit snap the pipeline applies before printing.
labels_rewrite <-
  bind_rows(
    fig2 |> mutate(figure = "Figure 2", study = as.character(study)),
    fig4 |> mutate(figure = "Figure 4", study = as.character(study)),
    fig5 |> mutate(figure = "Figure 5", study = as.character(study)),
    fige2 |> mutate(figure = if_else(str_detect(panel, "^\\(a\\)"),
                                     "Figure E.2a", "Figure E.2b"),
                    study = as.character(study))
  ) |>
  filter(!is.na(label)) |>
  transmute(key = str_c(figure, "|", study, "|", norm(Topic), "|", Party, "|",
                        Estimator, "|", Format),
            figure, estimate = round(estimate, 10), std.error = round(std.error, 10))

labels_published <-
  pub("published_figure_labels.csv") |>
  transmute(key = str_c(figure, "|", study, "|", norm(Topic), "|", Party, "|",
                        Estimator, "|", Format),
            figure, estimate, se)

labels_agreeing <- function(which_figure) {
  published <- labels_published[labels_published$figure == which_figure, ]
  rewritten <- labels_rewrite[labels_rewrite$figure == which_figure, ]
  stopifnot(nrow(published) > 0, nrow(published) == nrow(rewritten))
  cells_agreeing(published$key, published$estimate, published$se, 2L,
                 rewritten$key, rewritten$estimate, rewritten$std.error)
}

# Abstract ----

# "Using a series of experiments embedded in four studies, we show that the counterfactual
# format greatly reduces bias relative to the change format."
# covers: abstract_studies
claim("abstract_studies", n_distinct(clean$all$study),
      "distinct studies in the deposited data")
claim_evidence(str_c("studies: ", str_c(sort(unique(clean$all$study)), collapse = ", ")))

# Introduction ----

# "We obtained 4,034 survey responses (field dates: November 21 through December 10, 2019;
# cooperation rate: 97.7 percent) from Lucid, which quota samples online survey respondents
# to match U.S. Census demographic margins."
# covers: intro_impeach_n
claim("intro_impeach_n", stat("N impeachment survey"),
      "impeachment survey responses with both questions answered")

# "It then asked subjects for their level of support for impeachment: 'How strongly do you
# oppose or support the impeachment of Donald Trump? [1: Strongly oppose; 7: Strongly
# support]'"
# covers: intro_impeach_scale_low, intro_impeach_scale_high
claim("intro_impeach_scale_low", min(impeach$impeach_Y1, na.rm = TRUE),
      "lowest impeachment support value in the deposited data")
claim("intro_impeach_scale_high", max(impeach$impeach_Y1, na.rm = TRUE),
      "highest impeachment support value in the deposited data")

# "Panel (a) offers prima facie evidence that subjects are not bewildered by the
# nonstandard question format: the strong correlation (0.82) of the two responses indicates
# that most subjects believe their counterfactual attitudes would have been very close to
# their actual attitudes (72 percent report exactly zero change)."
# covers: intro_impeach_correlation, intro_impeach_zero_change
claim("intro_impeach_correlation", stat("cor(Y1, Y0) impeachment"),
      "correlation of the impeachment level question with the counterfactual guess")
claim_evidence(str_c("at four decimals: ", sprintf("%.4f", stat("cor(Y1, Y0) impeachment"))))
claim("intro_impeach_zero_change", 100 * stat("Share reporting exactly zero change"),
      "per cent reporting exactly zero change")

# "The average difference between the first and second question is 0.16 (SE = 0.02)."
# covers: intro_impeach_mean_tau, intro_impeach_mean_tau_se
claim("intro_impeach_mean_tau", stat("Mean tau_i impeachment"),
      "average of the individual differences, impeachment survey")
claim("intro_impeach_mean_tau_se", stat("SE of mean tau_i impeachment"),
      "standard error of that average")

# Figure 1, panel (b): "The right panel plots the average change by partisan group." Seven
# means are printed beside the points, one per seven-point party identification group.
# covers: fig1b_pid1 ... fig1b_pid7
walk(seq_len(nrow(fig1)), function(i) {
  claim(str_c("fig1b_pid", fig1$pid_7[i]), fig1$estimate[i],
        str_c("Figure 1b, average change, party identification group ", fig1$pid_7[i]))
})

# "Panel (b) shows the average difference across partisan groups. For all groups except
# strong Republicans, the estimated effect of the Ukraine revelations on impeachment
# support is small, positive, and statistically significant."
# covers: intro_impeach_all_but_strong_republicans
positive_significant <- fig1$estimate > 0 & fig1$conf.low > 0
claim("intro_impeach_all_but_strong_republicans", sum(positive_significant),
      "partisan groups of seven whose average change is positive with a confidence interval excluding zero")
claim_evidence(str_c("group estimates [95% CI]: ",
                     str_c(sprintf("%d: %.3f [%.3f, %.3f]", fig1$pid_7, fig1$estimate,
                                   fig1$conf.low, fig1$conf.high), collapse = "; ")))

# Detailed Example ----

# "A huge majority of Democratic respondents (87 percent) report that the accusation made
# them less likely to vote for Cornish. By contrast, most Republicans (57 percent) reported
# that the information had no effect."
# covers: cornish_democrat_less_share, cornish_republican_same_share
claim("cornish_democrat_less_share",
      100 * panel_share(fig2, "Disputed accusation", "Democrat", "Change", "Less"),
      "per cent of Democrats reporting the accusation made them less likely to support Cornish")
claim("cornish_republican_same_share",
      100 * panel_share(fig2, "Disputed accusation", "Republican", "Change", "Same"),
      "per cent of Republicans reporting no change")

# "Recoding the change question from -1 to 1, we see that Democrats score -0.85 on this
# measure, but answering the level question first increases their score to -0.61, for an
# increase of 0.24 (SE: 0.10). By contrast, the level question decreases this measure among
# Republicans (-0.24 points, SE: 0.14)."
# covers: cornish_democrat_change_score, cornish_democrat_change_level_score,
#   cornish_democrat_level_effect, cornish_democrat_level_effect_se,
#   cornish_republican_level_effect, cornish_republican_level_effect_se
claim("cornish_democrat_change_score",
      panel_row(fig2, "Disputed accusation", "Democrat", "More-less", "Change")$estimate,
      "Democrats' more-minus-less score, change format")
claim("cornish_democrat_change_level_score",
      panel_row(fig2, "Disputed accusation", "Democrat", "More-less", "Change + level")$estimate,
      "Democrats' more-minus-less score with the level question asked first")
claim("cornish_democrat_level_effect",
      level_effect("Disputed accusation", "Democrat", "estimate"),
      "effect of asking the level question first on Democrats' more-minus-less score")
claim("cornish_democrat_level_effect_se",
      level_effect("Disputed accusation", "Democrat", "std.error"),
      "standard error of that effect")
claim("cornish_republican_level_effect",
      level_effect("Disputed accusation", "Republican", "estimate"),
      "the same effect among Republicans")
claim("cornish_republican_level_effect_se",
      level_effect("Disputed accusation", "Republican", "std.error"),
      "standard error of that effect")

# "According to the unbiased difference-in-means estimate that uses first-stage responses
# only, the information had a very small average effect among Democrats (0.02 scale points,
# SE: 0.32, 7-point scale) and a large negative average effect among Republicans (-2.01
# points, SE: 0.33). Directly contrary to the implications of the change format, the
# experimental estimate shows that Cornish suffered a heavy loss of support among
# Republicans, not Democrats, as a consequence of the allegations."
# covers: cornish_democrat_dim, cornish_democrat_dim_se, cornish_outcome_scale_points,
#   cornish_republican_dim, cornish_republican_dim_se, cornish_experiment_direction
cornish_dim_democrat <- panel_row(fig2, "Disputed accusation", "Democrat",
                                  "CATE", "Diff. in means")
cornish_dim_republican <- panel_row(fig2, "Disputed accusation", "Republican",
                                    "CATE", "Diff. in means")

claim("cornish_democrat_dim", cornish_dim_democrat$estimate,
      "difference in means, Democrats, disputed accusation")
claim("cornish_democrat_dim_se", cornish_dim_democrat$std.error,
      "standard error of that difference in means")

cornish_outcome <- clean$all$Y[clean$all$study == "1" &
                                 clean$all$topic == "Disputed accusation"]
claim("cornish_outcome_scale_points",
      diff(range(cornish_outcome, na.rm = TRUE)) + 1,
      "points on the Cornish support scale in the deposited data")

claim("cornish_republican_dim", cornish_dim_republican$estimate,
      "difference in means, Republicans, disputed accusation")
claim("cornish_republican_dim_se", cornish_dim_republican$std.error,
      "standard error of that difference in means")

claim("cornish_experiment_direction",
      sum(c(cornish_dim_democrat$conf.high, cornish_dim_republican$conf.high) < 0),
      "parties of two whose experimental estimate is negative with an interval excluding zero")
claim_evidence(str_c("Democrat ", sprintf("%.2f [%.2f, %.2f]", cornish_dim_democrat$estimate,
                                          cornish_dim_democrat$conf.low, cornish_dim_democrat$conf.high),
                     "; Republican ", sprintf("%.2f [%.2f, %.2f]", cornish_dim_republican$estimate,
                                              cornish_dim_republican$conf.low,
                                              cornish_dim_republican$conf.high)))

# "Among Democrats, the counterfactual format estimate that uses responses from both stages
# is -0.49 scale points (SE: 0.11) and the corresponding figure for Republicans is -1.07
# points (SE: 0.17)."
# covers: cornish_democrat_counterfactual, cornish_democrat_counterfactual_se,
#   cornish_republican_counterfactual, cornish_republican_counterfactual_se
cornish_cf_democrat <- panel_row(fig2, "Disputed accusation", "Democrat",
                                 "CATE", "Counterfactual")
cornish_cf_republican <- panel_row(fig2, "Disputed accusation", "Republican",
                                   "CATE", "Counterfactual")

claim("cornish_democrat_counterfactual", cornish_cf_democrat$estimate,
      "counterfactual format estimate, Democrats, disputed accusation")
claim("cornish_democrat_counterfactual_se", cornish_cf_democrat$std.error,
      "standard error of that estimate")
claim("cornish_republican_counterfactual", cornish_cf_republican$estimate,
      "counterfactual format estimate, Republicans, disputed accusation")
claim("cornish_republican_counterfactual_se", cornish_cf_republican$std.error,
      "standard error of that estimate")

# "As demonstrated in the bottom two rows of figure 2, the counterfactual ATE estimates are
# biased away from the experimental benchmarks: Democrats overstate negative change by
# approximately half a point and Republicans understate it by almost a full point."
# covers: cornish_democrat_overstatement, cornish_republican_understatement
claim("cornish_democrat_overstatement",
      abs(cornish_cf_democrat$estimate - cornish_dim_democrat$estimate),
      "Democrats: counterfactual estimate minus the experimental benchmark, in scale points",
      digits = 2)
claim("cornish_republican_understatement",
      abs(cornish_cf_republican$estimate - cornish_dim_republican$estimate),
      "Republicans: counterfactual estimate minus the experimental benchmark, in scale points",
      digits = 2)

# Research Design ----

# "For a broader look at the performance of the two question formats, the full research
# design evaluated 11 total information treatments using the strategies described just above
# in the detailed example. Study 1 conducted the 'reducing response substitution'
# experiments using eight information treatments. Study 3 applied this strategy to one
# additional treatment. Using the same eight treatments, Study 1 also evaluated the
# counterfactual format using the same strategies described above. Study 2 applied these
# strategies to two additional treatments."
# covers: design_treatments_total, design_study1_treatments, design_study3_treatments,
#   design_study2_treatments
evaluated_topics <- unique(norm(fig4$Topic))
pretreated_topics <- unique(norm(fig5$Topic))
study3_topics <- setdiff(unique(clean$all$topic[clean$all$study == "3"]), NA)

claim("design_treatments_total", length(evaluated_topics) + length(study3_topics),
      "information treatments in the evaluation: the ten of Figure 4 plus Study 3's")
claim("design_study1_treatments", n_distinct(fig4$Topic[fig4$study == "1"]),
      "Study 1 information treatments")
claim("design_study3_treatments", length(study3_topics),
      "Study 3 information treatments")
claim("design_study2_treatments", n_distinct(fig4$Topic[fig4$study == "2a"]),
      "Study 2a treatments carried into the Figure 4 evaluation")

# "Study 2a was conducted October 19-31, 2018 (N = 2,475, cooperation rate = 97.2 percent),
# and tests two of the treatments used in our evaluation (table 2), plus four additional
# treatments that are used to demonstrate the nonrandomized counterfactual format."
# covers: design_study2a_extra_treatments
claim("design_study2a_extra_treatments", length(pretreated_topics),
      "Study 2a treatments demonstrating the nonrandomized format")
claim_evidence(str_c("pretreated treatments: ", str_c(pretreated_topics, collapse = "; ")))

# "The last set of results applies the nonrandomized counterfactual format to four cases in
# which we suspect pretreatment."
# covers: design_pretreated_cases
claim("design_pretreated_cases", length(pretreated_topics),
      "cases in which pretreatment is suspected, drawn as Figure 5")

# "The empirical analysis is based on four total surveys."
# covers: design_surveys
claim("design_surveys", n_distinct(clean$all$study),
      "surveys behind the analysis of Studies 1 to 3")

# "Study 1 was conducted May 8-9, 2018 (N = 417, cooperation rate = 97.0 percent), and
# included eight of the information treatments described in table 2."
# covers: design_study1_n
claim("design_study1_n", stat("N study 1 (format conditions)"),
      "Study 1 respondents in a question format condition")

# "Study 2a was conducted October 19-31, 2018 (N = 2,475, cooperation rate = 97.2 percent)."
# covers: design_study2a_n
claim("design_study2a_n", stat("N study 2a (format conditions)"),
      "Study 2a respondents in a question format condition")

# Table 2's level questions run on two scales: Study 1's seven-point support scales
# ("[1: Nearly zero; 7: Nearly certain]", "[1: Strongly oppose; 7: Strongly support]") and
# Study 2a's six-point ones ("[1: Definitely oppose; 6: Definitely support]").
# covers: table2_study1_scale_low, table2_study1_scale_high, table2_study2a_scale_low,
#   table2_study2a_scale_high
study1_outcome <- clean$all$Y[clean$all$study == "1"]
study2a_outcome <- clean$all$Y[clean$all$study == "2a"]

claim("table2_study1_scale_low", min(study1_outcome, na.rm = TRUE),
      "lowest Study 1 outcome value in the deposited data")
claim("table2_study1_scale_high", max(study1_outcome, na.rm = TRUE),
      "highest Study 1 outcome value in the deposited data")
claim("table2_study2a_scale_low", min(study2a_outcome, na.rm = TRUE),
      "lowest Study 2a outcome value in the deposited data")
claim("table2_study2a_scale_high", max(study2a_outcome, na.rm = TRUE),
      "highest Study 2a outcome value in the deposited data")

# "Study 2b (N = 1,110, cooperation rate = 96.8 percent), which was conducted November
# 20-December 7, 2018, replicated four of Study 2a's treatments using binary outcomes rather
# than Likert scales."
# covers: design_study2b_n, design_study2b_replications
claim("design_study2b_n", stat("N study 2b (format conditions)"),
      "Study 2b respondents in a question format condition")
claim("design_study2b_replications", n_distinct(clean$all$topic[clean$all$study == "2b"]),
      "Study 2a treatments replicated in Study 2b")

# "On May 28, 2019, the day after special counsel Robert Mueller made his first public
# comments about the investigation into Russian interference in the 2016 election, we
# included change questions in an otherwise unrelated survey conducted on Amazon Mechanical
# Turk (N = 1,074, cooperation rate = 99.7 percent)."
# covers: design_study3_n
claim("design_study3_n", stat("N study 3 (change responses)"),
      "Study 3 answers to the change question in the deposited extract")
claim_evidence(str_c("the Study 3 extract carries ", stat("N study 3 (rows in the deposit)"),
                     " rows against ", stat("N study 3 (distinct id values in the deposit)"),
                     " distinct id values, so the id column is not a key; the published Table D.3's ",
                     "six Study 3 sample sizes sum to the same ",
                     sum(as.numeric(d3_published$N[d3_published$study == "3"]))))

# Results ----

# "As shown in figure 3a, asking the level question first greatly reduces self-reports of
# attitude change. Overall, the effect is a 10 percentage point decrease in reporting any
# change (SE: 2 points). The effect is larger among Democrats (14 points, SE: 3 points) and
# Republicans (8 points, SE: 4 points) than among pure independents (2 points, SE: 6
# points)."
# covers: results_level_first_*
claim("results_level_first_overall", abs(100 * stat("Fig 3a overall estimate")),
      "pooled decrease in reporting any change, all respondents, points")
claim("results_level_first_overall_se", 100 * stat("Fig 3a overall SE"),
      "standard error of that decrease, points")
claim("results_level_first_democrat", abs(100 * stat("Fig 3a Democrat estimate")),
      "the same decrease among Democrats, points")
claim_evidence(str_c("at one decimal: ", sprintf("%.1f", abs(100 * stat("Fig 3a Democrat estimate")))))
claim("results_level_first_democrat_se", 100 * stat("Fig 3a Democrat SE"),
      "standard error among Democrats, points")
claim("results_level_first_republican", abs(100 * stat("Fig 3a Republican estimate")),
      "the same decrease among Republicans, points")
claim("results_level_first_republican_se", 100 * stat("Fig 3a Republican SE"),
      "standard error among Republicans, points")
claim("results_level_first_independent", abs(100 * stat("Fig 3a Independent estimate")),
      "the same decrease among pure independents, points")
claim("results_level_first_independent_se", 100 * stat("Fig 3a Independent SE"),
      "standard error among independents, points")

# The same sentence asserts an ordering, which a value by value check cannot see.
# covers: results_level_first_ordering
independent_effect <- abs(fig3a_pooled("Independent", "estimate"))
claim("results_level_first_ordering",
      sum(abs(c(fig3a_pooled("Democrat", "estimate"),
                fig3a_pooled("Republican", "estimate"))) > independent_effect),
      "of the two party groups whose decrease exceeds the independents'")

# "For example, in the Endorsed Trump facet, asking the level question first reduced
# Republican claims that their support of Kevin C. Kelly, a moderate Republican, increased
# because he endorsed Donald Trump. Similarly, in the Muller comments facet, Democrats
# became less likely to claim that special counsel Robert Mueller's comments made them
# believe Donald Trump had personally colluded with Russian agents."
# covers: results_endorsed_trump_republican, results_mueller_democrat
claim("results_endorsed_trump_republican",
      sign(level_effect("Endorsed Trump", "Republican", "estimate")),
      "sign of the level question's effect on the more-minus-less score, Endorsed Trump, Republicans")
claim_evidence(str_c("estimate ", sprintf("%.3f [%.3f, %.3f]",
                                          level_effect("Endorsed Trump", "Republican", "estimate"),
                                          level_effect("Endorsed Trump", "Republican", "conf.low"),
                                          level_effect("Endorsed Trump", "Republican", "conf.high"))))
claim("results_mueller_democrat",
      sign(level_effect("Mueller comments", "Democrat", "estimate")),
      "sign of the same effect, Mueller comments, Democrats")
claim_evidence(str_c("estimate ", sprintf("%.3f [%.3f, %.3f]",
                                          level_effect("Mueller comments", "Democrat", "estimate"),
                                          level_effect("Mueller comments", "Democrat", "conf.low"),
                                          level_effect("Mueller comments", "Democrat", "conf.high"))))

# "This section evaluates the randomized counterfactual format's performance in estimating
# the effects of ten information treatments to which subjects had likely not been
# pre-exposed."
# covers: results_treatments_evaluated
claim("results_treatments_evaluated", length(evaluated_topics),
      "treatments drawn in Figure 4")

# "The more-minus-less estimates derived from the change format have the opposite sign as
# the experimental difference-in-means estimates in 12 out of 20 opportunities, compared
# with 3 of 20 for the counterfactual format. In all three of those cases, neither the
# counterfactual estimate nor the difference in means estimate can be distinguished from
# zero."
# covers: results_change_wrong_sign, results_sign_opportunities,
#   results_counterfactual_wrong_sign, results_wrong_sign_cases_null
claim("results_change_wrong_sign", sum(signs$change_wrong_sign),
      "change format cells whose sign is opposite the difference in means")
claim("results_sign_opportunities", nrow(signs),
      "cells compared, ten treatments by two parties")
claim("results_counterfactual_wrong_sign", sum(signs$counterfactual_wrong_sign),
      "counterfactual format cells whose sign is opposite the difference in means",
      corrected = span_value("results_counterfactual_wrong_sign",
                             "\\[\\[([0-9]+) of 20\\]\\]"))

# An estimate of zero has no sign, so text_sign_comparison.R excludes it rather than
# counting it as opposite. The article's own three include one such cell, which is why its
# count is 3 where this one is 2. The exclusion is printed beside the count it changes so
# that no reader has to infer it from the difference.
zero_sign_cells <- signs[!signs$counterfactual_sign_determinate, ]
walk(seq_len(nrow(zero_sign_cells)), function(i) {
  row <- zero_sign_cells[i, ]
  claim_evidence(str_c("counterfactual estimate is zero, so it has no sign to be opposite: ",
                       topic_label(row$topic), ", ", row$Party,
                       ": counterfactual ", sprintf("%.2e", row$cate_counterfactual),
                       ", difference in means ", sprintf("%.2f", row$benchmark)))
})

wrong_sign_cells <- signs[signs$counterfactual_wrong_sign, ]
wrong_sign_estimates <-
  pmap_dfr(list(map_chr(wrong_sign_cells$topic, topic_label), wrong_sign_cells$Party),
           function(the_topic, the_party) {
    counterfactual <- panel_row(fig4, the_topic, the_party, "CATE", "Counterfactual")
    benchmark <- panel_row(fig4, the_topic, the_party, "CATE", "Diff. in means")
    tibble(topic = the_topic, party = the_party,
           cf = counterfactual$estimate, cf_low = counterfactual$conf.low,
           cf_high = counterfactual$conf.high,
           dim = benchmark$estimate, dim_low = benchmark$conf.low,
           dim_high = benchmark$conf.high)
  })

claim("results_wrong_sign_cases_null",
      sum(wrong_sign_estimates$cf_low <= 0 & wrong_sign_estimates$cf_high >= 0 &
            wrong_sign_estimates$dim_low <= 0 & wrong_sign_estimates$dim_high >= 0),
      str_glue("of the {nrow(wrong_sign_estimates)} sign-miss cells in which neither ",
               "estimate is distinguishable from zero"),
      corrected = span_value("results_wrong_sign_cases_null",
                             "\\[\\[In ([a-z]+) of those"))
walk(seq_len(nrow(wrong_sign_estimates)), function(i) {
  row <- wrong_sign_estimates[i, ]
  claim_evidence(str_c(row$topic, ", ", row$party,
                       ": counterfactual ", sprintf("%.2f [%.2f, %.2f]", row$cf, row$cf_low, row$cf_high),
                       ", difference in means ", sprintf("%.2f [%.2f, %.2f]", row$dim, row$dim_low, row$dim_high)))
})

# "In the change format, Democrats overwhelmingly report that the information made them less
# supportive of the tax cuts; by an even larger margin, Republicans report the opposite.
# This pattern is wholly contradicted by the experiment, which indicates small,
# nonsignificant effects in both parties."
# covers: results_tcja_experiment_null
tcja_dim <- map(c("Democrat", "Republican"),
                function(p) panel_row(fig4, "Tax Cuts and Jobs Act", p, "CATE", "Diff. in means"))
claim("results_tcja_experiment_null",
      sum(map_dbl(tcja_dim, "conf.low") <= 0 & map_dbl(tcja_dim, "conf.high") >= 0),
      "Tax Cuts and Jobs Act parties of two whose experimental estimate contains zero")
claim_evidence(str_c("change format more-minus-less: Democrat ",
                     sprintf("%.2f", panel_row(fig4, "Tax Cuts and Jobs Act", "Democrat",
                                               "More-less", "Change")$estimate),
                     ", Republican ",
                     sprintf("%.2f", panel_row(fig4, "Tax Cuts and Jobs Act", "Republican",
                                               "More-less", "Change")$estimate),
                     "; experiment: Democrat ",
                     sprintf("%.2f [%.2f, %.2f]", tcja_dim[[1]]$estimate, tcja_dim[[1]]$conf.low,
                             tcja_dim[[1]]$conf.high),
                     ", Republican ",
                     sprintf("%.2f [%.2f, %.2f]", tcja_dim[[2]]$estimate, tcja_dim[[2]]$conf.low,
                             tcja_dim[[2]]$conf.high)))

# "All told, this collection of tests yields 20 opportunities to compare the
# difference-in-means estimate to the counterfactual guess of the ATE (10 experiments x 2
# parties). The difference between the two estimates was statistically significant in six
# cases (30 percent)."
# covers: results_ate_opportunities, results_parties, results_significant_differences,
#   results_significant_share
d5_difference <- d5[d5$Estimator == "Difference", ]
d5_excludes_zero <- sign(d5_difference$conf.low) == sign(d5_difference$conf.high)

claim("results_ate_opportunities", nrow(d5_difference),
      "difference rows in Table D.5, one per treatment and party")
claim("results_parties", n_distinct(d5_difference$Party),
      "parties the analysis splits on")
claim("results_significant_differences", sum(d5_excludes_zero),
      "differences whose bootstrap interval excludes zero")
claim("results_significant_share", 100 * mean(d5_excludes_zero),
      "the same count as a percentage of the twenty comparisons")

# "Separating the treatment and control outcomes gives us 40 additional opportunities to
# evaluate subjects' performance (10 experiments x 2 parties x 2 potential outcomes). Of
# these, difference-in-means tests reject the null hypothesis of no difference in 12 cases
# (30.0 percent)."
# covers: results_potential_outcomes, results_outcome_opportunities,
#   results_dim_rejections, results_dim_rejection_share
d4_fig4 <- d4[d4$quantity == "Difference" &
                (d4$Study == "1" | (d4$Study == "2a" & str_detect(d4$Topic, "Obama|Trump"))), ]

claim("results_potential_outcomes", n_distinct(d4_fig4$Outcome),
      "potential outcomes compared per treatment and party")
claim("results_outcome_opportunities", nrow(d4_fig4),
      "opportunities to evaluate a counterfactual guess")
claim("results_dim_rejections", sum(d4_fig4$p.value < 0.05),
      "difference in means tests rejecting at p < 0.05")
claim("results_dim_rejection_share", 100 * mean(d4_fig4$p.value < 0.05),
      "the same count as a percentage of the forty tests")

# Figures ----

# Figure 4 caption: "This figure displays the full set of estimates for 10 treatments in
# studies 1 and 2."
# covers: fig4_caption_treatments
claim("fig4_caption_treatments", length(evaluated_topics),
      "treatments in Figure 4, counted from the panel's own data")

# Figures 2, 4, 5 and E.2 print an estimate and a standard error beside every row they draw.
# covers: fig2_labels_agreeing, fig4_labels_agreeing, fig5_labels_agreeing,
#   fige2a_labels_agreeing, fige2b_labels_agreeing
walk(c("Figure 2", "Figure 4", "Figure 5", "Figure E.2a", "Figure E.2b"), function(f) {
  id <- c("Figure 2" = "fig2_labels_agreeing", "Figure 4" = "fig4_labels_agreeing",
          "Figure 5" = "fig5_labels_agreeing", "Figure E.2a" = "fige2a_labels_agreeing",
          "Figure E.2b" = "fige2b_labels_agreeing")[[f]]
  claim(id, labels_agreeing(f),
        str_c(f, ": printed labels whose estimate and standard error both agree, of ",
              sum(labels_published$figure == f)))
})

# The one Figure 4 label that sits on an exact tie at its printed digit.
# covers: fig4_undisputed_republican_counterfactual
undisputed <- panel_row(fig4, "Undisputed accusation", "Republican", "CATE", "Counterfactual")
claim("fig4_undisputed_republican_counterfactual", round(undisputed$estimate, 10),
      "Figure 4, undisputed accusation, Republicans, counterfactual CATE")
claim_evidence(str_c("at seventeen significant digits: ", sprintf("%.17g", undisputed$estimate),
                     "; the plain mean lands on the tie -1.125 exactly, and the 10-digit snap before printing ",
                     "prints it -1.12 as the figure does"))

# Figure 3 prints no numbers of its own, so what it asserts is the shape of the two panels
# and the estimates the surrounding text quotes from them.
# covers: fig3a_facets, fig3b_facets
claim("fig3a_facets", sum(fig3a$Party %in% c("All", "Democrat", "Republican")),
      "estimates drawn in Figure 3a: ten facets by three party groups")
claim("fig3b_facets", nrow(fig3b_means),
      "means drawn in Figure 3b: nine facets by two formats by two parties")

# The Nonrandomized Counterfactual Format ----

# Table 3's level questions run on Study 2a's six-point scale, "[1: Definitely oppose; 6:
# Definitely support]".
# covers: table3_scale_low, table3_scale_high
pretreated_outcome <- clean$all$Y[clean$all$study == "2a" &
                                    clean$all$topic %in%
                                    c("Biden / Hill", "DREAM Act", "Opposed Kav", "Supported Kav")]
claim("table3_scale_low", min(pretreated_outcome, na.rm = TRUE),
      "lowest pretreated-example outcome value in the deposited data")
claim("table3_scale_high", max(pretreated_outcome, na.rm = TRUE),
      "highest pretreated-example outcome value in the deposited data")

# "According to the change format, voting for or against Kavanaugh has implausibly large
# electoral consequences for Senators, with large majorities of Democratic and Republican
# respondents saying it impacted their candidate preference. By contrast, the counterfactual
# format offers the more realistic conclusion that the effect was small, perhaps
# ever-so-slightly boosting the steadfastness of Democratic Senators' support within their
# own party."
# covers: nonrand_kavanaugh_change_majorities, nonrand_kavanaugh_counterfactual_small
kavanaugh_cells <- expand_grid(topic = c("Senator opposed Kavanaugh",
                                         "Senator supported Kavanaugh"),
                               party = c("Democrat", "Republican"))
kavanaugh_any_change <-
  pmap_dbl(list(kavanaugh_cells$topic, kavanaugh_cells$party),
           function(t, p) 1 - panel_share(fig5, t, p, "Change", "Same"))

claim("nonrand_kavanaugh_change_majorities", sum(kavanaugh_any_change > 0.5),
      "of the four Kavanaugh cells in which most respondents report the vote changed their preference")
claim_evidence(str_c("shares reporting any change: ",
                     str_c(sprintf("%s/%s %.2f", kavanaugh_cells$topic, kavanaugh_cells$party,
                                   kavanaugh_any_change), collapse = "; ")))

kavanaugh_counterfactual <-
  pmap_dfr(list(kavanaugh_cells$topic, kavanaugh_cells$party), function(t, p) {
    row <- panel_row(fig5, t, p, "CATE", "Counterfactual")
    tibble(topic = t, party = p, estimate = row$estimate,
           low = row$conf.low, high = row$conf.high)
  })
opposed_democrat <- kavanaugh_counterfactual[
  kavanaugh_counterfactual$topic == "Senator opposed Kavanaugh" &
    kavanaugh_counterfactual$party == "Democrat", ]

claim("nonrand_kavanaugh_counterfactual_small", sign(opposed_democrat$estimate),
      "sign of the counterfactual estimate for Democrats whose Senator opposed Kavanaugh")
claim_evidence(str_c("all four counterfactual estimates: ",
                     str_c(sprintf("%s/%s %.2f [%.2f, %.2f]", kavanaugh_counterfactual$topic,
                                   kavanaugh_counterfactual$party, kavanaugh_counterfactual$estimate,
                                   kavanaugh_counterfactual$low, kavanaugh_counterfactual$high),
                           collapse = "; ")))

# "In the case of Biden's skepticism of Hill's allegations, the difference across formats
# was also striking. The change format suggests that Biden's handling of the allegations
# cost him slightly more support among Republicans than among Democrats, while the
# counterfactual format suggests that any loss of support is concentrated among Democrats."
# covers: nonrand_biden_pattern
biden_change <- map_dbl(c("Democrat", "Republican"), function(p) {
  panel_row(fig5, "Biden skeptical of Anita Hill", p, "More-less", "Change")$estimate
})
biden_counterfactual <- map(c("Democrat", "Republican"), function(p) {
  panel_row(fig5, "Biden skeptical of Anita Hill", p, "CATE", "Counterfactual")
})

claim("nonrand_biden_pattern",
      sum(map_dbl(biden_counterfactual, "conf.high") < 0),
      "parties of two whose counterfactual estimate is negative with an interval excluding zero")
claim_evidence(str_c("change format more-minus-less: Democrat ", sprintf("%.2f", biden_change[1]),
                     ", Republican ", sprintf("%.2f", biden_change[2]),
                     "; counterfactual: Democrat ",
                     sprintf("%.2f [%.2f, %.2f]", biden_counterfactual[[1]]$estimate,
                             biden_counterfactual[[1]]$conf.low, biden_counterfactual[[1]]$conf.high),
                     ", Republican ",
                     sprintf("%.2f [%.2f, %.2f]", biden_counterfactual[[2]]$estimate,
                             biden_counterfactual[[2]]$conf.low, biden_counterfactual[[2]]$conf.high)))

# "The DREAM Act treatment produces a similar pattern: the change format suggests that the
# information immensely improves Democrats' already high support for the Act, whereas the
# counterfactual format suggests a small boost mainly for Republicans."
# covers: nonrand_dream_pattern
dream_change <- map_dbl(c("Democrat", "Republican"), function(p) {
  panel_row(fig5, "DREAM Act helps economy", p, "More-less", "Change")$estimate
})
dream_counterfactual <- map_dbl(c("Democrat", "Republican"), function(p) {
  panel_row(fig5, "DREAM Act helps economy", p, "CATE", "Counterfactual")$estimate
})

claim("nonrand_dream_pattern", sum(dream_counterfactual[2] > dream_counterfactual[1]),
      "of one: whether the counterfactual boost is larger for Republicans than for Democrats")
claim_evidence(str_c("change format more-minus-less: Democrat ", sprintf("%.2f", dream_change[1]),
                     ", Republican ", sprintf("%.2f", dream_change[2]),
                     "; counterfactual: Democrat ", sprintf("%.2f", dream_counterfactual[1]),
                     ", Republican ", sprintf("%.2f", dream_counterfactual[2])))

# Discussion ----

# "Across 10 treatments, we found that the counterfactual format yields far more accurate
# estimates of the effect of information on attitudes."
# covers: discussion_treatments
claim("discussion_treatments", length(evaluated_topics),
      "treatments behind the discussion's accuracy claim")

# Supplementary Material, Section D ----

# Table D.3, "Distribution of Change Format by Study and Treatment": every treatment by
# study, party and whether a level question came first, as four percentages with standard
# errors. The published headings over the second and third value columns are transposed
# relative to the body, so the cells are compared twice, once in the order the table's own
# Difference column implies and once in the order the headings state.
# covers: appendix_d3_cells, appendix_d3_cells_as_printed, appendix_d3_sample_sizes
d3_key <- function(printed_column) {
  str_c(d3_published$study, "|", d3_published$topic, "|", d3_published$party, "|",
        d3_published$level_q, "|", printed_column)
}
d3_rewrite_key <- str_c(d3$Study, "|", norm(d3$Topic), "|", d3$Party, "|", d3$`Y1?`, "|",
                        d3$category)

d3_agreeing <- function(category_for_no_change, category_for_less) {
  columns <- c(more = "More", no_change = category_for_no_change,
               less = category_for_less, difference = "Diff")
  sum(map_int(names(columns), function(printed) {
    cells_agreeing(d3_key(columns[[printed]]), d3_published[[printed]],
                   d3_published[[str_c(printed, "_se")]], 1L,
                   d3_rewrite_key, d3$estimate_pct, d3$se_pct)
  }))
}

claim("appendix_d3_cells", d3_agreeing("Less", "Same"),
      str_c("Table D.3 cells agreeing, headings read in the corrected order, of ",
            4 * nrow(d3_published)))
claim("appendix_d3_cells_as_printed", d3_agreeing("Same", "Less"),
      str_c("Table D.3 cells agreeing, headings read as printed, of ",
            4 * nrow(d3_published)),
      expect = "derived")
claim("appendix_d3_sample_sizes",
      values_agreeing(d3_key("More"), d3_published$N, 0L, d3_rewrite_key, d3$N),
      str_c("Table D.3 sample sizes agreeing, of ", nrow(d3_published)))

# Table D.4, "Accuracy of counterfactual guesses": the actual outcome, the counterfactual
# guess and the difference, with a p-value for each difference in means test.
# covers: appendix_d4_cells, appendix_d4_p_values, appendix_d4_sample_sizes
d4_published <- pub("published_table_d4.csv")
d4_key <- function(quantity) {
  str_c(d4_published$study, "|", d4_published$topic, "|", d4_published$outcome, "|",
        d4_published$party, "|", quantity)
}
d4_rewrite_key <- str_c(d4$Study, "|", norm(d4$Topic), "|", d4$Outcome, "|", d4$Party, "|",
                        d4$quantity)

d4_columns <- c(actual = "Actual", guess = "Guess", difference = "Difference")
claim("appendix_d4_cells",
      sum(map_int(names(d4_columns), function(printed) {
        cells_agreeing(d4_key(d4_columns[[printed]]), d4_published[[printed]],
                       d4_published[[str_c(printed, "_se")]], 2L,
                       d4_rewrite_key, d4$estimate, d4$std.error)
      })),
      str_c("Table D.4 cells agreeing, of ", 3 * nrow(d4_published)))
claim("appendix_d4_p_values",
      values_agreeing(d4_key("Difference"), d4_published$p, 3L, d4_rewrite_key, d4$p.value),
      str_c("Table D.4 p-values agreeing, of ", nrow(d4_published)))
claim("appendix_d4_sample_sizes",
      values_agreeing(d4_key("Actual"), d4_published$N, 0L, d4_rewrite_key, d4$N),
      str_c("Table D.4 sample sizes agreeing, of ", nrow(d4_published)))

# Table D.5, "Experimental versus Self-Reported Average Treatment Effect": the experimental
# estimate, the counterfactual guess and their difference, each with a bootstrapped standard
# error and a percentile interval.
# covers: appendix_d5_estimates, appendix_d5_sample_sizes, appendix_d5_intervals
d5_published <- pub("published_table_d5.csv")
d5_key <- str_c(d5_published$study, "|", d5_published$topic, "|", d5_published$party, "|",
                d5_published$estimator)
d5_rewrite_key <- str_c(d5$Study, "|", norm(d5$Topic), "|", d5$Party, "|", d5$Estimator)

claim("appendix_d5_estimates",
      values_agreeing(d5_key, d5_published$estimate, 2L, d5_rewrite_key, d5$estimate),
      str_c("Table D.5 point estimates agreeing, of ", nrow(d5_published)))
claim("appendix_d5_sample_sizes",
      values_agreeing(d5_key, d5_published$N, 0L, d5_rewrite_key, d5$N),
      str_c("Table D.5 sample sizes agreeing, of ", nrow(d5_published)))

d5_position <- positions(d5_key, d5_rewrite_key)
claim("appendix_d5_intervals",
      sum(printed_to_page(d5$std.error[d5_position], 2L) == d5_published$se &
            printed_to_page(d5$conf.low[d5_position], 2L) == d5_published$ci_low &
            printed_to_page(d5$conf.high[d5_position], 2L) == d5_published$ci_high),
      str_c("Table D.5 rows whose standard error and both interval endpoints agree, of ",
            nrow(d5_published)),
      expect = "range")

# Supplementary Material, Section E ----

# "The simultaneous outcomes format appears to be less accurate than the counterfactual
# format. Relative to the counterfactual format, estimates from the simultaneous outcomes
# format were less consistent with the experimental results on the two treatments for which
# we do not expect the pretreatment problem (Obama's torture order, and Trump's coal ash
# order (Figure E.2a). For Democrats, this was the case for both treatments. For
# Republicans, the counterfactual format appears a bit more accurate than the simultaneous
# format on the Obama torture order treatment, but had no obvious advantage or disadvantage
# on the Trump coal ash treatment."
# covers: appendix_e_simultaneous_less_accurate
e2a_cells <- expand_grid(topic = c("Obama torture exec. order", "Trump coal ash exec. order"),
                         party = c("Democrat", "Republican"))
e2a_gaps <-
  pmap_dfr(list(e2a_cells$topic, e2a_cells$party), function(t, p) {
    benchmark <- panel_row(fige2, t, p, "CATE", "Diff. in means")$estimate
    tibble(topic = t, party = p,
           counterfactual = abs(panel_row(fige2, t, p, "CATE", "Counterfactual")$estimate -
                                  benchmark),
           simultaneous = abs(panel_row(fige2, t, p, "CATE", "Simultaneous")$estimate -
                                benchmark))
  })

claim("appendix_e_simultaneous_less_accurate",
      sum(e2a_gaps$simultaneous > e2a_gaps$counterfactual),
      "of the four no-pretreatment cells in which the simultaneous format sits further from the experiment")
walk(seq_len(nrow(e2a_gaps)), function(i) {
  claim_evidence(str_c(e2a_gaps$topic[i], ", ", e2a_gaps$party[i],
                       ": counterfactual off by ", sprintf("%.3f", e2a_gaps$counterfactual[i]),
                       ", simultaneous off by ", sprintf("%.3f", e2a_gaps$simultaneous[i])))
})

# "Switching from the change format to the simultaneous format doubles the percentage of
# Democrats reporting that Biden's skepticism of Hill makes them more supportive of Biden's
# presidential run, producing a positive self-reported average treatment effect estimate."
# covers: appendix_e_biden_doubles
biden_change_more <- panel_share(fige2, "Biden skeptical of Anita Hill", "Democrat",
                                 "Change", "More")
biden_simultaneous_more <- panel_share(fige2, "Biden skeptical of Anita Hill", "Democrat",
                                       "Simultaneous", "More")
claim("appendix_e_biden_doubles", biden_simultaneous_more / biden_change_more,
      "ratio of the simultaneous to the change format share of Democrats reporting more support",
      digits = 2)
claim_evidence(str_c("change format ", sprintf("%.1f", 100 * biden_change_more),
                     " per cent, simultaneous format ", sprintf("%.1f", 100 * biden_simultaneous_more),
                     " per cent; the simultaneous more-minus-less estimate is ",
                     sprintf("%.2f", panel_row(fige2, "Biden skeptical of Anita Hill", "Democrat",
                                               "More-less", "Simultaneous")$estimate)))

# Gates ----
# assert_claims() reads the verdicts above: it stops on any failing verdict, on a quantity
# erratum whose claim was not printed as corrected, and on any difference between the
# claims printed and those the extraction says need a block. claim_summary() then prints
# the CLAIM SUMMARY line, which counts the exemptions rather than merely allowing them.
assert_claims()
invisible(claim_summary())
