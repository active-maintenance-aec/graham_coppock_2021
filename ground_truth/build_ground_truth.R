# graham_coppock_2021/ground_truth/build_ground_truth.R
# Output: ground_truth/graham_coppock_2021_ground_truth.csv
# Depends on: maintained/output/ (run run_all.R first), the published-value files and
#   archive_values.csv in this folder
# Description: Assemble the ground truth table.
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
#     - match and match_rewrite are computed, never asserted.
#
#   Published values are kept as strings so the precision the page prints survives, and
#   a value agrees when the rewrite's number, printed to that precision, gives the same
#   digits. Reading them as numbers would silently turn a standard error printed as
#   0.20 into a target good to one decimal place.

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

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

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

agrees <- function(value, target) {
  d <- decimals(target)
  as.numeric(fmt(value, d) == fmt(as.numeric(target), d))
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

label_misses <- labels |>
  filter(!ok) |>
  transmute(
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
# "Less". Its own Difference column is More minus the column headed "No change" in all
# 84 rows, so the two headings are transposed relative to the body: the column headed
# "No change" holds the share reporting less. The comparison below reads the columns in
# that corrected order, and one claim row records how many cells agree if the headings
# are taken at face value instead.
d3_rewrite <- d3 |>
  transmute(study = as.character(Study), topic = norm(Topic), party = Party,
            level_q = `Y1?`, N_rewrite = N, category,
            estimate_rewrite = estimate_pct, se_rewrite = se_pct)

d3_published_long <- pub("published_table_d3.csv") |>
  pivot_longer(c(more, no_change, less, difference), names_to = "printed", values_to = "estimate") |>
  mutate(se = case_when(printed == "more" ~ more_se, printed == "no_change" ~ no_change_se,
                        printed == "less" ~ less_se, .default = difference_se)) |>
  select(study, topic, party, level_q, N, printed, estimate, se)

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

d4_fig4_cells <- d4 |>
  filter(quantity == "Difference",
         Study == "1" | (Study == "2a" & str_detect(Topic, "Obama|Trump")))

# Assemble ----
gt <- bind_rows(
  tribble(
    ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "Text, p. 31", "Survey responses obtained from Lucid", stat("N impeachment survey"), "4034", "",
    "Text, p. 32", "Correlation of the two impeachment questions", stat("cor(Y1, Y0) impeachment"), "0.82",
      "The article gives 0.82; the data give 0.828, which prints as 0.83",
    "Text, p. 32", "Share reporting exactly zero change", stat("Share reporting exactly zero change"), "0.72", "",
    "Text, p. 32", "Average difference between the two questions", stat("Mean tau_i impeachment"), "0.16", "",
    "Text, p. 32", "Standard error of that average", stat("SE of mean tau_i impeachment"), "0.02", "",
    "Text, p. 38", "Cornish, Democrats reporting less likely (per cent)", cornish_share("Democrat", "Less"), "87", "",
    "Text, p. 38", "Cornish, Republicans reporting no effect (per cent)", cornish_share("Republican", "Same"), "57", "",
    "Text, p. 39", "Cornish, effect of the level question on Democrats' more-minus-less score",
      cornish_effect("Democrat", "estimate"), "0.24", "",
    "Text, p. 39", "Standard error of that effect", cornish_effect("Democrat", "se"), "0.10", "",
    "Text, p. 39", "Cornish, same effect among Republicans", cornish_effect("Republican", "estimate"), "-0.24", "",
    "Text, p. 39", "Standard error of that effect", cornish_effect("Republican", "se"), "0.14", "",
    "Text, p. 40", "Study 1 N", stat("N study 1 (format conditions)"), "417", "",
    "Text, p. 40", "Study 2a N", stat("N study 2a (format conditions)"), "2475", "",
    "Text, p. 44", "Study 2b N", stat("N study 2b (format conditions)"), "1110", "",
    "Text, p. 44", "Study 3 N", stat("N study 3 (respondents in the deposit)"), "1074",
      paste0("The deposit holds ", stat("N study 3 (respondents in the deposit)"),
             " distinct Study 3 respondents, in data_study3.csv and in the merged file alike. ",
             "The article's 1,074 is the full Mechanical Turk survey; the deposited extract covers ",
             "only the respondents in a question format condition, and Table D.3's Study 3 rows ",
             "reproduce exactly from it"),
    # The article states these four as magnitudes of a decrease, without a sign, so the
    # comparison is on the magnitude.
    "Text, p. 44", "Decrease in reporting any change, all respondents (points)",
      abs(100 * stat("Fig 3a overall estimate")), "10", "",
    "Text, p. 44", "Standard error of that effect (points)", 100 * stat("Fig 3a overall SE"), "2", "",
    "Text, p. 44", "Same decrease among Democrats (points)", abs(100 * stat("Fig 3a Democrat estimate")), "14",
      "The article gives 14 points. The estimate is 13.5, which prints as 13",
    "Text, p. 44", "Standard error, Democrats (points)", 100 * stat("Fig 3a Democrat SE"), "3", "",
    "Text, p. 44", "Same decrease among Republicans (points)", abs(100 * stat("Fig 3a Republican estimate")), "8", "",
    "Text, p. 44", "Standard error, Republicans (points)", 100 * stat("Fig 3a Republican SE"), "4", "",
    "Text, p. 44", "Same decrease among pure independents (points)",
      abs(100 * stat("Fig 3a Independent estimate")), "2", "",
    "Text, p. 44", "Standard error, independents (points)", 100 * stat("Fig 3a Independent SE"), "6", "",
    "Text, p. 46", "Cells compared, change format against the experiment", nrow(signs), "20", "",
    "Text, p. 46", "Change format has the opposite sign", sum(signs$change_wrong_sign), "12", "",
    "Text, p. 46", "Counterfactual format has the opposite sign", sum(signs$counterfactual_wrong_sign), "3", "",
    "Text, p. 46", "Opportunities to evaluate a counterfactual guess", nrow(d4_fig4_cells), "40", "",
    "Text, p. 47", "Difference in means tests rejecting at p < 0.05",
      sum(d4_fig4_cells$p.value < 0.05), "12", "",
    "Text, p. 46", "Comparisons of the experiment against the counterfactual guess",
      sum(d5_rewrite$estimator == "Difference"), "20", "",
    "Text, p. 46", "Differences whose bootstrap interval excludes zero",
      sum(d5_rewrite$estimator == "Difference" &
            sign(d5_rewrite$ci_low_rewrite) == sign(d5_rewrite$ci_high_rewrite)), "6", ""
  ),

  # Figure 1, panel (b): the seven printed group means
  fig1 |> transmute(
    table_figure = "Figure 1b",
    claim = paste0("Average change, party ID ", pid_7),
    value_rewrite = estimate,
    value_paper = c("0.17", "0.19", "0.29", "0.19", "0.14", "0.17", "0.06"),
    notes = "Printed beside each point in the published panel"
  ),

  # Figure 3, which prints no numbers: its shape, its pooled facet and the two facets
  # the surrounding text describes in words
  tribble(
    ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "Figure 3a", "Estimates plotted (ten facets by All, Democrat, Republican)",
      sum(fig3a$Party %in% c("All", "Democrat", "Republican")), "30",
      "The panel draws a Pooled facet and one per treatment in Studies 1 and 3",
    "Figure 3a", "Pooled decrease in reporting any change, all respondents",
      abs(fig3a_pooled("All")), "0.10",
      "The panel's Pooled facet plots what the surrounding text quotes as a 10 point decrease",
    "Figure 3b", "Means plotted (nine facets by two formats by two parties)", nrow(fig3b_means), "36", "",
    "Figure 3b", "Endorsed Trump, Republicans: sign of the level question's effect",
      fig3b_sign("Endorsed Trump", "Republican"), "-1",
      paste0("The text says asking the level question first reduced Republican claims that their ",
             "support increased, so the effect on the more-minus-less score is negative"),
    "Figure 3b", "Mueller comments, Democrats: sign of the level question's effect",
      fig3b_sign("Mueller comments", "Democrat"), "-1",
      paste0("The text says Democrats became less likely to claim the comments moved them, so the ",
             "effect on the more-minus-less score is negative")
  ),

  # Printed figure labels: one summary row per figure, plus every disagreement
  label_summary |> transmute(
    table_figure = figure,
    claim = "Printed labels agreeing with the published figure",
    value_rewrite = n_agree,
    value_paper = as.character(n_printed),
    notes = paste0("Each label is an estimate and a standard error; a label counts as agreeing ",
                   "only if both halves do at the two decimals the figure prints")
  ),
  label_misses,

  # Appendix tables
  tribble(
    ~table_figure, ~claim, ~value_rewrite, ~value_paper, ~notes,
    "Table D.3", "Cells agreeing with the published table", sum(d3_cmp$ok), as.character(nrow(d3_cmp)),
      paste0("Each cell is a percentage and its standard error at one decimal. Compared with the ",
             "table's 'No change' and 'Less' headings read in the transposed order its own ",
             "Difference column implies"),
    "Table D.3", "Cells agreeing if the column headings are read as printed",
      sum(d3_cmp_literal$ok), as.character(nrow(d3_cmp_literal)),
      paste0("The same comparison with 'No change' taken to mean the share reporting no change. ",
             "The shortfall is the size of the transposition: in all 84 rows the printed ",
             "Difference equals More minus the column headed 'No change'"),
    "Table D.3", "Sample sizes agreeing", sum(d3_cmp$n_ok) / 4, as.character(nrow(d3_cmp) / 4), "",
    "Table D.4", "Cells agreeing with the published table", sum(d4_cmp$ok), as.character(nrow(d4_cmp)),
      paste0("Each cell is an estimate and its standard error at the two decimals the published ",
             "table prints. The deposited script formats these cells to three decimals instead"),
    "Table D.4", "p-values agreeing", sum(d4_cmp$p_ok, na.rm = TRUE), as.character(nrow(d4_cmp) / 3),
      "One p-value per difference in means test, printed to three decimals",
    "Table D.4", "Sample sizes agreeing", sum(d4_cmp$n_ok) / 3, as.character(nrow(d4_cmp) / 3), "",
    "Table D.5", "Point estimates agreeing with the published table",
      sum(d5_cmp$est_ok), as.character(nrow(d5_cmp)), "",
    "Table D.5", "Sample sizes agreeing", sum(d5_cmp$n_ok), as.character(nrow(d5_cmp)), "",
    "Table D.5", "Bootstrap standard errors and interval endpoints agreeing",
      sum(d5_cmp$interval_ok), as.character(nrow(d5_cmp)),
      paste0("The deposited script resamples with replicate() and sample() over every cell in the ",
             "counterfactual data; the rewrite uses rsample::bootstraps() at the same seed and the ",
             "same 10,000 draws over the twenty cells the table reports, so the two walk different ",
             "random streams. The deposited script reproduces all sixty published rows exactly; the ",
             "rewrite differs on seven of them by a unit in the second decimal, which is Monte Carlo ",
             "variation rather than a difference in the estimator")
  )
)

# value_script ----
# Everything the deposited scripts and the rewrite both produce agreed to machine
# precision, checked above against archive_values.csv, so the two columns carry the same
# numbers everywhere except the one row where the bootstrap streams part company.
gt <- gt |>
  mutate(
    paper_id = "graham_coppock_2021",
    value_script = value_rewrite,
    match = agrees(value_script, value_paper),
    match_rewrite = agrees(value_rewrite, value_paper)
  )

boot_row <- gt$table_figure == "Table D.5" &
  gt$claim == "Bootstrap standard errors and interval endpoints agreeing"
stopifnot(sum(boot_row) == 1)
gt$value_script[boot_row] <- archive_d5_boot_reproduced
gt$match[boot_row] <- agrees(archive_d5_boot_reproduced, gt$value_paper[boot_row])

# Where the rewrite and the article disagree, name the locus ----
gt <- gt |>
  mutate(defect_locus = case_when(
    match_rewrite == 1 | is.na(match_rewrite) ~ NA_character_,
    claim == "Study 3 N" ~ "archive",
    claim == "Correlation of the two impeachment questions" ~ "paper_internal",
    claim == "Same decrease among Democrats (points)" ~ "paper_internal",
    str_detect(claim, "^Cells agreeing if the column headings") ~ "paper_internal",
    str_detect(claim, "^Bootstrap standard errors") ~ "rewrite",
    claim == "Undisputed accusation, Republican, CATE, Counterfactual" ~ "environment",
    table_figure == "Figure 4" & str_detect(claim, "^Printed labels agreeing") ~ "environment",
    .default = "unresolved"
  ))

# The one printed label that disagrees sits on a rounding tie ----
tie_row <- gt$claim == "Undisputed accusation, Republican, CATE, Counterfactual"
stopifnot(sum(tie_row) == 1)
gt$notes[tie_row] <- paste0(
  gt$notes[tie_row],
  " The quantity is a mean of 72 values that comes to -1.125 exactly, which is a tie at the ",
  "second decimal. lm_robust returns it one unit in the last place away from that tie, so it ",
  "prints as -1.13; the mean of the same 72 values, which the appendix's Table D.5 reports for ",
  "the same cell, lands on the tie and prints as -1.12, which is what the published figure ",
  "shows. The deposited script produces the same bits as the rewrite and prints -1.13 too."
)

gt$notes[gt$table_figure == "Figure 4" & str_detect(gt$claim, "^Printed labels agreeing")] <-
  paste0(gt$notes[gt$table_figure == "Figure 4" & str_detect(gt$claim, "^Printed labels agreeing")],
         ". The one label that disagrees has its own row below.")

stopifnot(all(!is.na(gt$defect_locus[!is.na(gt$match_rewrite) & gt$match_rewrite == 0])))

gt <- gt |>
  select(paper_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

write_csv(gt, here::here("ground_truth", "graham_coppock_2021_ground_truth.csv"))

print(gt |> select(table_figure, claim, value_paper, value_rewrite, match_rewrite, defect_locus),
      n = nrow(gt), width = 200)
print(tibble(
  rows = nrow(gt),
  match_1 = sum(gt$match == 1, na.rm = TRUE),
  match_0 = sum(gt$match == 0, na.rm = TRUE),
  match_na = sum(is.na(gt$match)),
  rewrite_1 = sum(gt$match_rewrite == 1, na.rm = TRUE),
  rewrite_0 = sum(gt$match_rewrite == 0, na.rm = TRUE),
  rewrite_na = sum(is.na(gt$match_rewrite))
))
