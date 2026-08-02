# graham_coppock_2021/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive, build
# the analysis subsets, estimate everything the grand comparison figures show, then
# every published figure, appendix table and in-text number.
# Every script is self-contained and can also be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Cleaning ----
source(here::here("maintained", "clean_studies.R"))

# Estimation shared by Figures 2 and 4 and by the sign comparison ----
source(here::here("maintained", "estimates_grand_comparison.R"))

# The same estimates split by the format a subject saw first, for Figures 5 and E.2 ----
source(here::here("maintained", "estimates_pretreated.R"))

# Figures ----
source(here::here("maintained", "figure_1_impeach_counterfactual.R"))
source(here::here("maintained", "figure_2_cornish_example.R"))
source(here::here("maintained", "figure_3_response_substitution.R"))
source(here::here("maintained", "figure_4_grand_comparison.R"))
source(here::here("maintained", "figure_5_pretreat_comparison.R"))
source(here::here("maintained", "figure_e2_study2b_simultaneous.R"))

# Appendix tables ----
# table_d5_ate_vs_self.R is the slow step, about 65 of the run's 75 seconds: 20 cells
# by 10,000 bootstrap resamples.
source(here::here("maintained", "table_d3_change_distribution.R"))
source(here::here("maintained", "table_d4_counterfactual_accuracy.R"))
source(here::here("maintained", "table_d5_ate_vs_self.R"))

# In-text numbers ----
source(here::here("maintained", "text_intext_stats.R"))
source(here::here("maintained", "text_sign_comparison.R"))

# Ground truth ----
# Rebuilt last, from the outputs above, so it cannot drift from the pipeline.
source(here::here("ground_truth", "build_ground_truth.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on MD5, byte size and membership.
source(here::here("download_original.R"))
