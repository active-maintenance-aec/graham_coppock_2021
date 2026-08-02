# graham_coppock_2021/maintained/clean_studies.R
# Output: output/data_study1-3_clean.rds, output/data_impeach_clean.rds
# Depends on: original/data_study1-3.rds, original/data_impeach.rds, helpers.R
# Description: Reads the two deposited analysis frames, wraps topic labels, and builds
#   the four subsets the analysis scripts use. No estimation happens here.

source(here::here("maintained", "helpers.R"))

all <- read_rds(here::here(data_dir, "data_study1-3.rds"))

# Format ordering used by the grand comparison figures ----
format_levels <- c("Diff. in means", "Counterfactual", "Simultaneous", "Change + level", "Change")

# Response substitution subset: the change and change-plus-level arms ----
# Topic labels are wrapped onto two lines so the facet strips fit.
substitution_dat <-
  all |>
  filter(!is.na(Party), format %in% c("Change", "Change + level")) |>
  mutate(
    topic = str_replace(topic, " whistleblower", "\nwhistleblower"),
    topic = str_replace(topic, " accusation", "\naccusation"),
    topic = str_replace(topic, " comments", "\ncomments"),
    topic = str_replace(topic, " charters", "\ncharters")
  )

substitution_dat_study13 <- substitution_dat |> filter(study %in% c(1, 3))

# Counterfactual format subset ----
state_both <- all |> filter(format == "Counterfactual")
state_both_no_inds <- state_both |> filter(pid_7n != 4, !is.na(pid_7n))

# Impeachment survey ----
dat_impeach <- read_rds(here::here(data_dir, "data_impeach.rds"))

write_rds(
  list(
    all = all,
    substitution_dat = substitution_dat,
    substitution_dat_study13 = substitution_dat_study13,
    state_both = state_both,
    state_both_no_inds = state_both_no_inds,
    format_levels = format_levels
  ),
  here::here(out_dir, "data_study1-3_clean.rds")
)

write_rds(dat_impeach, here::here(out_dir, "data_impeach_clean.rds"))

print(tibble(
  object = c("all", "substitution_dat", "substitution_dat_study13",
             "state_both_no_inds", "dat_impeach"),
  rows = c(nrow(all), nrow(substitution_dat), nrow(substitution_dat_study13),
           nrow(state_both_no_inds), nrow(dat_impeach))
))
