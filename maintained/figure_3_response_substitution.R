# graham_coppock_2021/maintained/figure_3_response_substitution.R
# Output: output/figure_3_response_substitution.pdf, output/figure_3_response_substitution.png,
#   output/figure_3a_reporting_any_change.csv, output/figure_3b_self_reported_means.csv,
#   output/figure_3b_level_question_effects.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Reproduces Figure 3: (a) the estimated effect of asking the level
#   question first on reporting any attitude change, and (b) its effect on the sign of
#   self-reported change. Both panels use Studies 1 and 3. Panel (a)'s pooled rows are
#   also the effects the article quotes in the surrounding text, including the pure
#   independents the panel itself leaves out, so they are estimated once here and read
#   back by text_intext_stats.R. Panel (b) plots a mean per format, and the gap between
#   the two is what the Cornish narrative on page 39 quotes with a standard error, so
#   that difference is estimated here too.

source(here::here("maintained", "helpers.R"))

clean <- read_rds(here::here(out_dir, "data_study1-3_clean.rds"))
substitution_dat_study13 <- clean$substitution_dat_study13

# --- Panel (a): effect of level question on P(reporting any change) ---

tab_reduce_change <-
  substitution_dat_study13 |>
  (\(x) bind_rows(
    x,
    x |> mutate(topic = "Pooled", Topic = "Pooled")
  ))() |>
  (\(x) bind_rows(x, x |> mutate(pid_3 = "All")))() |>
  group_by(topic, Topic, pid_3) |>
  reframe(
    tidy(lm_robust((YC_usual != 0) ~ Format, data = pick(everything()), clusters = id))
  ) |>
  filter(term != "(Intercept)") |>
  mutate(
    Party = pid_3,
    party_initial = str_replace(pid_3, "emocrat|epublican", ""),
    Topic = str_replace_all(Topic, " and", " &"),
    Topic = str_replace_all(Topic, "blower", "blow"),
    Topic = relevel(factor(Topic), "Pooled")
  )

# The panel plots Democrats, Republicans and the two combined. Pure independents are
# estimated above because the article quotes them in the text.
write_csv(
  tab_reduce_change |>
    select(topic, Topic, Party, estimate, std.error, conf.low, conf.high, p.value, df),
  here::here(out_dir, "figure_3a_reporting_any_change.csv")
)

g3a <- tab_reduce_change |>
  filter(Party != "Independent") |>
  ggplot(aes(x = party_initial, y = estimate, ymin = conf.low, ymax = conf.high,
             color = Party, shape = Party)) +
  geom_hline(yintercept = 0, color = "white",  linewidth = .25) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = .25, lty = 2) +
  geom_point(size = 1.75) +
  geom_linerange() +
  theme_bw() +
  theme(
    legend.position      = "none",
    legend.title         = element_blank(),
    legend.spacing.x     = unit(.1, "cm"),
    axis.ticks           = element_blank(),
    axis.text.y          = element_text(hjust = 0.5),
    axis.title           = element_text(size = 9, color = "gray5"),
    axis.title.x         = element_blank(),
    axis.text            = element_text(size = 9, color = "gray5"),
    axis.text.x          = element_text(size = 7, angle = 0, vjust = .5, hjust = .5),
    legend.text          = element_text(size = 9, color = "gray5"),
    strip.background     = element_blank(),
    strip.placement      = "outside",
    strip.text           = element_text(size = 7),
    axis.title.y         = element_text(margin = margin(0, 8, 0, 0)),
    legend.key.height    = unit(.3, "cm"),
    legend.margin        = margin(0, 0, 0, 0)
  ) +
  scale_color_manual(values = c("gray5", bpr_colors_noI)) +
  scale_shape_manual(values = c(17, 19, 15)) +
  facet_wrap(~Topic, nrow = 1) +
  labs(y = "Difference in percent\nreporting change", x = "Party")

# --- Panel (b): self-reported means by format and party ---

tab_self_means <-
  substitution_dat_study13 |>
  mutate(Format = recode_values(format, "Change" ~ "No", "Change + level" ~ "Yes")) |>
  filter(pid_3 != "Independent", !is.na(Format)) |>
  group_by(topic, Topic, pid_3, format, Format) |>
  reframe(tidy(lm_robust(YC_usual ~ 1, data = pick(everything())))) |>
  ungroup()

# Append a dummy "All respondents" row for legend anchor (matches original)
dummy_row <- tab_self_means[1, ] |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) 5),
         pid_3 = "All respondents")

tab_self_means <- bind_rows(tab_self_means, dummy_row)

g3b <- tab_self_means |>
  mutate(
    Topic = str_replace_all(Topic, " and", " &"),
    Topic = str_replace_all(Topic, "blower", "blow")
  ) |>
  ggplot(aes(y = estimate, x = Format, color = pid_3, group = pid_3, shape = pid_3)) +
  scale_color_manual(values = c("gray5", bpr_colors_noI)) +
  scale_shape_manual(values = c(17, 19, 15)) +
  geom_path(linewidth = .5, position = position_dodge(width = .5)) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    linewidth = .4, position = position_dodge(width = .5)
  ) +
  geom_point(size = 1.75, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = c(-1, 0, 1),
    labels = c("Less (-1)", "None (0)", "More (1)")
  ) +
  theme_bw() +
  theme(
    axis.ticks           = element_blank(),
    axis.text            = element_text(size = 8),
    axis.text.y          = element_text(hjust = 0.5),
    axis.title           = element_text(size = 9, color = "gray5"),
    axis.title.y         = element_text(margin = margin(0, 8, 0, 0)),
    axis.title.x         = element_text(margin = margin(8, 0, 0, 0)),
    strip.background     = element_blank(),
    strip.placement      = "outside",
    strip.text           = element_text(size = 8),
    legend.title         = element_blank(),
    legend.text          = element_text(size = 9, color = "gray5"),
    legend.spacing.x     = unit(.1, "cm"),
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.key.height    = unit(.3, "cm"),
    legend.margin        = margin(0, 0, 0, 0)
  ) +
  labs(y = "Self-reported change", x = "Asked level question first?") +
  facet_grid(~Topic, switch = "y")

# --- The gap between the two points in each panel (b) facet ---

# Panel (b) draws one mean per format. The article's Cornish narrative quotes the
# difference between them, with a standard error, which a difference of two plotted
# means cannot supply. It is estimated directly here.
tab_level_question_effects <-
  substitution_dat_study13 |>
  group_by(topic, Topic, pid_3) |>
  reframe(tidy(lm_robust(YC_usual ~ Format, data = pick(everything())))) |>
  filter(term != "(Intercept)") |>
  select(topic, Topic, Party = pid_3, estimate, std.error, conf.low, conf.high, p.value, df)

write_csv(tab_level_question_effects,
          here::here(out_dir, "figure_3b_level_question_effects.csv"))

# --- Combine and save ---

# The dummy row exists only to anchor the legend, so it is dropped from the CSV.
write_csv(
  tab_self_means |>
    filter(pid_3 != "All respondents") |>
    select(topic, Topic, Party = pid_3, format, level_question_first = Format,
           estimate, std.error, conf.low, conf.high),
  here::here(out_dir, "figure_3b_self_reported_means.csv")
)

pdf(NULL)
g <- gridExtra::arrangeGrob(g3a, g3b, nrow = 2, heights = c(2, 2.5))
dev.off()

ggsave(here::here(out_dir, "figure_3_response_substitution.pdf"), plot = g,
       width = 6.8, height = 4.5)
ggsave(here::here(out_dir, "figure_3_response_substitution.png"), plot = g,
       width = 6.8, height = 4.5, dpi = 300)

print(tab_reduce_change |> select(Topic, Party, estimate, std.error, conf.low, conf.high), n = 30)
