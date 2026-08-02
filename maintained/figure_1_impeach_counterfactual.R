# graham_coppock_2021/maintained/figure_1_impeach_counterfactual.R
# Output: output/figure_1_impeach_counterfactual.pdf, output/figure_1_impeach_counterfactual.png,
#   output/figure_1_impeach_counterfactual.csv
# Depends on: clean_studies.R output, helpers.R
# Description: Reproduces Figure 1: the joint distribution of the two impeachment
#   questions (panel a) and the average change by partisan group (panel b) for the
#   Ukraine example.

source(here::here("maintained", "helpers.R"))

dat <- read_rds(here::here(out_dir, "data_impeach_clean.rds"))

theme_impeach <- function() {
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(margin = margin(0, 5, 0, 0)),
    axis.title.x     = element_text(margin = margin(5, 0, 0, 0))
  )
}

# Panel (a): joint distribution ----
# The archive's position_jitter_ellipse ggproto calls ggplot2:::with_seed_null, which
# is an unexported internal. Standard position_jitter does the same job here: the
# paper describes it only as "a small amount random noise added to distinguish the
# points", and the panel carries no numbers. The seed makes the jitter reproducible.
set.seed(12345)
g1 <-
  ggplot(dat, aes(impeach_Y1, impeach_Y0, color = pid_7)) +
  geom_point(
    position = position_jitter(width = 0.25, height = 0.25),
    alpha = 0.1,
    stroke = 0
  ) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  scale_color_gradient2(
    low = bpr_colors[1], mid = bpr_colors[2], high = bpr_colors[3], midpoint = 4
  ) +
  labs(
    y = "Q2: Imagine that you did not know about [Ukraine].\n       How would you have answered the question:\n       How strongly do you oppose or support the\nimpeachment of Donald Trump?",
    x = "Q1: How strongly do you oppose or support\nthe impeachment of Donald Trump?\n[1: Strongly oppose, 7: Strongly support]",
    title = "(a)  Joint distribution of individual responses"
  ) +
  theme_poself() +
  theme_impeach()

# Panel (b): average change by partisan group
summary_df <-
  dat |>
  group_by(pid_7) |>
  reframe(tidy(lm_robust(impeach_tau_i ~ 1, data = pick(everything()))))

g2 <-
  ggplot(summary_df, aes(pid_7, estimate, color = pid_7)) +
  geom_point() +
  geom_text(aes(label = format_num(estimate, 2)), nudge_x = 0.45, size = 2.5) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_gradient2(
    low = bpr_colors[1], mid = bpr_colors[2], high = bpr_colors[3], midpoint = 4
  ) +
  scale_x_continuous(breaks = 1:7) +
  theme_poself() +
  theme_impeach() +
  labs(
    x = "Party ID\n[1 = Strong Democrat, 7 = Strong Republican]\n",
    y = "\nAverage change in support for impeachment\ndue to [Ukraine]: Q1 - Q2",
    title = "(b)  Average change by partisan group"
  )

# Combine panels ----
# Building the grob needs a graphics device for its text metrics, and under Rscript
# that means the default device writes an Rplots.pdf beside the working directory.
# pdf(NULL) gives it a null device instead.
pdf(NULL)
g <- gridExtra::arrangeGrob(
  g1 + theme(plot.margin = margin(1, 10, 1, 1)),
  g2 + theme(plot.margin = margin(1, 1, 1, 10)),
  nrow = 1
)
dev.off()

ggsave(here::here(out_dir, "figure_1_impeach_counterfactual.pdf"), plot = g,
       width = 6.5, height = 3.25)
ggsave(here::here(out_dir, "figure_1_impeach_counterfactual.png"), plot = g,
       width = 6.5, height = 3.25, dpi = 300)

# Panel (b) is the only part of the figure that carries numbers: the seven printed
# means. Panel (a) is the raw joint distribution of the two questions, so its
# contents are the deposited data and are not rewritten here.
write_csv(
  summary_df |>
    select(pid_7, estimate, std.error, conf.low, conf.high) |>
    mutate(label = format_num(estimate, 2)),
  here::here(out_dir, "figure_1_impeach_counterfactual.csv")
)

print(summary_df |> select(pid_7, estimate, std.error, conf.low, conf.high))
