# Week 2 Wednesday: normal approximation animation
#
# Creates images/week2_normal_approx.gif for week2_wednesday.qmd. Three panels,
# left to right:
#   1. The population: proportion of homes with soil Pb > 200, by build era
#   2. A sample of 50 homes from each era (one sample per state)
#   3. The sampling distribution of Pre - Post, built up one sample at a time,
#      with the normal approximation overlaid
# Re-run this script when the animation needs to change; the slides only
# embed the saved GIF.

library(tidyverse)
library(gganimate)

set.seed(222)
theme_set(theme_bw(18))

# Population parameters
p_pre <- 0.60
p_post <- 0.52
n_per_group <- 50
true_diff <- p_pre - p_post
se_diff <- sqrt(
  p_pre * (1 - p_pre) / n_per_group + p_post * (1 - p_post) / n_per_group
)

panel_levels <- c(
  "Population",
  sprintf("Sample (n = %d per group)", n_per_group),
  "Sampling distribution (Pre − Post)"
)
panel_factor <- function(i) factor(panel_levels[i], levels = panel_levels)

# Shared x positions for the two groups in panels 1 and 2
group_x <- c(Pre = 3, Post = 10)
group_scale_x <- scale_x_continuous(
  limits = c(0, 13),
  breaks = group_x,
  labels = c("Pre-1950", "Post-1950")
)

# Panel 1: population proportions as stacked bars ----------------------------

pop_bars <- tibble(
  built = rep(c("Pre", "Post"), each = 2),
  high_pb = factor(rep(c(TRUE, FALSE), 2), levels = c(FALSE, TRUE)),
  prop = c(p_pre, 1 - p_pre, p_post, 1 - p_post),
  x = group_x[built],
  panel = panel_factor(1)
)

# Panel 2: one sample of 50 homes per group in each state --------------------

# Grid position for each home: 5 columns per group, high-Pb homes on the bottom
# rows so the two groups are easy to compare
grid_rows <- n_per_group / 5
grid_positions <- function(homes) {
  homes |>
    arrange(built, desc(high_pb)) |>
    group_by(built) |>
    mutate(
      slot = row_number() - 1,
      x = slot %% 5 + group_x[built] - 2,
      y = slot %/% 5 + 1
    ) |>
    ungroup() |>
    select(-slot)
}

diff_props <- function(homes) {
  mean(homes$high_pb[homes$built == "Pre"]) -
    mean(homes$high_pb[homes$built == "Post"])
}

n_samples <- 100
states <- seq_len(n_samples)
samples <- map(states, \(s) {
  tibble(
    built = rep(c("Pre", "Post"), each = n_per_group),
    high_pb = c(
      runif(n_per_group) < p_pre,
      runif(n_per_group) < p_post
    )
  ) |>
    grid_positions() |>
    mutate(home_id = row_number(), state = s)
}) |>
  list_rbind()

# Panel 3: sampling distribution, stacked into a dot plot --------------------

# With n per group, every difference is a multiple of 1/n
bin_width <- 1 / n_per_group
sample_diffs <- samples |>
  group_by(state) |>
  summarize(diff = round(diff_props(pick(everything())), 2)) |>
  group_by(diff) |>
  mutate(stack = row_number()) |>
  ungroup() |>
  rename(sample_id = state)

# Sampling distribution in each state: every sample so far, newest highlighted
dist_dots <- map(states, \(s) {
  sample_diffs |>
    filter(sample_id <= s) |>
    mutate(state = s, newest = sample_id == s)
}) |>
  list_rbind() |>
  mutate(panel = panel_factor(3))

# Normal approximation, scaled from density to counts so it lines up with the
# dot stacks once all samples are drawn. curve_scale stretches it vertically
# for visibility
curve_scale <- 1.3
dist_xlim <- true_diff + c(-4, 4) * se_diff
normal_curve <- tibble(
  x = seq(dist_xlim[1], dist_xlim[2], length.out = 200),
  y = dnorm(x, true_diff, se_diff) * n_samples * bin_width * curve_scale,
  panel = panel_factor(3)
)
dist_ymax <- max(sample_diffs$stack, normal_curve$y)

# Labels ---------------------------------------------------------------------

pop_label <- tibble(
  label = sprintf("Pre − Post = %.2f", true_diff),
  x = 6.5,
  y = 1.1,
  panel = panel_factor(1)
)
sample_labels <- samples |>
  group_by(state) |>
  summarize(
    label = sprintf("Pre − Post = %.2f", diff_props(pick(everything())))
  ) |>
  mutate(x = 6.5, y = grid_rows + 1.6, panel = panel_factor(2))
count_labels <- tibble(
  state = states,
  # Must not start with a digit: tweenr would parse "5 samples" as a palette
  # colour and tween the label through hex codes
  label = sprintf("Samples: %d", state),
  x = true_diff,
  y = dist_ymax + 2.5,
  panel = panel_factor(3)
)
normal_label <- tibble(
  label = sprintf("Normal(%.2f, SE = %.2f)", true_diff, se_diff),
  x = dist_xlim[2],
  y = dist_ymax + 0.8,
  panel = panel_factor(3)
)

samples <- mutate(
  samples,
  high_pb = factor(high_pb, levels = c(FALSE, TRUE)),
  panel = panel_factor(2)
)

# Animation ------------------------------------------------------------------

pb_values <- c(`TRUE` = "firebrick", `FALSE` = "grey70")

normal_anim <- ggplot() +
  geom_col(
    aes(x, prop, fill = high_pb),
    data = pop_bars,
    width = 5
  ) +
  geom_hline(
    aes(yintercept = prop),
    data = filter(pop_bars, high_pb == "TRUE"),
    linetype = "dashed",
    colour = "grey20"
  ) +
  geom_vline(
    aes(xintercept = true_diff),
    data = tibble(panel = panel_factor(3)),
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_point(
    aes(x, y, colour = high_pb, group = home_id),
    data = samples,
    size = 3
  ) +
  geom_point(
    aes(diff, stack, group = sample_id),
    data = filter(dist_dots, !newest),
    colour = "grey30",
    size = 2.5
  ) +
  geom_point(
    aes(diff, stack, group = sample_id),
    data = filter(dist_dots, newest),
    colour = "darkorange",
    size = 4
  ) +
  geom_line(
    aes(x, y),
    data = normal_curve,
    colour = "steelblue",
    linewidth = 1.5
  ) +
  # Static labels need their own layers: a layer mixing rows with and without
  # a state breaks transition_states
  geom_label(
    aes(x, y, label = label),
    data = bind_rows(sample_labels, count_labels),
    size = 5
  ) +
  geom_label(aes(x, y, label = label), data = pop_label, size = 5) +
  geom_label(
    aes(x, y, label = label),
    data = normal_label,
    size = 4.5,
    colour = "steelblue",
    hjust = 1
  ) +
  facet_wrap(~panel, nrow = 1, scales = "free", drop = FALSE) +
  ggh4x::facetted_pos_scales(
    x = list(
      panel != panel_levels[3] ~ group_scale_x,
      panel == panel_levels[3] ~ scale_x_continuous(limits = dist_xlim)
    ),
    y = list(
      panel == panel_levels[1] ~ scale_y_continuous(
        limits = c(0, 1.2),
        breaks = seq(0, 1, 0.25)
      ),
      panel == panel_levels[2] ~ scale_y_continuous(
        limits = c(0, grid_rows + 2),
        breaks = NULL
      ),
      panel == panel_levels[3] ~ scale_y_continuous(
        limits = c(0, dist_ymax + 3.5),
        breaks = NULL
      )
    )
  ) +
  # Identical colour and fill scales merge into one legend
  scale_colour_manual("Soil Pb > 200", values = pb_values) +
  scale_fill_manual("Soil Pb > 200", values = pb_values) +
  labs(x = NULL, y = NULL, title = "Sample {closest_state} of {n_samples}") +
  theme(legend.position = "bottom", panel.grid = element_blank()) +
  transition_states(
    state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  enter_fade() +
  enter_grow()

normal_gif <- animate(
  normal_anim,
  nframes = n_samples * 6 + 40,
  fps = 15,
  end_pause = 40,
  width = 15,
  height = 5.5,
  units = "in",
  res = 100,
  renderer = gifski_renderer()
)

anim_save(
  here::here("course-materials/pre-class/images/week2_normal_approx.gif"),
  normal_gif
)
