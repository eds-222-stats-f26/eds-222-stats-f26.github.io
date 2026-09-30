# Week 2 Monday: sampling variation animation
#
# Creates images/week2_sampling_variation.gif for week2_monday_a.qmd.
# Re-run this script when the animation needs to change; the slides only
# embed the saved GIF.

library(tidyverse)
library(gganimate)

set.seed(222)
theme_set(theme_bw(18))

# Population: 2,000 homes in each direction, with a true difference of 0.10
population <- tibble(
  direction = rep(c("NE", "SW"), each = 2000),
  high_risk = c(
    rep(c(TRUE, FALSE), c(800, 1200)),
    rep(c(TRUE, FALSE), c(600, 1400))
  )
)

# Proportions for a set of homes, labeled by panel and sample number
summarize_props <- function(homes, panel, sample_id) {
  homes |>
    count(direction, high_risk) |>
    group_by(direction) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(panel = panel, sample_id = sample_id)
}

# Draw 12 samples of 50 homes per direction
n_samples <- 12
sample_props <- map(seq_len(n_samples), \(i) {
  population |>
    group_by(direction) |>
    slice_sample(n = 50) |>
    ungroup() |>
    summarize_props(paste0("Sample (n = 50 per group)"), i)
}) |>
  list_rbind()

# Repeat the population in every frame so it stays fixed
pop_props <- map(seq_len(n_samples), \(i) {
  summarize_props(population, "Population", i)
}) |>
  list_rbind()

plot_props <- bind_rows(pop_props, sample_props) |>
  mutate(
    panel = fct_inorder(panel),
    high_risk = factor(high_risk, levels = c(FALSE, TRUE))
  )

# Difference in proportions (NE - SW) for each panel and frame
diff_labels <- plot_props |>
  filter(high_risk == "TRUE") |>
  select(panel, sample_id, direction, prop) |>
  pivot_wider(names_from = direction, values_from = prop) |>
  mutate(label = sprintf("NE \u2212 SW = %.2f", NE - SW))

anim <- ggplot(plot_props, aes(direction, prop)) +
  geom_col(aes(fill = high_risk), width = 0.6) +
  geom_label(
    aes(x = 1.5, y = 1.1, label = label),
    data = diff_labels,
    size = 6
  ) +
  facet_wrap(~panel, ncol = 1) +
  scale_fill_manual(
    "High risk",
    values = c(`TRUE` = "firebrick", `FALSE` = "grey70")
  ) +
  scale_y_continuous(
    "Proportion of homes",
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1.2)
  ) +
  labs(x = NULL, title = "Sample {closest_state} of {n_samples}") +
  transition_states(sample_id, transition_length = 1, state_length = 2)

anim_gif <- animate(
  anim,
  nframes = n_samples * 15,
  fps = 10,
  width = 8,
  height = 7,
  units = "in",
  res = 100,
  renderer = gifski_renderer()
)

anim_save(
  here::here(
    "course-materials/pre-class/images/week2_sampling_variation.gif"
  ),
  anim_gif
)

# Permutation animation -------------------------------------------------------
#
# Creates images/week2_permutation.gif. Three panels, left to right:
#   1. The observed sample, homes arranged in a grid by direction
#   2. The same homes with direction labels shuffled (one shuffle per state)
#   3. The null distribution of NE - SW, built up one shuffle at a time

# One observed sample of 50 homes per direction
obs_sample <- population |>
  group_by(direction) |>
  slice_sample(n = 50) |>
  ungroup() |>
  mutate(home_id = row_number())

diff_props <- function(homes) {
  mean(homes$high_risk[homes$direction == "NE"]) -
    mean(homes$high_risk[homes$direction == "SW"])
}

# Grid position for each home: 5 columns per direction, high-risk homes on the
# bottom rows so the two groups are easy to compare
grid_positions <- function(homes) {
  homes |>
    arrange(direction, desc(high_risk), home_id) |>
    group_by(direction) |>
    mutate(
      slot = row_number() - 1,
      x = slot %% 5 + if_else(direction == "NE", 1, 8),
      y = slot %/% 5 + 1
    ) |>
    ungroup() |>
    select(-slot)
}

panel_levels <- c("Observed", "Shuffled", "Null distribution (NE − SW)")

# State 0 is the unshuffled data; states 1..n_perms are shuffles
n_perms <- 50
states <- 0:n_perms
shuffled <- map(states, \(s) {
  homes <- obs_sample
  if (s > 0) homes$direction <- sample(homes$direction)
  homes |>
    grid_positions() |>
    mutate(state = s)
}) |>
  list_rbind()

# Repeat the observed grid in every state so it stays fixed
observed <- map(states, \(s) {
  obs_sample |>
    grid_positions() |>
    mutate(state = s)
}) |>
  list_rbind()

grid_homes <- bind_rows(
  mutate(observed, panel = panel_levels[1]),
  mutate(shuffled, panel = panel_levels[2])
) |>
  mutate(panel = factor(panel, levels = panel_levels))

# Difference in proportions for each shuffle, stacked into a dot plot
perm_diffs <- shuffled |>
  filter(state > 0) |>
  group_by(state) |>
  summarize(diff = diff_props(pick(everything()))) |>
  mutate(diff = round(diff, 2)) |>
  group_by(diff) |>
  mutate(stack = row_number()) |>
  ungroup() |>
  rename(perm = state)

# Null distribution in each state: every shuffle so far, newest highlighted
null_dots <- map(states, \(s) {
  perm_diffs |>
    filter(perm <= s) |>
    mutate(state = s, newest = perm == s)
}) |>
  list_rbind() |>
  mutate(panel = factor(panel_levels[3], levels = panel_levels))

# Difference labels above the two grids, shuffle count above the null
grid_labels <- grid_homes |>
  group_by(panel, state) |>
  summarize(
    label = sprintf("NE − SW = %.2f", diff_props(pick(everything()))),
    .groups = "drop"
  ) |>
  mutate(x = 6.5, y = 12)
grid_homes <- mutate(
  grid_homes,
  high_risk = factor(high_risk, levels = c(FALSE, TRUE))
)
null_max <- max(perm_diffs$stack)
null_xlim <- max(abs(perm_diffs$diff)) + 0.06
null_labels <- tibble(
  state = states,
  panel = factor(panel_levels[3], levels = panel_levels),
  # Must not start with a digit: tweenr would parse "5 shuffles" as a palette
  # colour and tween the label through hex codes
  label = sprintf("Shuffles: %d", state),
  x = 0,
  y = null_max + 2
)
text_labels <- bind_rows(grid_labels, null_labels)

grid_x <- scale_x_continuous(
  limits = c(0, 13),
  breaks = c(3, 10),
  labels = c("NE", "SW")
)
grid_y <- scale_y_continuous(limits = c(0, 13), breaks = NULL)

perm_anim <- ggplot() +
  geom_vline(
    aes(xintercept = 0),
    data = tibble(panel = factor(panel_levels[3], levels = panel_levels)),
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_point(
    aes(x, y, colour = high_risk, group = home_id),
    data = grid_homes,
    size = 3.5
  ) +
  geom_point(
    aes(diff, stack, group = perm),
    data = filter(null_dots, !newest),
    colour = "grey30",
    size = 3.5
  ) +
  geom_point(
    aes(diff, stack, group = perm),
    data = filter(null_dots, newest),
    colour = "steelblue",
    size = 5
  ) +
  geom_label(aes(x, y, label = label), data = text_labels, size = 5) +
  facet_wrap(~panel, nrow = 1, scales = "free") +
  ggh4x::facetted_pos_scales(
    x = list(
      panel != panel_levels[3] ~ grid_x,
      panel == panel_levels[3] ~ scale_x_continuous(
        limits = c(-null_xlim, null_xlim)
      )
    ),
    y = list(
      panel != panel_levels[3] ~ grid_y,
      panel == panel_levels[3] ~ scale_y_continuous(
        limits = c(0.5, null_max + 3),
        breaks = NULL
      )
    )
  ) +
  scale_colour_manual(
    "High risk",
    values = c(`TRUE` = "firebrick", `FALSE` = "grey70")
  ) +
  labs(x = NULL, y = NULL, title = "Shuffle {closest_state} of {n_perms}") +
  theme(legend.position = "bottom", panel.grid = element_blank()) +
  transition_states(
    state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  enter_fade() +
  enter_grow()

perm_gif <- animate(
  perm_anim,
  nframes = length(states) * 8 + 30,
  fps = 10,
  end_pause = 30,
  width = 13,
  height = 5.5,
  units = "in",
  res = 100,
  renderer = gifski_renderer()
)

anim_save(
  here::here("course-materials/pre-class/images/week2_permutation.gif"),
  perm_gif
)

# Bootstrap animation ---------------------------------------------------------
#
# Creates images/week2_bootstrap.gif. Three panels, left to right:
#   1. The observed sample; homes not drawn in the current resample are faded
#   2. A bootstrap resample: 50 homes per direction drawn with replacement
#   3. The bootstrap distribution of NE - SW, built up one resample at a time,
#      with the 95% confidence interval added after the last resample

boot_levels <- c("Observed", "Resampled", "Bootstrap distribution (NE − SW)")

# State 0 is the observed sample; states 1..n_boot are resamples. Resample
# within each direction so group sizes match the observed sample.
n_boot <- 50
boot_states <- 0:n_boot
resampled <- map(boot_states, \(s) {
  homes <- obs_sample
  if (s > 0) {
    homes <- homes |>
      group_by(direction) |>
      slice_sample(prop = 1, replace = TRUE) |>
      ungroup()
  }
  homes |>
    grid_positions() |>
    mutate(draw_id = row_number(), state = s)
}) |>
  list_rbind()

# Observed grid in every state, faded where a home wasn't drawn
boot_observed <- map(boot_states, \(s) {
  drawn_ids <- resampled$home_id[resampled$state == s]
  obs_sample |>
    grid_positions() |>
    mutate(drawn = home_id %in% drawn_ids, state = s)
}) |>
  list_rbind() |>
  mutate(panel = factor(boot_levels[1], levels = boot_levels))

# Difference in proportions for each resample, stacked into a dot plot
boot_diffs <- resampled |>
  filter(state > 0) |>
  group_by(state) |>
  summarize(diff = diff_props(pick(everything()))) |>
  mutate(diff = round(diff, 2)) |>
  group_by(diff) |>
  mutate(stack = row_number()) |>
  ungroup() |>
  rename(boot = state)

obs_diff_boot <- diff_props(obs_sample)
boot_ci <- quantile(boot_diffs$diff, c(0.025, 0.975))

boot_dots <- map(boot_states, \(s) {
  boot_diffs |>
    filter(boot <= s) |>
    mutate(state = s, newest = boot == s)
}) |>
  list_rbind() |>
  mutate(panel = factor(boot_levels[3], levels = boot_levels))

boot_grid_labels <- bind_rows(
  mutate(boot_observed, panel = boot_levels[1]),
  mutate(resampled, panel = boot_levels[2])
) |>
  group_by(panel, state) |>
  summarize(
    label = sprintf("NE − SW = %.2f", diff_props(pick(everything()))),
    .groups = "drop"
  ) |>
  mutate(panel = factor(panel, levels = boot_levels), x = 6.5, y = 12)
boot_max <- max(boot_diffs$stack)
boot_xlim <- range(boot_diffs$diff) + c(-0.08, 0.08)
boot_count_labels <- tibble(
  state = boot_states,
  panel = factor(boot_levels[3], levels = boot_levels),
  # Must not start with a digit (see null_labels above)
  label = sprintf("Resamples: %d", state),
  x = mean(boot_xlim),
  y = boot_max + 2
)

# CI bracket appears only in the final state
ci_panel <- factor(boot_levels[3], levels = boot_levels)
boot_ci_segment <- tibble(
  state = n_boot,
  panel = ci_panel,
  x = boot_ci[1],
  xend = boot_ci[2],
  y = 0
)
boot_ci_label <- tibble(
  state = n_boot,
  panel = ci_panel,
  label = sprintf("95%% CI: [%.2f, %.2f]", boot_ci[1], boot_ci[2]),
  x = mean(boot_xlim),
  y = -1
)

boot_text_labels <- bind_rows(boot_grid_labels, boot_count_labels)
boot_observed <- mutate(
  boot_observed,
  high_risk = factor(high_risk, levels = c(FALSE, TRUE))
)
resampled <- mutate(
  resampled,
  high_risk = factor(high_risk, levels = c(FALSE, TRUE)),
  panel = factor(boot_levels[2], levels = boot_levels)
)

boot_anim <- ggplot() +
  geom_vline(
    aes(xintercept = obs_diff_boot),
    data = tibble(panel = ci_panel),
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_point(
    aes(x, y, colour = high_risk, alpha = drawn, group = home_id),
    data = boot_observed,
    size = 3.5
  ) +
  geom_point(
    aes(x, y, colour = high_risk, group = draw_id),
    data = resampled,
    size = 3.5
  ) +
  geom_point(
    aes(diff, stack, group = boot),
    data = filter(boot_dots, !newest),
    colour = "grey30",
    size = 3.5
  ) +
  geom_point(
    aes(diff, stack, group = boot),
    data = filter(boot_dots, newest),
    colour = "darkorange",
    size = 5
  ) +
  geom_segment(
    aes(x, y, xend = xend, yend = y),
    data = boot_ci_segment,
    colour = "darkorange",
    linewidth = 2,
    arrow = arrow(angle = 90, ends = "both", length = unit(0.1, "in"))
  ) +
  geom_label(aes(x, y, label = label), data = boot_text_labels, size = 5) +
  geom_label(
    aes(x, y, label = label),
    data = boot_ci_label,
    size = 5,
    colour = "darkorange"
  ) +
  facet_wrap(~panel, nrow = 1, scales = "free", drop = FALSE) +
  ggh4x::facetted_pos_scales(
    x = list(
      panel != boot_levels[3] ~ grid_x,
      panel == boot_levels[3] ~ scale_x_continuous(limits = boot_xlim)
    ),
    y = list(
      panel != boot_levels[3] ~ grid_y,
      panel == boot_levels[3] ~ scale_y_continuous(
        limits = c(-1.8, boot_max + 3),
        breaks = NULL
      )
    )
  ) +
  scale_colour_manual(
    "High risk",
    values = c(`TRUE` = "firebrick", `FALSE` = "grey70")
  ) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.15), guide = "none") +
  labs(x = NULL, y = NULL, title = "Resample {closest_state} of {n_boot}") +
  theme(legend.position = "bottom", panel.grid = element_blank()) +
  transition_states(
    state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  enter_fade() +
  enter_grow()

boot_gif <- animate(
  boot_anim,
  nframes = length(boot_states) * 8 + 40,
  fps = 10,
  end_pause = 40,
  width = 13,
  height = 5.5,
  units = "in",
  res = 100,
  renderer = gifski_renderer()
)

anim_save(
  here::here("course-materials/pre-class/images/week2_bootstrap.gif"),
  boot_gif
)
