# Week 5 Monday: permutation and bootstrap animations for a regression slope
#
# Creates images/week5_permutation.gif and images/week5_bootstrap.gif for
# week5_monday.qmd. Both use the soil Pb ~ distance data from week 3. Two
# panels, left to right:
#   1. The sample (shuffled or resampled) with its fitted line
#   2. The slope from each shuffle/resample, stacked into a dot plot
# Re-run this script when the animations need to change; the slides only
# embed the saved GIFs.

library(tidyverse)
library(geosphere)
library(gganimate)

set.seed(222)
theme_set(theme_bw(18))

# Data -----------------------------------------------------------------------

# Approximate location of the former Exide facility (2700 S Indiana St, Vernon)
exide_lon <- -118.1923
exide_lat <- 34.0058

soil_cores <- read_csv(
  here::here("course-materials/pre-class/data/exide-soil-cores.csv")
) |>
  filter(location_code == "YD", pb_ppm <= 5e3) |>
  mutate(
    distance_ft = distGeo(cbind(longitude, latitude), c(exide_lon, exide_lat)) *
      3.28,
    distance_mi = distance_ft / 5280,
    bearing_deg = bearing(c(exide_lon, exide_lat), cbind(longitude, latitude))
  ) |>
  drop_na()

step_out <- read_csv(
  here::here("course-materials/pre-class/data/step-out.csv")
) |>
  mutate(distance_mi = distance_ft / 5280)

all_samples_north <- bind_rows(
  `soil core` = transmute(
    filter(soil_cores, between(bearing_deg, 0, 30)),
    property_id,
    distance_mi,
    pb_ppm
  ),
  `step out` = select(step_out, property_id, distance_mi, pb_ppm),
  .id = "source"
) |>
  drop_na() |>
  mutate(obs_id = row_number())

# Least squares slope, without the overhead of lm()
ols_slope <- function(x, y) cov(x, y) / var(x)

obs_slope <- ols_slope(all_samples_north$distance_mi, all_samples_north$pb_ppm)
x_range <- range(all_samples_north$distance_mi)
y_range <- c(0, max(all_samples_north$pb_ppm) * 1.08)

# Fitted line as a segment spanning the observed distances, one row per state
fit_segments <- function(points) {
  points |>
    group_by(state) |>
    summarize(
      slope = ols_slope(distance_mi, pb_ppm),
      intercept = mean(pb_ppm) - slope * mean(distance_mi)
    ) |>
    mutate(
      x = x_range[1],
      xend = x_range[2],
      y = intercept + slope * x,
      yend = intercept + slope * xend
    )
}

# Stack slopes into a dot plot, binned so similar slopes share a column
bin_width <- 4
stack_slopes <- function(slopes) {
  slopes |>
    mutate(bin = round(slope / bin_width) * bin_width) |>
    group_by(bin) |>
    mutate(stack = row_number()) |>
    ungroup()
}

# Dot plot in each state: every slope so far, newest highlighted
accumulate_dots <- function(stacked, states) {
  map(states, \(s) {
    stacked |>
      filter(draw <= s) |>
      mutate(state = s, newest = draw == s)
  }) |>
    list_rbind()
}

n_draws <- 50
states <- 0:n_draws
pb_colour <- "firebrick"

# Permutation animation ------------------------------------------------------

perm_levels <- c("Shuffled sample", "Null distribution (slope)")
perm_panel <- function(i) factor(perm_levels[i], levels = perm_levels)

# State 0 is the observed sample; states 1..n_draws shuffle distances across
# Pb values, so points only move horizontally
perm_points <- map(states, \(s) {
  pts <- all_samples_north
  if (s > 0) pts$distance_mi <- sample(pts$distance_mi)
  mutate(pts, state = s)
}) |>
  list_rbind() |>
  mutate(panel = perm_panel(1))

perm_lines <- fit_segments(perm_points) |>
  mutate(panel = perm_panel(1))

perm_slopes <- perm_lines |>
  filter(state > 0) |>
  transmute(draw = state, slope) |>
  stack_slopes()
perm_dots <- accumulate_dots(perm_slopes, states) |>
  mutate(panel = perm_panel(2))

perm_xlim <- c(-1, 1) * (abs(obs_slope) + 15)
perm_ymax <- max(perm_slopes$stack)

perm_slope_labels <- perm_lines |>
  transmute(
    state,
    panel,
    label = sprintf("slope = %.1f", slope),
    x = mean(x_range),
    y = y_range[2] * 0.95
  )
perm_count_labels <- tibble(
  state = states,
  panel = perm_panel(2),
  # Must not start with a digit: tweenr would parse "5 shuffles" as a palette
  # colour and tween the label through hex codes
  label = sprintf("Shuffles: %d", state),
  x = 0,
  y = perm_ymax + 2
)
obs_label <- tibble(
  panel = perm_panel(2),
  label = sprintf("Observed\n%.1f", obs_slope),
  x = obs_slope,
  y = perm_ymax + 1
)

perm_anim <- ggplot() +
  # Observed line for reference in every shuffle
  geom_abline(
    aes(intercept = intercept, slope = slope),
    data = tibble(
      panel = perm_panel(1),
      slope = obs_slope,
      intercept = mean(all_samples_north$pb_ppm) -
        obs_slope * mean(all_samples_north$distance_mi)
    ),
    colour = "grey50",
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = x),
    data = tibble(panel = perm_panel(2), x = c(0, obs_slope)),
    colour = c("grey50", pb_colour),
    linetype = c("dashed", "solid"),
    linewidth = c(0.5, 1)
  ) +
  geom_point(
    aes(distance_mi, pb_ppm, group = obs_id),
    data = perm_points,
    shape = 21,
    alpha = 0.25
  ) +
  geom_segment(
    aes(x, y, xend = xend, yend = yend),
    data = perm_lines,
    colour = "steelblue",
    linewidth = 1.5
  ) +
  geom_point(
    aes(bin, stack, group = draw),
    data = filter(perm_dots, !newest),
    colour = "grey30",
    size = 3.5
  ) +
  geom_point(
    aes(bin, stack, group = draw),
    data = filter(perm_dots, newest),
    colour = "steelblue",
    size = 5
  ) +
  # Static labels need their own layers: a layer mixing rows with and without
  # a state breaks transition_states
  geom_label(
    aes(x, y, label = label),
    data = bind_rows(perm_slope_labels, perm_count_labels),
    size = 5
  ) +
  geom_text(
    aes(x, y, label = label),
    data = obs_label,
    colour = pb_colour,
    size = 4.5,
    hjust = -0.1
  ) +
  facet_wrap(~panel, nrow = 1, scales = "free", drop = FALSE) +
  ggh4x::facetted_pos_scales(
    x = list(
      panel == perm_levels[1] ~ scale_x_continuous("Distance (mi)"),
      panel == perm_levels[2] ~ scale_x_continuous(
        "Slope (ppm / mi)",
        limits = perm_xlim
      )
    ),
    y = list(
      panel == perm_levels[1] ~ scale_y_continuous(
        "Soil Pb (ppm)",
        limits = y_range
      ),
      panel == perm_levels[2] ~ scale_y_continuous(
        NULL,
        limits = c(0.5, perm_ymax + 3),
        breaks = NULL
      )
    )
  ) +
  labs(title = "Shuffle {closest_state} of {n_draws}") +
  theme(panel.grid = element_blank()) +
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
  here::here("course-materials/pre-class/images/week5_permutation.gif"),
  perm_gif
)

# Bootstrap animation --------------------------------------------------------

boot_levels <- c("Resampled sample", "Bootstrap distribution (slope)")
boot_panel <- function(i) factor(boot_levels[i], levels = boot_levels)

# State 0 is the observed sample; states 1..n_draws resample with replacement.
# Each copy of an observation is its own group, so points never move: copies
# fade in and out, and observations drawn several times look darker
boot_points <- map(states, \(s) {
  pts <- all_samples_north
  if (s > 0) pts <- slice_sample(pts, prop = 1, replace = TRUE)
  pts |>
    group_by(obs_id) |>
    mutate(copy_id = str_c(obs_id, "-", row_number())) |>
    ungroup() |>
    mutate(state = s)
}) |>
  list_rbind() |>
  mutate(panel = boot_panel(1))

boot_lines <- fit_segments(boot_points) |>
  mutate(panel = boot_panel(1))

boot_slopes <- boot_lines |>
  filter(state > 0) |>
  transmute(draw = state, slope) |>
  stack_slopes()
boot_dots <- accumulate_dots(boot_slopes, states) |>
  mutate(panel = boot_panel(2))

boot_ci <- quantile(boot_slopes$slope, c(0.025, 0.975))
boot_xlim <- range(boot_slopes$bin) + c(-15, 15)
boot_ymax <- max(boot_slopes$stack)

boot_slope_labels <- boot_lines |>
  transmute(
    state,
    panel,
    label = sprintf("slope = %.1f", slope),
    x = mean(x_range),
    y = y_range[2] * 0.95
  )
boot_count_labels <- tibble(
  state = states,
  panel = boot_panel(2),
  # Must not start with a digit (see perm_count_labels above)
  label = sprintf("Resamples: %d", state),
  x = mean(boot_xlim),
  y = boot_ymax + 2.2
)

# CI bracket appears only in the final state
boot_ci_segment <- tibble(
  state = n_draws,
  panel = boot_panel(2),
  x = boot_ci[1],
  xend = boot_ci[2],
  y = 0.4
)
# Label sits under the resample count so the dots can rest on the axis
boot_ci_label <- tibble(
  state = n_draws,
  panel = boot_panel(2),
  label = sprintf("95%% CI: [%.1f, %.1f]", boot_ci[1], boot_ci[2]),
  x = mean(boot_xlim),
  y = boot_ymax + 1
)

boot_anim <- ggplot() +
  geom_vline(
    aes(xintercept = obs_slope),
    data = tibble(panel = boot_panel(2)),
    colour = "grey50",
    linetype = "dashed"
  ) +
  geom_point(
    aes(distance_mi, pb_ppm, group = copy_id),
    data = boot_points,
    colour = "black",
    alpha = 0.2
  ) +
  geom_segment(
    aes(x, y, xend = xend, yend = yend),
    data = boot_lines,
    colour = "darkorange",
    linewidth = 1.5
  ) +
  geom_point(
    aes(bin, stack, group = draw),
    data = filter(boot_dots, !newest),
    colour = "grey30",
    size = 3.5
  ) +
  geom_point(
    aes(bin, stack, group = draw),
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
  geom_label(
    aes(x, y, label = label),
    data = bind_rows(boot_slope_labels, boot_count_labels),
    size = 5
  ) +
  geom_label(
    aes(x, y, label = label),
    data = boot_ci_label,
    size = 5,
    colour = "darkorange"
  ) +
  facet_wrap(~panel, nrow = 1, scales = "free", drop = FALSE) +
  ggh4x::facetted_pos_scales(
    x = list(
      panel == boot_levels[1] ~ scale_x_continuous("Distance (mi)"),
      panel == boot_levels[2] ~ scale_x_continuous(
        "Slope (ppm / mi)",
        limits = boot_xlim
      )
    ),
    y = list(
      panel == boot_levels[1] ~ scale_y_continuous(
        "Soil Pb (ppm)",
        limits = y_range
      ),
      panel == boot_levels[2] ~ scale_y_continuous(
        NULL,
        limits = c(0.2, boot_ymax + 3),
        expand = expansion(),
        breaks = NULL
      )
    )
  ) +
  labs(title = "Resample {closest_state} of {n_draws}") +
  theme(panel.grid = element_blank()) +
  transition_states(
    state,
    transition_length = 2,
    state_length = 1,
    wrap = FALSE
  ) +
  enter_fade() +
  exit_fade()

boot_gif <- animate(
  boot_anim,
  nframes = length(states) * 8 + 40,
  fps = 10,
  end_pause = 40,
  width = 13,
  height = 5.5,
  units = "in",
  res = 100,
  renderer = gifski_renderer()
)

anim_save(
  here::here("course-materials/pre-class/images/week5_bootstrap.gif"),
  boot_gif
)
