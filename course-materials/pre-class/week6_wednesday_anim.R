# Week 6 Wednesday: permutation animation for the F statistic
#
# Creates images/week6_f_permutation.gif for week6_wednesday.qmd. Two panels,
# left to right:
#   1. The streams with shuffled land use labels, as boxplots
#   2. The F statistic from each shuffle, stacked into a dot plot
# Boxplots can't tween between shuffles, so frames are drawn one at a time. The
# first few shuffles are held longer so students can follow them. Re-run this
# script when the animation needs to change; the slides only embed the saved GIF.

library(patchwork)

source(here::here("course-materials/pre-class/week6_wednesday_setup.R"))

set.seed(222)
theme_set(theme_bw(18))

n_draws <- 60
n_slow <- 6
frames_slow <- 15
frames_fast <- 3
frames_end <- 50

f_obs <- f_land_use(streams)
shuffles <- map(seq_len(n_draws), \(i) shuffle_labels(streams))

# Stack F statistics into a dot plot, binned so similar values share a column.
# Bins start at the observed F so no column straddles the line.
bin_width <- 0.25
f_dots <- tibble(draw = seq_len(n_draws), f = map_dbl(shuffles, f_land_use)) |>
  mutate(bin = f_obs + (floor((f - f_obs) / bin_width) + 0.5) * bin_width) |>
  group_by(bin) |>
  mutate(stack = row_number()) |>
  ungroup()

n_extreme <- sum(f_dots$f >= f_obs)
x_max <- max(f_obs, f_dots$f) + 1
y_max <- max(f_dots$stack)

# Dot plot of every F so far: newest highlighted, extreme values in red
plot_dots <- function(s, title) {
  dots <- filter(f_dots, draw <= s)
  ggplot(dots, aes(bin, stack)) +
    geom_vline(xintercept = f_obs, colour = "firebrick", linewidth = 1) +
    annotate(
      "text",
      x = f_obs,
      y = y_max + 1,
      hjust = -0.08,
      label = sprintf("Observed\nF = %.2f", f_obs),
      colour = "firebrick",
      size = 5
    ) +
    geom_point(aes(colour = f >= f_obs), size = 3.5) +
    geom_point(data = filter(dots, draw == s), colour = "steelblue", size = 5) +
    scale_colour_manual(
      values = c(`FALSE` = "grey30", `TRUE` = "firebrick"),
      guide = "none"
    ) +
    scale_x_continuous("F", limits = c(0, x_max)) +
    scale_y_continuous(NULL, limits = c(0.5, y_max + 2), breaks = NULL) +
    labs(title = title) +
    theme(panel.grid = element_blank())
}

draw_frame <- function(s) {
  if (s == 0) {
    left <- plot_streams(
      streams,
      "Observed data",
      sprintf("F = %.2f", f_obs)
    )
    right <- plot_dots(0, "F under the null hypothesis")
  } else {
    f_now <- f_dots$f[f_dots$draw == s]
    left <- plot_streams(
      shuffles[[s]],
      sprintf("Shuffle %d of %d", s, n_draws),
      sprintf("F = %.2f", f_now)
    )
    right <- plot_dots(s, "F under the null hypothesis")
  }
  print(left + right)
}

draw_end <- function() {
  left <- plot_streams(
    shuffles[[n_draws]],
    sprintf("Shuffle %d of %d", n_draws, n_draws),
    sprintf("F = %.2f", f_dots$f[n_draws])
  )
  right <- plot_dots(
    n_draws,
    sprintf("%d of %d shuffles reach the observed F", n_extreme, n_draws)
  )
  print(left + right)
}

gifski::save_gif(
  {
    for (s in 0:n_draws) {
      n_frames <- if (s <= n_slow) frames_slow else frames_fast
      for (i in seq_len(n_frames)) {
        draw_frame(s)
      }
    }
    for (i in seq_len(frames_end)) {
      draw_end()
    }
  },
  here::here("course-materials/pre-class/images/week6_f_permutation.gif"),
  width = 1300,
  height = 550,
  res = 100,
  delay = 0.1
)
