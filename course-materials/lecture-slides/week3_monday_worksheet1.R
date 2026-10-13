library(tidyverse)
set.seed(222)
theme_set(theme_bw(18))

homes <- tibble(
  direction = rep(c("NE", "SW"), each = 50),
  high_risk = c(
    rep(c(TRUE, FALSE), c(21, 29)),
    rep(c(TRUE, FALSE), c(16, 34))
  )
)

obs_diff <- mean(homes$high_risk[homes$direction == "NE"]) -
  mean(homes$high_risk[homes$direction == "SW"])

null_diffs <- map_dbl(1:10000, \(i) {
  shuffled <- sample(homes$direction)
  mean(homes$high_risk[shuffled == "NE"]) -
    mean(homes$high_risk[shuffled == "SW"])
})

pvalue <- mean(null_diffs > obs_diff)

p_null <- tibble(diff = null_diffs) |>
  mutate(in_tail = diff > obs_diff) |>
  ggplot(aes(diff, fill = in_tail)) +
  geom_histogram(binwidth = 0.04, colour = "white") +
  geom_vline(xintercept = obs_diff, linewidth = 1) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "steelblue")) +
  labs(
    x = "NE − SW",
    y = "Count"
  ) +
  theme(legend.position = "none")

p_null

# Normal approximation: under the null, both groups share the pooled
# proportion, so NE − SW ~ Normal(0, SE)
p_pool <- mean(homes$high_risk)
se_null <- sqrt(p_pool * (1 - p_pool) * (1 / 50 + 1 / 50))

pvalue_norm <- pnorm(obs_diff, mean = 0, sd = se_null, lower.tail = FALSE)

p_norm <- tibble(diff = seq(-4 * se_null, 4 * se_null, length.out = 500)) |>
  mutate(density = dnorm(diff, mean = 0, sd = se_null)) |>
  ggplot(aes(diff, density)) +
  geom_area(
    data = \(d) filter(d, diff > obs_diff),
    fill = "steelblue"
  ) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = obs_diff, linewidth = 1) +
  labs(
    x = "NE − SW",
    y = "Density"
  )

p_norm

# Resample homes with replacement within each direction 10,000 times,
# recalculating NE - SW each time
boot_diffs <- map_dbl(1:10000, \(i) {
  resampled <- homes |>
    group_by(direction) |>
    slice_sample(prop = 1, replace = TRUE) |>
    ungroup()
  mean(resampled$high_risk[resampled$direction == "NE"]) -
    mean(resampled$high_risk[resampled$direction == "SW"])
}) |>
  # Differences are multiples of 0.02; rounding stops floating point error from
  # splitting a bin at the CI boundary
  round(2)

ci <- quantile(boot_diffs, c(0.025, 0.975))

p_ci <- tibble(diff = boot_diffs) |>
  mutate(in_ci = between(diff, ci[1], ci[2])) |>
  ggplot(aes(diff, fill = in_ci)) +
  geom_histogram(binwidth = 0.04, colour = "white") +
  geom_vline(xintercept = obs_diff, linewidth = 1) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "darkorange")) +
  labs(
    x = "NE − SW",
    y = "Count"
  ) +
  theme(legend.position = "none")

p_ci

# Normal approximation: centered on the observed difference, with each group's
# own proportion in the SE (no pooling, since we're not assuming the null)
p_ne <- mean(homes$high_risk[homes$direction == "NE"])
p_sw <- mean(homes$high_risk[homes$direction == "SW"])
se_diff <- sqrt(p_ne * (1 - p_ne) / 50 + p_sw * (1 - p_sw) / 50)

ci_norm <- obs_diff + qnorm(c(0.025, 0.975)) * se_diff

p_ci_norm <- tibble(
  diff = seq(obs_diff - 4 * se_diff, obs_diff + 4 * se_diff, length.out = 500)
) |>
  mutate(density = dnorm(diff, mean = obs_diff, sd = se_diff)) |>
  ggplot(aes(diff, density)) +
  geom_area(
    data = \(d) filter(d, between(diff, ci_norm[1], ci_norm[2])),
    fill = "darkorange"
  ) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = obs_diff, linewidth = 1) +
  labs(
    x = "NE − SW",
    y = "Density"
  )

p_ci_norm

cowplot::plot_grid(
  p_null,
  p_norm,
  p_ci,
  p_ci_norm,
  nrow = 2,
  labels = "AUTO"
)


ggplot(tibble(x = -100, y = -100), aes(x, y)) +
  geom_point() +
  geom_abline(intercept = 80, slope = -0.5) +
  coord_cartesian(xlim = c(20, 80), ylim = c(20, 80)) +
  theme_bw()

ggplot(tibble(x = -100, y = -100), aes(x, y)) +
  geom_point() +
  # geom_abline(intercept = 0.5, slope = 0.1) +
  coord_cartesian(xlim = c(2, 10), ylim = c(0, 2)) +
  theme_bw()
