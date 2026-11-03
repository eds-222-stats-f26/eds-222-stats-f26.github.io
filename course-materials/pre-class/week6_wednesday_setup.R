# Week 6 Wednesday: simulated data and helpers
#
# Sourced by week6_wednesday.qmd and week6_wednesday_anim.R so the slides and
# the animation use the same data. Seeds were chosen so the observed p-values
# land near 0.03 (streams) and 0.1 (corals).

library(tidyverse)

# Data -----------------------------------------------------------------------

# Invertebrate taxa in 10 streams from each of four land uses
land_uses <- c("Forest", "Agriculture", "Suburban", "Urban")
set.seed(197)
streams <- tibble(
  land_use = factor(rep(land_uses, each = 10), levels = land_uses),
  taxa = round(rnorm(40, rep(c(24, 22, 21, 19), each = 10), 5)),
  obs_id = 1:40
)

# Coral calcification across temperature, in 15 tanks from each of three
# acidification treatments
acid_levels <- c("Ambient", "Moderate", "High")
set.seed(29)
corals <- tibble(
  acid = factor(rep(acid_levels, each = 15), levels = acid_levels),
  temp = runif(45, 26, 32)
) |>
  mutate(
    calc = c(2.4, 2.05, 1.7)[acid] +
      c(-0.08, -0.12, -0.2)[acid] * (temp - 29) +
      rnorm(45, 0, 0.3),
    temp = round(temp, 1),
    calc = round(calc, 2)
  )

# Helpers --------------------------------------------------------------------

# Residual sum of squares
rss <- function(m) sum(residuals(m)^2)

# F statistic comparing nested models
f_compare <- function(simple, complex) {
  df_spent <- df.residual(simple) - df.residual(complex)
  ((rss(simple) - rss(complex)) / df_spent) /
    (rss(complex) / df.residual(complex))
}

# F statistic for intercept-only vs one categorical predictor
f_land_use <- function(data) {
  f_compare(lm(taxa ~ 1, data), lm(taxa ~ land_use, data))
}

# Shuffle land use labels across streams; taxa stay put
shuffle_labels <- function(data) {
  mutate(data, land_use = sample(land_use))
}

# Fixed jitter, so a stream only moves when its label changes
stream_jitter <- withr::with_seed(222, runif(nrow(streams), -0.2, 0.2))

# Boxplots of taxa by land use, with points behind
plot_streams <- function(data, title = NULL, subtitle = NULL) {
  data |>
    mutate(x = as.numeric(land_use) + stream_jitter[obs_id]) |>
    ggplot(aes(group = land_use)) +
    geom_point(aes(x, taxa), colour = "grey40", size = 2.5, alpha = 0.6) +
    geom_boxplot(
      aes(as.numeric(land_use), taxa),
      fill = NA,
      width = 0.6,
      outlier.shape = NA,
      linewidth = 0.8
    ) +
    scale_x_continuous(breaks = 1:4, labels = land_uses) +
    scale_y_continuous(limits = range(streams$taxa) + c(-1, 1)) +
    labs(x = NULL, y = "Invertebrate taxa", title = title, subtitle = subtitle)
}
