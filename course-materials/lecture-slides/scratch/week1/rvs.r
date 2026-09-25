library(tidyverse)
library(palmerpenguins)
theme_set(theme_void(24))

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 12, fill = "cornflowerblue") +
  geom_vline(
    xintercept = mean(penguins$body_mass_g, na.rm = TRUE),
    linewidth = 2,
    color = "firebrick"
  ) +
  geom_vline(
    xintercept = median(penguins$body_mass_g, na.rm = TRUE),
    linewidth = 2,
    linetype = "dashed",
    color = "firebrick"
  )

std_normal <- tibble(
  x = seq(-4, 4, length.out = 1e3),
  y = dnorm(x)
)
ggplot(std_normal, aes(x, y)) +
  # geom_area(data = filter(std_normal, between(x, -1.96, 1.96)),
  #           fill = "cornflowerblue",
  #           alpha = 0.25) +
  # geom_area(data = filter(std_normal, between(x, -1, 1)),
  #           fill = "cornflowerblue",
  #           alpha = 0.25) +
  geom_line(linewidth = 2, color = "cornflowerblue") +
  labs(y = "Density") +
  theme_classic(18)

binomial_n4_p50 <- tibble(
  x = 0:4,
  y = dbinom(x, 4, 0.50)
)
ggplot(binomial_n4_p50, aes(x, y)) +
  geom_segment(
    aes(xend = x, yend = 0),
    linewidth = 2,
    color = "cornflowerblue"
  ) +
  geom_point(color = "cornflowerblue", size = 6, shape = 21, stroke = 2) +
  labs(y = "Mass") +
  theme_classic(18)


ggplot() +
  stat_function(
    fun = \(x) dbeta(x, 4, 8),
    linewidth = 2,
    color = "cornflowerblue"
  )

beta_samples <- map(1:3, \(i) {
  tibble(i = i, x = rbeta(50, 4, 8))
}) %>%
  list_rbind()
ggplot(beta_samples, aes(x)) +
  geom_histogram(bins = 12, fill = "cornflowerblue") +
  facet_grid(cols = vars(i)) +
  theme(strip.text = element_blank(), axis.line.x = element_line(linewidth = 3))


library(ggdag)
dag <- dagify(
  Biodiversity ~ Remoteness + Protection,
  Protection ~ Remoteness
) %>%
  tidy_dagitty()
ggplot(dag, aes(x, y, xend = xend, yend = yend)) +
  geom_dag_edges(linewidth = 2, edge_color = "cornflowerblue") +
  geom_dag_point(color = "cornflowerblue", size = 20)

penguins %>%
  filter(species == "Adelie", sex == "female") %>%
  slice_sample(n = 20) %>%
  ggplot(aes(bill_depth_mm, bill_length_mm)) +
  geom_point(color = "firebrick", size = 3, shape = 21, stroke = 2) +
  geom_smooth(
    method = "lm",
    color = "cornflowerblue",
    fill = "cornflowerblue",
    linewidth = 2
  )


ggplot() +
  stat_function(
    fun = \(x) dbeta(x, 4, 8),
    linewidth = 2,
    color = "cornflowerblue"
  ) +
  geom_vline(xintercept = 4 / 12, color = "firebrick", linewidth = 2)


beta_samples <- map(1:6, \(i) {
  tibble(i = i, x = rbeta(20, 4, 8))
}) %>%
  list_rbind()
beta_summaries <- beta_samples %>%
  group_by(i) %>%
  summarize(mean_x = mean(x))
ggplot(beta_samples, aes(x)) +
  geom_histogram(bins = 8, fill = "cornflowerblue") +
  geom_vline(
    aes(xintercept = mean_x),
    data = beta_summaries,
    color = "#F0E442",
    linewidth = 1.5
  ) +
  geom_vline(xintercept = 4 / 12, color = "firebrick", linewidth = 1.5) +
  facet_grid(rows = vars(i)) +
  theme(strip.text = element_blank())

set.seed(12345)
fishmerc <- tibble(
  `Mercury content (ppm)` = rnorm(65, mean = rnorm(65, 1.035, 0.005), sd = 0.04)
)
ggplot(fishmerc, aes(`Mercury content (ppm)`)) +
  geom_histogram(
    binwidth = 0.025,
    boundary = 0,
    fill = "cornflowerblue",
    color = "grey80"
  ) +
  geom_vline(
    xintercept = mean(fishmerc$`Mercury content (ppm)`),
    linewidth = 2,
    color = "firebrick"
  ) +
  scale_x_continuous(breaks = seq(0.95, 1.15, by = 0.05)) +
  labs(y = "Frequency") +
  theme_classic(24)

set.seed(12345)
fishmerc2 <- tibble(
  `Length (cm)` = rnorm(65, sqrt(163), 0.5)^2,
  length_z = (`Length (cm)` - mean(`Length (cm)`)) / sd(`Length (cm)`),
  expected_merc = 1.035 + 0.012 * length_z,
  `Mercury content (ppm)` = rnorm(65, mean = expected_merc, sd = 0.035)
)
ggplot(fishmerc2, aes(`Length (cm)`, `Mercury content (ppm)`)) +
  geom_point(color = "cornflowerblue", size = 3, shape = 21, stroke = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
  scale_y_continuous(breaks = seq(0.95, 1.15, by = 0.05)) +
  theme_classic(24)


set.seed(321)
fish_toxic <- tibble(
  x = runif(30),
  y = runif(30),
  `Mercury content (ppm)` = rbinom(
    30,
    size = 1,
    prob = 0.4 - x / 4
  )
)
ggplot(aes(x, y, color = `Mercury content (ppm)`)) +
  geom_point(size = 4) +
  scale_color_gradient(low = "cornflowerblue", high = "firebrick") +
  annotate(
    "rect",
    xmin = -0.15,
    xmax = -0.05,
    ymin = 0,
    ymax = 1,
    fill = "grey20"
  ) +
  theme(legend.position = "none")


binom_pmf <- tibble(
  x = 0:4,
  pmf = dbinom(x, 4, 0.50)
)
binom_rnd <- tibble(
  x = rbinom(50, 4, 0.5)
) %>%
  count(x) %>%
  mutate(x_prop = n / sum(n))
ggplot() +
  geom_segment(
    aes(x = x, y = pmf, xend = x, yend = 0),
    binom_pmf,
    linewidth = 2,
    color = "cornflowerblue"
  ) +
  geom_point(
    aes(x = x, y = pmf),
    binom_pmf,
    color = "cornflowerblue",
    size = 6,
    shape = 21,
    stroke = 2
  ) +
  labs(y = "Mass") +
  theme_classic(18)
ggplot() +
  geom_col(aes(x = x, y = n), binom_rnd, fill = "firebrick") +
  labs(y = "Frequency") +
  theme_classic(18)

norm_pdf <- tibble(
  x = seq(40, 70, length.out = 1e3),
  pdf = dnorm(x, 55, 5)
)
ggplot(norm_pdf, aes(x, pdf)) +
  geom_line(linewidth = 2, color = "cornflowerblue") +
  labs(y = "Density") +
  theme_classic(18)
norm_rnd <- tibble(
  x = rnorm(40, 55, 5)
)
ggplot(norm_rnd, aes(x)) +
  geom_histogram(fill = "firebrick", binwidth = 2, boundary = 0) +
  xlim(40, 70) +
  labs(y = "Frequency") +
  theme_classic(18)


# Quiz 3
binomial_n4_p50 <- tibble(
  x = 0:4,
  y = dbinom(x, 4, 0.50)
)
p1 <- ggplot(binomial_n4_p50, aes(x, y)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 2) +
  geom_point(size = 2, shape = 21, stroke = 2) +
  labs(y = "Mass") +
  theme_classic(12)
binomial_n3_p10 <- tibble(
  x = 0:3,
  y = dbinom(x, 3, 0.10)
)
p2 <- ggplot(binomial_n3_p10, aes(x, y)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 2) +
  geom_point(size = 2, shape = 21, stroke = 2) +
  labs(y = "Mass") +
  theme_classic(12)
binomial_n4_p75 <- tibble(
  x = 0:4,
  y = dbinom(x, 4, 0.75)
)
p3 <- ggplot(binomial_n4_p75, aes(x, y)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 2) +
  geom_point(size = 2, shape = 21, stroke = 2) +
  labs(y = "Mass") +
  theme_classic(12)
cowplot::plot_grid(p1, p2, p3, ncol = 1)

normal_pdfs <- tibble(
  x = seq(-10, 10, length.out = 1000),
  y_0_1 = dnorm(x, 0, 1),
  y_0_15 = dnorm(x, 0, 3),
  y_1_1 = dnorm(x, 2, 1)
) %>%
  pivot_longer(-x, names_to = "rv", values_to = "Density")
ggplot(normal_pdfs, aes(x = x, y = Density)) +
  geom_line(aes(linetype = rv), linewidth = 1.5) +
  theme_classic(12) +
  theme(legend.position = "none")
