library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)


# How to run a statistical "experiment" -----------------------------------

# This is the population:
p1 <- 0.42
p2 <- 0.55
n1 <- 60
n2 <- 40

# Here are three samples
plot_sample <- function() {
  x1 <- rbinom(1, size = n1, prob = p1)
  x2 <- rbinom(1, size = n2, prob = p2)
  rbind(
    tibble(
      treatment = "Garden staple",
      success = c(rep(1, x1), rep(0, n1 - x1))
    ),
    tibble(
      treatment = "Popsicle stick",
      success = c(rep(1, x2), rep(0, n2 - x2))
    )
  ) %>%
    mutate(success_fct = factor(success, labels = c("Fail", "Succeed"))) %>%
    ggplot(aes(treatment, fill = success_fct)) +
    geom_bar() +
    scale_fill_manual(values = c("firebrick", "cornflowerblue")) +
    labs(x = "Restoration method", y = "Sites", fill = "Outcome") +
    theme(legend.position = "none")
}
cowplot::plot_grid(
  plot_sample(),
  plot_sample(),
  plot_sample(),
  ncol = 1
)
ggsave(
  "course-materials/lecture-slides/scratch/week5/3samples.png",
  width = 4,
  height = 8,
  units = "in"
)


# confidence interval -----------------------------------------------------

# Claim: a 95% confidence interval contains the population parameter 95% of the time

# 1. Choose parameters and predictor
p1 <- 0.42
p2 <- 0.55
n1 <- 60
n2 <- 40

# 2. Simulate many samples
n_samples <- 1e4
x1 <- rbinom(n = n_samples, size = n1, prob = p1)
x2 <- rbinom(n = n_samples, size = n2, prob = p2)

# 3. Calculate statistic for each sample
p1_hat <- x1 / n1
p2_hat <- x2 / n2
diff_prop <- p2_hat - p1_hat

# 4. Calculate standard error for CI
se_diff_prop <- sqrt(p1_hat * (1 - p1_hat) / n1 + p2_hat * (1 - p2_hat) / n2)

# 5. Calculate CI for each sample
ci_95_lwr <- qnorm(0.025, mean = diff_prop, sd = se_diff_prop)
ci_95_upr <- qnorm(0.975, mean = diff_prop, sd = se_diff_prop)

# 6. Calculate coverage
covered <- ci_95_lwr <= p2 - p1 & p2 - p1 <= ci_95_upr
coverage <- mean(covered)
coverage

# 7. Visualize result
tibble(
  sample = 1:n_samples,
  diff_prop,
  ci_95_lwr,
  ci_95_upr,
  covered = ci_95_lwr <= p2 - p1 & p2 - p1 <= ci_95_upr
) %>%
  sample_n(100) %>%
  ggplot(aes(
    diff_prop,
    sample,
    xmin = ci_95_lwr,
    xmax = ci_95_upr,
    color = covered
  )) +
  geom_pointrange() +
  scale_color_manual(
    values = c(`FALSE` = "firebrick", `TRUE` = "cornflowerblue")
  ) +
  theme(legend.position = "none")


# hypothesis test ---------------------------------------------------------

# Claim: at alpha=0.05, p values reject the null hypothesis correctly 95% of the time

# 1. Choose parameters and predictor FOR THE NULL HYPOTHESIS
p <- 0.5
n1 <- 60
n2 <- 40

# 2. Simulate many samples
n_samples <- 1e4
x1 <- rbinom(n = n_samples, size = n1, prob = p)
x2 <- rbinom(n = n_samples, size = n2, prob = p)

# 3. Calculate statistic for each sample
p1_hat <- x1 / n1
p2_hat <- x2 / n2
diff_prop <- p2_hat - p1_hat

# 4. Calculate standard error for null hypothesis
p_hat <- (x1 + x2) / (n1 + n2)
se_null <- sqrt(p_hat * (1 - p_hat) * (1 / n1 + 1 / n2))

# 5. Calculate p value
pval <- pnorm(diff_prop, mean = 0, sd = se_null, lower.tail = FALSE)
mean(pval <= 0.05)

# 6. Visualize result
tibble(diff_prop, reject = pval <= 0.05) %>%
  ggplot(aes(diff_prop, fill = reject)) +
  geom_histogram(binwidth = 0.02) +
  scale_fill_manual(
    values = c(`FALSE` = "cornflowerblue", `TRUE` = "firebrick")
  ) +
  labs(x = "Difference in proportions") +
  theme(legend.position = "none")


# One sample mean ---------------------------------------------------------
set.seed(123)

mu <- 120
sigma <- 20
n <- 40

# Here is the sample
x <- rnorm(n, mu, sigma)
x_bar <- mean(x)
restoration_1mean <- tibble(
  shoot_density_m2 = x
)
ggplot(restoration_1mean, aes(shoot_density_m2)) +
  geom_histogram(
    binwidth = 10,
    boundary = 80,
    color = "grey10",
    fill = "grey90"
  ) +
  geom_vline(xintercept = x_bar, color = "firebrick", linewidth = 2) +
  scale_x_continuous(
    "Shoot density (m^-2)",
    limits = c(80, 160),
    breaks = seq(80, 160, by = 20)
  ) +
  scale_y_continuous("Count", breaks = seq(0, 12, by = 3))
ggsave(
  "course-materials/lecture-slides/scratch/week5/1samplemean.png",
  width = 5,
  height = 5,
  units = "in"
)

# Claim: a 95% confidence interval contains the population parameter 95% of the time

# 1. Choose parameters and predictor
mu <- 120
sigma <- 20
n <- 40

# 2. Simulate many samples
n_samples <- 1e4
sim_sample <- function(i) rnorm(n, mu, sigma)
x <- map(1:1e4, sim_sample)

# 3. Calculate statistic for each sample
x_bar <- map_dbl(x, mean)

# 4. Calculate standard error for CI
se_mean <- map_dbl(x, sd) / sqrt(n)

# 5. Calculate CI for each sample
ci_95_lwr <- qnorm(0.025, mean = x_bar, sd = se_mean)
ci_95_upr <- qnorm(0.975, mean = x_bar, sd = se_mean)

# 6. Calculate coverage
covered <- ci_95_lwr <= mu & mu <= ci_95_upr
coverage <- mean(covered)
coverage

# 7. Visualize result
tibble(
  sample = 1:n_samples,
  x_bar,
  ci_95_lwr,
  ci_95_upr,
  covered = ci_95_lwr <= mu & mu <= ci_95_upr
) %>%
  sample_n(100) %>%
  ggplot(aes(
    x_bar,
    sample,
    xmin = ci_95_lwr,
    xmax = ci_95_upr,
    color = covered
  )) +
  geom_pointrange() +
  geom_vline(xintercept = mu, linewidth = 1.5) +
  scale_color_manual(
    values = c(`FALSE` = "firebrick", `TRUE` = "cornflowerblue")
  ) +
  theme(legend.position = "none")


# Two sample difference in means ------------------------------------------

set.seed(123)

mu1 <- 120
mu2 <- 80
sigma <- 20
n1 <- 40
n2 <- 60

# Here is the sample
x1 <- rnorm(n1, mu1, sigma)
x_bar1 <- mean(x1)
x2 <- rnorm(n2, mu2, sigma)
x_bar2 <- mean(x2)
restoration_2means <- tibble(
  shoot_density_m2 = c(x1, x2),
  treatment = c(rep("Popsicle stick", n1), rep("Garden staple", n2))
)
ggplot(restoration_2means, aes(shoot_density_m2)) +
  geom_histogram(
    binwidth = 10,
    boundary = 80,
    color = "grey10",
    fill = "grey90"
  ) +
  geom_vline(
    xintercept = x_bar1,
    color = "firebrick",
    linewidth = 2,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = x_bar2,
    color = "firebrick",
    linewidth = 2,
    linetype = "dotted"
  ) +
  facet_grid(rows = vars(treatment)) +
  scale_x_continuous(
    "Shoot density (m^-2)",
    limits = c(30, 160),
    breaks = seq(30, 160, by = 20)
  ) +
  scale_y_continuous("Count", breaks = seq(0, 15, by = 3))
ggsave(
  "course-materials/lecture-slides/scratch/week5/2samplemean.png",
  width = 5,
  height = 5,
  units = "in"
)
