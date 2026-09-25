library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

# Choose population parameters (probability of success)
p1 <- 0.42
p2 <- 0.55
delta_p_pop <- p2 - p1

# Simulate sample
restoration_n <- 100
restoration <- tibble(
  plot_id = 1:restoration_n,
  treatment = sample(
    c("Garden staple", "Popsicle stick"),
    size = restoration_n,
    replace = TRUE,
    prob = c(0.6, 0.4)
  ),
  success = rbinom(
    restoration_n,
    size = 1,
    p = ifelse(treatment == "Garden staple", p1, p2)
  ),
  shoot_density_m2 = rgamma(
    restoration_n,
    shape = ifelse(treatment == "Garden staple", 4, 5),
    rate = 0.05
  ),
  success_fct = factor(success, labels = c("Fail", "Succeed"))
)

# Data distribution
ggplot(restoration, aes(treatment, fill = success_fct)) +
  geom_bar() +
  scale_fill_manual(values = c("firebrick", "cornflowerblue")) +
  labs(x = "Restoration method", y = "Sites", fill = "Outcome") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1)
  )

# Population
plot_binom_pmf <- function(n, p, xmax = n, norm = FALSE) {
  binom_pmf <- tibble(
    x = 0:n,
    pmf = dbinom(x, n, p)
  )
  if (norm) {
    binom_pmf$x <- binom_pmf$x / n
  }
  ggplot() +
    geom_segment(
      aes(x = x, y = pmf, xend = x, yend = 0),
      binom_pmf,
      linewidth = 1.5,
      color = "black"
    ) +
    geom_point(
      aes(x = x, y = pmf),
      binom_pmf,
      color = "black",
      size = 3,
      shape = 21,
      stroke = 1.5
    ) +
    labs(x = ifelse(norm, "Proportion success", "Successes"), y = "Mass") +
    xlim(0, ifelse(norm, 1, xmax))
}
cowplot::plot_grid(
  plot_binom_pmf(60, p1, xmax = 60),
  plot_binom_pmf(40, p2, xmax = 60),
  plot_binom_pmf(60, p1, norm = TRUE),
  plot_binom_pmf(40, p2, norm = TRUE),
  nrow = 2,
  ncol = 2,
  byrow = FALSE
)
ggsave(
  "course-materials/lecture-slides/scratch/week4/two_pops.png",
  width = 35,
  height = 20,
  units = "cm"
)

# Sampling distribution
outcome1 <- rbinom(1e6, 60, p1)
outcome2 <- rbinom(1e6, 40, p2)
diff_p <- outcome2 / 40 - outcome1 / 60
ggplot(tibble(diff_p), aes(diff_p)) +
  geom_histogram(binwidth = 0.025) +
  geom_vline(xintercept = p2 - p1, linewidth = 2, color = "firebrick") +
  labs(x = "Difference in proportion", y = "Number of samples")
ggsave(
  "course-materials/lecture-slides/scratch/week4/samp_dist.png",
  width = 20,
  height = 20,
  units = "cm"
)

# Calculate test statistic
p_hat1 <- mean(restoration$success[restoration$treatment == "Garden staple"])
p_hat2 <- mean(restoration$success[restoration$treatment == "Popsicle stick"])
delta_p_spl <- p_hat2 - p_hat1

# Compare
delta_p_pop
delta_p_spl

# Repeat 10 times
sim_spl <- function(i) {
  # Simulate samples
  sample1 <- rbinom(40, size = 1, prob = p1)
  sample2 <- rbinom(60, size = 1, prob = p2)

  # Calculate test statistic
  delta_p_spl <- mean(sample2) - mean(sample1)

  delta_p_spl
}
spl_10 <- map_dbl(1:10, sim_spl)
round(spl_10, 2)

# Repeat 10,000 times
spl_many <- map_dbl(1:10000, sim_spl)
ggplot(tibble(delta_p = spl_many), aes(delta_p)) +
  geom_histogram(binwidth = 0.025) +
  geom_vline(xintercept = delta_p_pop, color = "firebrick", linewidth = 2)

# Superimpose normal pdf
spl_mean <- mean(spl_many)
spl_se <- sd(spl_many)
ggplot(tibble(delta_p = spl_many), aes(delta_p)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.025) +
  geom_vline(xintercept = delta_p_pop, color = "firebrick", linewidth = 2) +
  stat_function(
    fun = \(x) dnorm(x, mean = spl_mean, sd = spl_se),
    color = "cornflowerblue",
    linewidth = 1.5
  ) +
  labs(x = "Difference in proportions", y = "Density")

ggsave(
  "course-materials/lecture-slides/scratch/week4/hist_norm.png",
  width = 20,
  height = 20,
  units = "cm"
)

# SE of difference in proportions
norm_approx_mu <- delta_p_spl
norm_approx_se <- sqrt(p_hat1 * (1 - p_hat1) / 40 + p_hat2 * (1 - p_hat2) / 60)
ggplot() +
  geom_vline(xintercept = delta_p_spl, color = "firebrick", linewidth = 2) +
  # geom_vline(xintercept = delta_p_pop, color = "firebrick", linewidth = 2) +
  stat_function(
    fun = \(x) dnorm(x, mean = norm_approx_mu, sd = norm_approx_se),
    color = "cornflowerblue",
    linewidth = 1.5,
    xlim = c(-0.2, 0.5)
  ) +
  labs(x = "Difference in proportions", y = "Density")
ggsave(
  "course-materials/lecture-slides/scratch/week4/norm_se.png",
  width = 5,
  height = 5,
  units = "in"
)

# Confidence interval
# delta_p ~ Normal(mu, sd)
delta_p_ci <- qnorm(c(0.025, 0.975), mean = norm_approx_mu, sd = norm_approx_se)
ggplot() +
  geom_vline(xintercept = delta_p_spl, color = "firebrick", linewidth = 2) +
  geom_vline(
    xintercept = delta_p_ci,
    color = "firebrick",
    linewidth = 2,
    linetype = "dotted"
  ) +
  stat_function(
    fun = \(x) dnorm(x, mean = norm_approx_mu, sd = norm_approx_se),
    color = "cornflowerblue",
    linewidth = 1.5,
    xlim = c(-0.2, 0.5)
  ) +
  labs(x = "Difference in proportions", y = "Density")
ggsave(
  "course-materials/lecture-slides/scratch/week4/norm_se2.png",
  width = 5,
  height = 5,
  units = "in"
)


# Now: hypothesis test
# The null distribution is the distribution of the test statistic under the null hypothesis
# Null hypothesis: p1 = p2
p_pool <- sum(restoration$success) / nrow(restoration)
se_pooled <- sqrt(p_pool * (1 - p_pool) * (1 / 40 + 1 / 60))
ggplot() +
  stat_function(
    fun = \(x) dnorm(x, mean = 0, sd = se_pooled),
    color = "cornflowerblue",
    linewidth = 1.5,
    xlim = c(-0.5, 0.5)
  ) +
  geom_vline(xintercept = delta_p_spl, color = "firebrick", linewidth = 2) +
  labs(x = "Difference in proportions", y = "Density")
ggsave(
  "course-materials/lecture-slides/scratch/week4/null_hyp.png",
  width = 5,
  height = 5,
  units = "in"
)
pnorm(delta_p_spl, mean = 0, sd = se_pooled, lower.tail = FALSE)

permute <- function(i) {
  r <- restoration
  r$treatment <- sample(r$treatment)
  p_hat1 <- mean(r$success[r$treatment == "Garden staple"])
  p_hat2 <- mean(r$success[r$treatment == "Popsicle stick"])
  delta_p_spl <- p_hat2 - p_hat1
}
restoration_perm <- map_dbl(1:1e3, permute)
ggplot() +
  geom_histogram(
    aes(x = delta_p, y = after_stat(density)),
    tibble(delta_p = restoration_perm),
    binwidth = 0.05
  ) +
  geom_vline(
    xintercept = delta_p_spl,
    color = "firebrick",
    linewidth = 2,
    linetype = "dotted"
  ) +
  stat_function(
    fun = \(x) dnorm(x, mean = 0, sd = se_pooled),
    color = "cornflowerblue",
    linewidth = 1.5,
    xlim = c(-0.4, 0.4)
  )
