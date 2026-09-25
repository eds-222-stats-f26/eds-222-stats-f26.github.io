library(tidyverse)
theme_set(theme_classic(14))
set.seed(123)

# Testing CI coverage -----------------------------------------------------

# 1. Define your population
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

# Here's the distribution of our samples
tibble(p1_hat, p2_hat) %>%
  pivot_longer(
    c(p1_hat, p2_hat),
    names_to = "treatment",
    values_to = "prop_success"
  ) %>%
  ggplot(aes(prop_success)) +
  geom_histogram() +
  facet_wrap(~treatment)

# 4. Calculate the SE for the CI
se_diff_prop <- sqrt(p1_hat * (1 - p1_hat) / n1 + p2_hat * (1 - p2_hat) / n2)

# 5. Calculate the CI for each sample
ci_95_lwr <- qnorm(0.025, mean = diff_prop, sd = se_diff_prop)
ci_95_upr <- qnorm(0.975, mean = diff_prop, sd = se_diff_prop)

# 6. Calculate coverage
is_covered <- ci_95_lwr <= p2 - p1 & p2 - p1 <= ci_95_upr
coverage <- mean(is_covered)
coverage

# Visualize the CI's
tibble(sample = 1:n_samples, diff_prop, ci_95_lwr, ci_95_upr, is_covered) %>%
  sample_n(100) %>%
  ggplot(aes(
    diff_prop,
    sample,
    xmin = ci_95_lwr,
    xmax = ci_95_upr,
    color = is_covered
  )) +
  geom_pointrange()


# Testing p-value rejections ----------------------------------------------

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

# What does the null distribution look like?
tibble(diff_prop) %>%
  ggplot(aes(diff_prop)) +
  geom_histogram()

# 4. Calculate standard error for null hypothesis
p_hat <- (x1 + x2) / (n1 + n2)
se_null <- sqrt(p_hat * (1 - p_hat) * (1 / n1 + 1 / n2))

# 5. Calculate p value
pval <- pnorm(diff_prop, mean = 0, sd = se_null, lower.tail = FALSE)
mean(pval <= 0.05)

# Visualize result
tibble(diff_prop, reject = pval <= 0.05) %>%
  ggplot(aes(diff_prop, fill = reject)) +
  geom_histogram(binwidth = 0.02) +
  scale_fill_manual(
    values = c(`FALSE` = "cornflowerblue", `TRUE` = "firebrick")
  ) +
  labs(x = "Difference in proportions") +
  theme(legend.position = "none")
