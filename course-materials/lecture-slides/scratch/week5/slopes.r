library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

# Phytoplankton shells fall apart as pH rises

# Our "population"
co2_umol_l <- rnorm(200, mean = 20, sd = 5)
beta0 <- 0.8
beta1 <- -0.01
sigma <- 0.1
calcite_poc <- rnorm(200, beta0 + beta1 * co2_umol_l, sd = sigma)

g_huxleyi <- tibble(
  co2_umol_l,
  calcite_poc
)

ggplot(g_huxleyi, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "cornflowerblue",
    linewidth = 2
  ) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi.png",
  width = 5,
  height = 5,
  units = "in"
)

# What's the model?
g_huxleyi_lm <- lm(calcite_poc ~ co2_umol_l, g_huxleyi)
summary(g_huxleyi_lm)

# What if we sample 30?
g_huxleyi_sample <- g_huxleyi %>%
  slice_sample(n = 30)
ggplot(g_huxleyi_sample, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(
    data = filter(
      g_huxleyi,
      between(
        co2_umol_l,
        min(g_huxleyi_sample$co2_umol_l),
        max(g_huxleyi_sample$co2_umol_l)
      )
    ),
    method = "lm",
    se = FALSE,
    color = "cornflowerblue",
    linewidth = 2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_sample.png",
  width = 5,
  height = 5,
  units = "in"
)
g_huxleyi_lm2 <- lm(calcite_poc ~ co2_umol_l, g_huxleyi_sample)
summary(g_huxleyi_lm2)

# Do it 1000 times
sample_models <- replicate(1e4, {
  g_huxleyi_sample <- g_huxleyi %>%
    slice_sample(n = 30)
  g_huxleyi_lm2 <- lm(calcite_poc ~ co2_umol_l, g_huxleyi_sample)
  coef(g_huxleyi_lm2)
})
sample_models_df <- expand_grid(
  co2_umol_l = range(g_huxleyi$co2_umol_l),
  iter = seq(ncol(sample_models))
) %>%
  mutate(
    beta0 = sample_models[1, iter],
    beta1 = sample_models[2, iter],
    calcite_poc = beta0 + beta1 * co2_umol_l
  )

ggplot(g_huxleyi, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5, color = "grey70") +
  geom_line(
    aes(group = iter),
    filter(sample_models_df, iter %in% sample(1:1e4, 15)),
    color = "firebrick",
    linewidth = 1.5,
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "cornflowerblue",
    linewidth = 2
  ) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_samples.png",
  width = 5,
  height = 5,
  units = "in"
)

# Hypothesis testing
# Question: is there a relationship between these two variables?
# What are the null hypothesis and alternative hypotheses?
# H0: beta1 = 0, HA: beta1 != 0

ggplot(g_huxleyi_sample, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_sample_only.png",
  width = 5,
  height = 5,
  units = "in"
)

# Randomization method
# What variable would you permute?

p1 <- ggplot(g_huxleyi_sample, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
g_huxleyi_permute <- g_huxleyi_sample
g_huxleyi_permute$co2_umol_l <- sample(g_huxleyi_permute$co2_umol_l)
p2 <- ggplot(g_huxleyi_permute, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_permute.png",
  width = 10,
  height = 5,
  units = "in"
)

null_beta1 <- map_dbl(1:1e4, \(i) {
  permute_df <- g_huxleyi_sample
  permute_df$co2_umol_l <- sample(permute_df$co2_umol_l)
  permute_lm <- lm(calcite_poc ~ co2_umol_l, permute_df)
  coef(permute_lm)[2]
})

ggplot(tibble(null_beta1), aes(null_beta1)) +
  geom_histogram(binwidth = 0.002) +
  geom_vline(
    xintercept = coef(g_huxleyi_lm2)[2],
    color = "firebrick",
    linewidth = 2
  ) +
  labs(x = expression(beta[1]))
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_null.png",
  width = 5,
  height = 5,
  units = "in"
)

summary(g_huxleyi_lm2)

# Confidence intervals
# Question: how strong is the relationship between these two variables?

# What's the CI of the coefficient?
# Mathematical model
summary(g_huxleyi_lm2)$coef
beta1_est <- summary(g_huxleyi_lm2)$coef[2, 1]
beta1_se <- summary(g_huxleyi_lm2)$coef[2, 2]
beta1_dens <- tibble(
  beta1 = seq(-0.022, 0.005, length.out = 100),
  density = dnorm(beta1, mean = beta1_est, sd = beta1_se)
)
confint(g_huxleyi_lm2)
ggplot(beta1_dens, aes(beta1, density)) +
  geom_line() +
  geom_vline(xintercept = beta1_est, color = "firebrick", linewidth = 2) +
  geom_vline(
    xintercept = confint(g_huxleyi_lm2)[2, ],
    color = "firebrick",
    linewidth = 2,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = qnorm(c(0.025, 0.975), mean = beta1_est, sd = beta1_se),
    color = "firebrick",
    linewidth = 2,
    linetype = "dotted"
  ) +
  labs(x = expression(beta[1]))
ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_ci.png",
  width = 5,
  height = 5,
  units = "in"
)

# What's the CI of the mean?
g_huxleyi_grid <- tibble(
  co2_umol_l = seq(
    min(g_huxleyi_sample$co2_umol_l),
    max(g_huxleyi_sample$co2_umol_l),
    length.out = 100
  )
)
g_huxleyi_mean <- predict(
  g_huxleyi_lm2,
  newdata = g_huxleyi_grid,
  se.fit = TRUE,
  interval = "confidence"
)$fit %>%
  as_tibble() %>%
  rename(calcite_poc = fit, calcite_poc_lwr = lwr, calcite_poc_upr = upr) %>%
  cbind(g_huxleyi_grid)

ggplot(g_huxleyi_sample, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_ribbon(
    aes(ymin = calcite_poc_lwr, ymax = calcite_poc_upr),
    g_huxleyi_mean,
    fill = "firebrick",
    alpha = 0.3
  ) +
  geom_line(data = g_huxleyi_mean, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")

ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_ci_mean.png",
  width = 5,
  height = 5,
  units = "in"
)

g_huxleyi_pred <- predict(
  g_huxleyi_lm2,
  newdata = g_huxleyi_grid,
  se.fit = TRUE,
  interval = "prediction"
)$fit %>%
  as_tibble() %>%
  rename(calcite_poc = fit, calcite_poc_lwr = lwr, calcite_poc_upr = upr) %>%
  cbind(g_huxleyi_grid)
ggplot(g_huxleyi_sample, aes(co2_umol_l, calcite_poc)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_ribbon(
    aes(ymin = calcite_poc_lwr, ymax = calcite_poc_upr),
    g_huxleyi_pred,
    fill = "firebrick",
    alpha = 0.1
  ) +
  geom_ribbon(
    aes(ymin = calcite_poc_lwr, ymax = calcite_poc_upr),
    g_huxleyi_mean,
    fill = "firebrick",
    alpha = 0.3
  ) +
  geom_line(data = g_huxleyi_mean, color = "firebrick", linewidth = 2) +
  labs(x = expression("CO"[2] ~ (mu * "mol l"^-1)), y = "Calcite / POC")

ggsave(
  "course-materials/lecture-slides/scratch/week5/g_huxleyi_ci_pred.png",
  width = 5,
  height = 5,
  units = "in"
)

# Shrimp aquaculture example
beta0 <- 0.50
beta1 <- -0.15
sigma <- 0.03
shrimp_aqua <- tibble(
  ammonia_mg_l = rnorm(50, mean = 0.5, sd = 0.2),
  shrimp_survival = rnorm(50, beta0 + beta1 * ammonia_mg_l, sigma)
)
ggplot(shrimp_aqua, aes(ammonia_mg_l, shrimp_survival)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(
    method = "lm",
    fill = "firebrick",
    alpha = 0.2,
    color = "firebrick"
  ) +
  labs(x = expression(Ammonia ~ (mg ~ l^-1)), y = "Shrimp survival")
ggsave(
  "course-materials/lecture-slides/scratch/week5/shrimp_aqua.png",
  width = 5,
  height = 5,
  units = "in"
)

shrimp_aqua_lm <- lm(shrimp_survival ~ ammonia_mg_l, shrimp_aqua)
summary(shrimp_aqua_lm)
confint(shrimp_aqua_lm)
