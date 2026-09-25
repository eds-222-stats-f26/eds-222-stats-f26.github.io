# Does indigenous stewardship buffer forest integrity against deforestation pressure?

library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

# Distribution of intensification
x_mean <- 80
x_disp <- 0.2
x_shape <- 1 / x_disp
x_rate <- 1 / (x_mean * x_disp)
plot(\(x) dgamma(x + 40, x_shape, x_rate), from = -50, to = 200)

yield_change <- rgamma(200, x_shape, x_rate) - 40

# make indigenous stewardship inversely correlated with yield change
indigenous_steward <- rbinom(200, size = 1, prob = plogis(-yield_change / 50))

ggplot(
  tibble(yield_change, indigenous_steward),
  aes(indigenous_steward, yield_change)
) +
  geom_jitter()

# forest_loss = beta_0 + beta_1 * yield_change + beta_2 * indig_steward + beta_3 * yield_change * indig_steward

beta_0 <- -5
beta_1 <- -0.08
beta_2 <- 2
beta_3 <- 0.1
sigma <- 3

forest_data <- tibble(
  yield_change,
  indigenous_steward,
  mu = beta_0 +
    beta_1 * yield_change +
    beta_2 * indigenous_steward +
    beta_3 * yield_change * indigenous_steward,
  forest_loss = rnorm(200, mu, sigma)
)

ggplot(forest_data, aes(yield_change, forest_loss)) +
  geom_point(shape = 21, size = 1.5, stroke = 1) +
  labs(x = "Yield change (%)", y = expression("Forest loss "("km"^2 ~ "yr"^-1)))

int_mod <- lm(
  forest_loss ~ yield_change * indigenous_steward,
  data = forest_data
)
summary(int_mod)

add_mod <- lm(
  forest_loss ~ yield_change + indigenous_steward,
  data = forest_data
)
summary(add_mod)

add_pred <- expand_grid(
  yield_change = range(forest_data$yield_change),
  indigenous_steward = 0:1
) %>%
  mutate(forest_loss = predict(add_mod, .))
ggplot(
  forest_data,
  aes(yield_change, forest_loss, color = factor(indigenous_steward))
) +
  geom_point(shape = 21, size = 1.5, stroke = 1) +
  geom_line(data = add_pred, linewidth = 1) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  labs(
    x = "Yield change (%)",
    y = expression("Forest loss "("km"^2 ~ "yr"^-1))
  ) +
  theme(legend.position = "none")

ggplot(
  forest_data,
  aes(yield_change, forest_loss, color = factor(indigenous_steward))
) +
  geom_point(shape = 21, size = 1.5, stroke = 1) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  labs(
    x = "Yield change (%)",
    y = expression("Forest loss "("km"^2 ~ "yr"^-1))
  ) +
  theme(legend.position = "none")

int_pred <- expand_grid(
  yield_change = range(forest_data$yield_change),
  indigenous_steward = 0:1
) %>%
  mutate(forest_loss = predict(int_mod, .))
ggplot(
  forest_data,
  aes(yield_change, forest_loss, color = factor(indigenous_steward))
) +
  geom_point(shape = 21, size = 1.5, stroke = 1) +
  geom_line(data = int_pred, linewidth = 1) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  labs(
    x = "Yield change (%)",
    y = expression("Forest loss "("km"^2 ~ "yr"^-1))
  ) +
  theme(legend.position = "none")
