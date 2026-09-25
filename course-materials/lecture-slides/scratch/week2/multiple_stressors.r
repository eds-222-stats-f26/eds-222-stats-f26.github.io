library(tidyverse)
theme_set(theme_classic(18))
set.seed(321)

# Oak example

# Number of oaks
n <- 60

# simulate data
beta_0 <- 2.5 # intercept
beta_1 <- -1.5 # soil = rocky
beta_2 <- 6 # moisture
sigma <- 0.6
oak_data <- tibble(
  soil = rep(c("Loamy", "Rocky"), each = n / 2),
  moisture_vwc = rbeta(
    n,
    shape1 = ifelse(soil == "Loamy", 4.3, 3.2),
    shape2 = ifelse(soil == "Loamy", 12, 40)
  ),
  growth_mean = beta_0 + beta_1 * (soil == "Rocky") + beta_2 * moisture_vwc,
  growth_mm_yr = rnorm(n, growth_mean, sigma)
)


# growth by soil
ggplot(oak_data, aes(soil, growth_mm_yr)) +
  geom_jitter(width = 0.2) +
  labs(x = "Soil type", y = "Growth (mm yr^-1)")

summary(lm(growth_mm_yr ~ soil, data = oak_data))


## Example for students
library(palmerpenguins)
ggplot(drop_na(penguins, sex), aes(sex, body_mass_g)) +
  geom_jitter(width = 0.2) +
  labs(x = "Penguin sex", y = "Body mass (g)")

summary(lm(body_mass_g ~ sex, data = penguins))

ggplot(drop_na(penguins, species, body_mass_g), aes(species, body_mass_g)) +
  geom_jitter(width = 0.2) +
  labs(x = "Species", y = "Body mass (g)")

summary(lm(body_mass_g ~ species, data = penguins))


betas <- tibble(
  soil = c("Loamy", "Rocky"),
  beta_0 = c(beta_0, beta_0 + beta_1),
  beta_2 = c(beta_2, beta_2)
)

# Continuous predictor
ggplot(oak_data, aes(moisture_vwc, growth_mm_yr)) +
  geom_point(shape = 21, size = 2, stroke = 1) +
  labs(x = "Moisture (VWC)", y = "Growth (mm yr^-1)") +
  expand_limits(x = 0, y = 0)

ggplot(oak_data, aes(moisture_vwc, growth_mm_yr, color = soil)) +
  geom_point(shape = 21, size = 2, stroke = 1) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Moisture (VWC)", y = "Growth (mm yr^-1)") +
  expand_limits(x = 0, y = 0) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 0),
    legend.justification = c(0.95, -0.05),
    legend.title = element_blank()
  )


# Example OLS regression
oak_mod1 <- lm(growth_mm_yr ~ moisture_vwc, data = oak_data)
summary(oak_mod1)
oak_mod2 <- lm(growth_mm_yr ~ moisture_vwc + soil, data = oak_data)
summary(oak_mod2)

oak_pred <- expand_grid(
  moisture_vwc = range(oak_data$moisture_vwc),
  soil = unique(oak_data$soil)
) %>%
  mutate(growth_mm_yr = predict(oak_mod2, .))
ggplot(oak_data, aes(moisture_vwc, growth_mm_yr, color = soil)) +
  geom_point(shape = 21, size = 2, stroke = 1) +
  geom_line(data = oak_pred, linewidth = 1) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Moisture (VWC)", y = "Growth (mm yr^-1)") +
  expand_limits(x = 0, y = 0) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 0),
    legend.justification = c(0.95, -0.05),
    legend.title = element_blank()
  )


# Confounding variables (Ditch)
penguins2 <- filter(penguins, species != "Chinstrap")
ggplot(penguins2, aes(body_mass_g, bill_depth_mm)) +
  geom_point(shape = 21, size = 2, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  labs(x = "Body mass (g)", y = "Bill length (mm)")

summary(lm(bill_depth_mm ~ body_mass_g, data = penguins2))

ggplot(penguins2, aes(body_mass_g, bill_depth_mm, color = species)) +
  geom_point(shape = 21, size = 2, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, formula = y ~ x) +

  labs(x = "Body mass (g)", y = "Bill length (mm)") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(0.95, 0.95),
    legend.title = element_blank()
  )

summary(lm(bill_length_mm ~ body_mass_g + species, data = penguins2))
