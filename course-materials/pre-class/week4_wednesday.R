library(tidyverse)
theme_set(theme_bw(18))

# load the soil core data
soil_cores <- read_csv("data/exide-soil-cores.csv") |>
  filter(pb_ppm <= 5e3) |>
  left_join(
    read_csv("data/exide-property-covariates.csv"),
    by = "property_id"
  ) |>
  mutate(home_age = 2026 - year_built) |>
  drop_na(pb_ppm, home_age, location_code)

# show the home age x sample location data
ggplot(soil_cores, aes(home_age, pb_ppm, color = location_code)) +
  geom_point(shape = 21, alpha = 0.5) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Home age (years)", y = "Soil Pb (ppm)", color = "Sample location") +
  theme(legend.position = "bottom")

# fit the model and get the summary
age_location_lm <- lm(pb_ppm ~ home_age * location_code, soil_cores)
summary(age_location_lm)

# show the model lines on the figure
coefs <- coef(age_location_lm)
model_lines <- tibble(
  location_code = c("DL", "YD"),
  intercept = c(coefs[1], coefs[1] + coefs[3]),
  slope = c(coefs[2], coefs[2] + coefs[4])
)

ggplot(soil_cores, aes(home_age, pb_ppm, color = location_code)) +
  geom_point(shape = 21, alpha = 0.1) +
  geom_abline(
    aes(intercept = intercept, slope = slope, color = location_code),
    model_lines,
    linewidth = 1.5
  ) +
  scale_color_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = "Home age (years)", y = "Soil Pb (ppm)", color = "Sample location") +
  theme(legend.position = "bottom")
