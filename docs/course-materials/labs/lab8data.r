library(climateR)
library(sf)
library(tidyverse)
library(terra)
library(tidyterra)

# California shapefile
usa <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
california <- filter(usa, ID == "california")

# Download VPD (vapor pressure deficit) from gridMET
# Limit to June-August in 1980-2024
vpd <- purrr::map(
  1980:2024,
  \(yr) {
    getGridMET(
      california,
      "vpd",
      startDate = str_glue("{yr}-06-01"),
      endDate = str_glue("{yr}-08-31"),
      verbose = TRUE,
      dryrun = FALSE
    )$daily_mean_vapor_pressure_deficit
  }
) %>%
  rast() %>%
  mask(mask = california)

# Aggregate VPD data to the annual mean
vpd_summer <- tapp(vpd, index = rep(1980:2024, each = 92), fun = "mean")
names(vpd_summer) <- 1980:2024
writeRaster(vpd_summer, here::here("course-materials/labs/data/vpd.tiff"))

# Large wildfires (>= 10,000 acres)
wildfires <- vect("~/Downloads/fire24_1.gdb/", "firep24_1")
wildfires_lg <- wildfires %>%
  mutate(
    month = lubridate::month(ALARM_DATE),
    fire_year = YEAR_ + ifelse(month <= 8, -1, 0)
  ) %>%
  filter(between(fire_year, 1980, 2024), GIS_ACRES >= 1e4) %>%
  select(
    fire_name = FIRE_NAME,
    fire_year,
    month,
    acres = GIS_ACRES,
    alarm_date = ALARM_DATE,
    cont_date = CONT_DATE,
    cause = CAUSE
  )
writeVector(wildfires_lg, "course-materials/labs/data/wildfires.shp")

# Tabular format
wildfires_annual <- wildfires_lg %>%
  as_tibble() %>%
  group_by(fire_year) %>%
  summarize(n_fires = n()) %>%
  complete(fire_year = 1980:2023, fill = list(n_fires = 0)) %>%
  drop_na(fire_year)
wildfire_weather <- wildfires_annual %>%
  mutate(mean_vpd_kpa = global(vpd_summer, mean, na.rm = TRUE)$mean)
write_csv(wildfire_weather, "course-materials/labs/data/wildfires.csv")

# Zip the files up
wildfire_files <- dir("course-materials/labs/data", pattern = "wildfires")
setwd("course-materials/labs/data/")
zip("wildfires.zip", c(wildfire_files, "vpd.tiff"))
setwd("../../..")

# Visualizations
ggplot(wildfire_weather, aes(mean_vpd_kpa, n_fires)) +
  geom_point(
    aes(fill = fire_year),
    shape = 21,
    stroke = 1,
    size = 4,
    color = "white"
  ) +
  ggrepel::geom_label_repel(
    aes(label = fire_year),
    filter(wildfire_weather, fire_year == 2007),
    nudge_x = -0.1
  ) +
  scale_fill_steps2(
    midpoint = 2002,
    low = "navy",
    mid = "gold",
    high = "firebrick"
  ) +
  labs(x = "Mean VPD (kPa)", y = "Fires >10k acres (n)", fill = "Year") +
  theme_classic(14)
ggsave(
  "course-materials/labs/images/vpd_fires.png",
  height = 4,
  width = 6,
  units = "in"
)

vpd_subset <- vpd_summer[[c(1, 21, 41)]]
ggplot() +
  geom_spatraster(data = vpd_subset, na.rm = TRUE) +
  facet_wrap(~lyr, nrow = 1) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(limits = c(-125, -113), breaks = seq(-125, -113, by = 4)) +
  scale_y_continuous(limits = c(31, 43), breaks = seq(31, 43, by = 4)) +
  labs(fill = "VPD (kPa)") +
  theme(
    legend.position = "bottom",
    panel.grid = element_line(color = "#CCCCCC88", linewidth = 0.5),
    panel.background = element_blank(),
    panel.ontop = TRUE,
    panel.spacing = unit(0.75, "cm")
  )
ggsave(
  "course-materials/labs/images/vpd_maps.png",
  height = 4,
  width = 8,
  units = "in"
)

fire_subset <- wildfires_lg %>%
  filter(fire_year %in% c(1980, 2000, 2020))
ggplot() +
  geom_spatvector(data = california, fill = "#4DDDDD99") +
  geom_spatvector(data = fire_subset, fill = "firebrick", color = NA) +
  facet_wrap(~fire_year, nrow = 1) +
  scale_x_continuous(limits = c(-125, -113), breaks = seq(-125, -113, by = 4)) +
  scale_y_continuous(limits = c(31, 43), breaks = seq(31, 43, by = 4)) +
  theme(
    panel.grid = element_line(color = "#CCCCCC88", linewidth = 0.5),
    panel.background = element_blank(),
    panel.ontop = TRUE,
    panel.spacing = unit(0.75, "cm")
  )
ggsave(
  "course-materials/labs/images/fire_maps.png",
  height = 4,
  width = 8,
  units = "in"
)

# Prediction visualization
wildfire_weather <- read_csv("course-materials/labs/data/wildfires.csv")
fire_mod_pois <- glm(
  n_fires ~ mean_vpd_kpa,
  data = wildfire_weather,
  family = poisson(link = "log")
)
seq_rng <- function(x, n) {
  seq(min(x), max(x), length.out = n)
}
fire_grid <- expand_grid(
  mean_vpd_kpa = seq_rng(wildfire_weather$mean_vpd_kpa, 1000)
)
pred_se <- predict(
  fire_mod_pois,
  newdata = fire_grid,
  type = "link",
  se.fit = TRUE
)
fire_pred_pois <- fire_grid %>%
  mutate(
    n_fires = exp(pred_se$fit),
    n_fires_lwr = exp(qnorm(0.025, pred_se$fit, pred_se$se.fit)),
    n_fires_upr = exp(qnorm(0.975, pred_se$fit, pred_se$se.fit)),
    n_fires_pi_lwr = qpois(0.025, n_fires),
    n_fires_pi_upr = qpois(0.975, n_fires)
  )
p1 <- ggplot(wildfire_weather, aes(mean_vpd_kpa, n_fires)) +
  geom_point(
    aes(fill = fire_year),
    shape = 21,
    stroke = 1,
    size = 4,
    color = "white"
  ) +
  geom_ribbon(
    aes(ymin = n_fires_lwr, ymax = n_fires_upr),
    data = fire_pred_pois,
    alpha = 0.2
  ) +
  geom_line(data = fire_pred_pois, linewidth = 2, color = "cornflowerblue") +
  ggrepel::geom_label_repel(
    aes(label = fire_year),
    filter(wildfire_weather, fire_year == 2007),
    nudge_x = -0.1
  ) +
  scale_fill_steps2(
    midpoint = 2002,
    low = "navy",
    mid = "gold",
    high = "firebrick"
  ) +
  labs(x = "Mean VPD (kPa)", y = "Fires >10k acres (n)", fill = "Year") +
  theme_classic(14)
p1
ggsave(
  "course-materials/labs/images/fire_preds.png",
  height = 4,
  width = 6,
  units = "in"
)

p1 +
  geom_ribbon(
    aes(ymin = n_fires_pi_lwr, ymax = n_fires_pi_upr),
    data = fire_pred_pois,
    fill = "magenta",
    alpha = 0.2
  )
ggsave(
  "course-materials/labs/images/fire_preds_pi.png",
  height = 4,
  width = 6,
  units = "in"
)


fire_mod_lm <- lm(n_fires ~ mean_vpd_kpa, wildfire_weather)
fire_pred_lm <- fire_grid %>%
  mutate(
    n_fires = predict(fire_mod_lm, newdata = .),
    n_fires_lwr = predict(
      fire_mod_lm,
      newdata = fire_grid,
      interval = "confidence"
    )[, "lwr"],
    n_fires_upr = predict(
      fire_mod_lm,
      newdata = fire_grid,
      interval = "confidence"
    )[, "upr"],
    n_fires_pi_lwr = qnorm(0.025, n_fires, summary(fire_mod_lm)$sigma),
    n_fires_pi_upr = qnorm(0.975, n_fires, summary(fire_mod_lm)$sigma)
  )
ggplot(wildfire_weather, aes(mean_vpd_kpa, n_fires)) +
  geom_point(
    aes(fill = fire_year),
    shape = 21,
    stroke = 1,
    size = 4,
    color = "white"
  ) +
  geom_ribbon(
    aes(ymin = n_fires_lwr, ymax = n_fires_upr),
    data = fire_pred_lm,
    alpha = 0.2
  ) +
  geom_line(data = fire_pred_lm, linewidth = 2, color = "cornflowerblue") +
  geom_ribbon(
    aes(ymin = n_fires_pi_lwr, ymax = n_fires_pi_upr),
    data = fire_pred_lm,
    fill = "magenta",
    alpha = 0.2
  ) +
  ggrepel::geom_label_repel(
    aes(label = fire_year),
    filter(wildfire_weather, fire_year == 2007),
    nudge_x = -0.1
  ) +
  scale_fill_steps2(
    midpoint = 2002,
    low = "navy",
    mid = "gold",
    high = "firebrick"
  ) +
  labs(x = "Mean VPD (kPa)", y = "Fires >10k acres (n)", fill = "Year") +
  theme_classic(14)
