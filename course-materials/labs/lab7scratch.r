library(tidyverse)
drinking_water <- read_csv(
  "course-materials/labs/data/drinking_water_orig.csv"
) %>%
  select(
    pwsid,
    pwsname,
    pctpov = percpov,
    pcthisp,
    health,
    medianincomehouse
  ) %>%
  drop_na(health, pcthisp, pctpov) %>%
  mutate(
    violation = health > 0,
    pcthisp_bin = cut(pcthisp, seq(-10, 110, by = 10)),
    pctpov_bin = cut(pctpov, seq(-10, 110, by = 10))
  )
write_csv(drinking_water, "course-materials/labs/data/drinking_water.csv")

violation_pcthisp <- drinking_water %>%
  group_by(pcthisp_bin) %>%
  summarize(violation_rate = mean(violation), pcthisp_n = n())

violation_pctpov <- drinking_water %>%
  group_by(pctpov_bin) %>%
  summarize(violation_rate = mean(violation), pctpov_n = n())

ggplot(drinking_water, aes(health)) +
  geom_bar()
ggplot(drinking_water, aes(pcthisp, violation)) +
  geom_point()

# Violation rate increases with pct hisp
ggplot(violation_pcthisp, aes(pcthisp_bin, violation_rate)) +
  geom_point(aes(size = pcthisp_n))

# Violation rate increases with poverty
ggplot(violation_pctpov, aes(pctpov_bin, violation_rate)) +
  geom_point(aes(size = pctpov_n))

full_mod <- glm(
  violation ~ pcthisp * pctpov,
  drinking_water,
  family = binomial(link = "logit")
)

summary(full_mod)

violation_grid <- expand_grid(
  pcthisp = 0:100,
  pctpov = 0:100
)
linkinv <- full_mod$family$linkinv
violation_pred <- violation_grid %>%
  mutate(
    violation_link = predict(full_mod, newdata = violation_grid, type = "link"),
    violation_se = predict(
      full_mod,
      newdata = violation_grid,
      type = "response",
      se.fit = TRUE
    )$se.fit,
    violation = linkinv(violation_link),
    violation_lwr = linkinv(violation_link - 1.96 * violation_se),
    violation_upr = linkinv(violation_link + 1.96 * violation_se)
  )

ggplot(violation_pred, aes(pcthisp, pctpov, fill = violation)) +
  geom_raster() +
  scale_fill_distiller(palette = "RdYlBu") +
  labs(
    x = "% Hispanic Population",
    y = "% Below Poverty Line",
    fill = "Prob. of Violation"
  ) +
  theme_classic(16) +
  theme(legend.position = "bottom", aspect.ratio = 1)

violation_pred %>%
  filter(pctpov %in% c(0, 50, 75, 100)) %>%
  ggplot(aes(pcthisp, violation)) +
  geom_ribbon(
    aes(
      fill = pctpov,
      group = pctpov,
      ymin = violation_lwr,
      ymax = violation_upr
    ),
    alpha = 0.2
  ) +
  geom_line(aes(color = pctpov, group = pctpov), linewidth = 1) +
  scale_color_distiller(
    palette = "RdYlBu",
    guide = guide_legend(title.position = "top")
  ) +
  scale_fill_distiller(
    palette = "RdYlBu",
    guide = guide_legend(title.position = "top")
  ) +
  labs(
    x = "% Hispanic Population",
    y = "Prob. of Violation",
    color = "% Below Poverty Line",
    fill = "% Below Poverty Line"
  ) +
  theme_classic(18) +
  theme(legend.position = "bottom")


# Student pre-class activity ----------------------------------------------
library(tidyverse)
drinking_water <- read_csv("course-materials/labs/data/drinking_water.csv")
# violation ~ Binomial(1, p)
# p = beta0 + beta1 * pcthisp
beta0 <- 0.2
beta1 <- 0.01
pcthisp <- seq(0, 100, by = 25)
rbinom(1e3, size = 1, prob = beta0 + beta1 * pcthisp)
