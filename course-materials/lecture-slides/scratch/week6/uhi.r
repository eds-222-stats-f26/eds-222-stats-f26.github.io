library(ggdag)
library(tidyverse)
theme_set(theme_classic(18))

# urban heat islands
# Does density increase temperatures in UHIs?

# Density, veg, and temperature
uhi_dag_dvt <- dagify(
  V ~ D, # Vegetation decreases with density
  T ~ V # Temperature decreased by vegetation and increased by height
)

set.seed(12)
ggdag(uhi_dag_dvt) +
  theme_void(18)
ggsave(
  "course-materials/lecture-slides/scratch/week6/dag_dvt.png",
  height = 5,
  width = 5
)


uhi_dag_full <- dagify(
  V ~ D, # Vegetation decreases with density
  D ~ W, # Density increased by proximity to water
  E ~ T + D, # Energy usage increases with temperature and decreases with density
  T ~ V + W # Temperature decreased by vegetation and proximity to water
)

set.seed(22)
ggdag(uhi_dag_full) +
  theme_void(18)
ggsave(
  "course-materials/lecture-slides/scratch/week6/dag_full.png",
  height = 5,
  width = 5
)


set.seed(42)
pop <- tibble(
  w = runif(100, 0, 5),
  d = rnorm(100, 20 + 5 * w, sd = 10),
  v = rnorm(100, 50 - 0.5 * d, sd = 6),
  t = rnorm(100, 80 - 1 * v - 4 * w, sd = 3),
  e = rnorm(100, 25 - 3 * d + 1.5 * t, sd = 4)
)
ggplot(pop, aes(d, t)) +
  geom_point(shape = 21, stroke = 2) +
  labs(x = "Population density", y = "Temperature")
ggsave(
  "course-materials/lecture-slides/scratch/week6/dt.png",
  height = 5,
  width = 5
)

ggplot(pop, aes(w, d)) +
  geom_point(shape = 21, stroke = 2) +
  labs(x = "Nearby water", y = "Density")

ggplot(pop, aes(d, v)) +
  geom_point(shape = 21, stroke = 2)

ggplot(pop, aes(v, t)) +
  geom_point(shape = 21, stroke = 2)

summary(lm(t ~ w + d, pop))
confint(lm(t ~ w + d, pop))


summary(lm(t ~ d, pop))

summary(lm(t ~ w + d + e, pop))
confint(lm(t ~ w + d + e, pop))
