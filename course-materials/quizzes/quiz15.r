library(tidyverse)

# Shrimp aquaculture example
set.seed(42)
beta0 <- -2.4
beta1 <- 5
sigma <- 2
shrimp_aqua <- tibble(
  ammonia_mg_l = rnorm(50, mean = 0.5, sd = 0.2),
  shrimp_survival = rnorm(50, beta0 + beta1 * ammonia_mg_l, sigma)
)
ggplot(shrimp_aqua, aes(ammonia_mg_l, shrimp_survival)) +
  geom_point(shape = 21, stroke = 1.5) +
  geom_smooth(method = "lm", fill = "grey50", alpha = 0.5, color = "grey10") +
  labs(x = expression(Ammonia ~ (mg ~ l^-1)), y = "Shrimp survival")
ggsave(
  "course-materials/lecture-slides/scratch/week5/shrimp_aqua_bw.png",
  width = 5,
  height = 5,
  units = "in"
)

shrimp_aqua_lm <- lm(shrimp_survival ~ ammonia_mg_l, shrimp_aqua)
summary(shrimp_aqua_lm)
confint(shrimp_aqua_lm)
