library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

carwash <- expand_grid(
  hour = 7:19,
  day = 0:4
) %>%
  mutate(
    x = day + hour / 24,
    customers = rpois(nrow(.), lambda = 3.2)
  )

p1 <- ggplot(carwash, aes(x, customers)) +
  geom_col() +
  scale_x_continuous("Day", limits = c(0, 5)) +
  labs(y = "Customers (hourly)")

p2 <- ggplot(carwash, aes(customers)) +
  geom_bar() +
  labs(x = "Customers (hourly)", y = "Count")

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/carwash.png",
  width = 8,
  height = 4,
  units = "in"
)


stars <- tibble(
  sector = factor(paste0("Sector ", 1:25), levels = paste0("Sector ", 1:25))
) %>%
  mutate(
    n_stars = rpois(nrow(.), lambda = 12),
    xy = map(n_stars, \(.n) {
      tibble(star = seq(.n), x = runif(.n), y = runif(.n))
    })
  ) %>%
  unnest(xy)

p1 <- ggplot(stars, aes(x, y)) +
  geom_point(color = "gold", shape = 8) +
  facet_wrap(~sector, nrow = 5) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "black"),
    strip.background = element_blank()
  )

p2 <- stars %>%
  count(sector) %>%
  ggplot(aes(n)) +
  geom_bar(fill = "gold") +
  labs(x = "Sectors", y = "Stars") +
  theme(panel.background = element_rect(fill = "black"))

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/stars.png",
  width = 8,
  height = 4,
  units = "in"
)


p1 <- tibble(
  lambda = seq(exp(-3), exp(3), length.out = 1e3),
  log_lambda = log(lambda)
) %>%
  ggplot(aes(log_lambda, lambda)) +
  geom_line() +
  labs(x = expression(log(lambda)), y = expression(lambda))

p2 <- tibble(
  p = seq(0.001, 0.999, length.out = 1e3),
  logit_p = log(p / (1 - p))
) %>%
  ggplot(aes(logit_p, p)) +
  geom_line() +
  labs(x = expression(logit(p)), y = expression(p))

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/links.png",
  width = 8,
  height = 4,
  units = "in"
)
