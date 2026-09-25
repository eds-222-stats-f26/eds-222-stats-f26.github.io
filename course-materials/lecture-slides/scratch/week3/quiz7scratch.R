library(tidyverse)
set.seed(123)

# quiz 7
x1 <- rbinom(6, 1, 0.5)
x2 <- rnorm(6, mean = 1.5 * x1, sd = 1)
y <- rnorm(6, mean = -4 * x1 + x2, sd = 1)

p1 <- tibble(x1, x2, y) %>%
  ggplot(aes(x2, y)) +
  geom_point(size = 2, stroke = 1.5) +
  labs(x = "x") +
  theme_classic(18) +
  theme(legend.position = "none")

p2 <- tibble(x1, x2, y) %>%
  ggplot(aes(x2, y, shape = factor(x1))) +
  geom_point(size = 2, stroke = 1.5) +
  scale_shape_manual(values = c(0, 2)) +
  labs(x = "x") +
  theme_classic(18) +
  theme(legend.position = "none")

cowplot::plot_grid(p1, p2, nrow = 1)

# quiz 8

x1 <- rep(0:1, 4)
x2 <- rnorm(8, mean = 1.5 * x1, sd = 1)
y <- rnorm(8, mean = -4 * x1 + 2 * x2 - 2 * x1 * x2, sd = 1)

p3 <- tibble(x1, x2, y) %>%
  ggplot(aes(x2, y, shape = factor(x1))) +
  geom_point(size = 2, stroke = 1.5) +
  scale_shape_manual(values = c(0, 2)) +
  labs(x = "x") +
  theme_classic(18) +
  theme(legend.position = "none")

cowplot::plot_grid(p3, p3, nrow = 1)


library(palmerpenguins)
penguins %>%
  count(species, island) %>%
  group_by(island) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = island, values_from = n, values_fill = 0)
penguins %>%
  count(species, island) %>%
  group_by(species) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = species, values_from = n, values_fill = 0)
