library(tidyverse)
theme_set(theme_classic(18))

# PMF
binom_n1_pmf <- expand_grid(
  x = 0:1,
  p = c(0.2, 0.5, 0.8)
) %>%
  mutate(mass = dbinom(x, 1, p))
ggplot(binom_n1_pmf, aes(x, mass)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1.5) +
  geom_point(shape = 21, stroke = 2, fill = "white") +
  scale_x_continuous(breaks = c(0, 1), limits = c(-0.2, 1.2)) +
  ylim(0, 1) +
  facet_grid(cols = vars(p))
# Without data, just axes
ggplot(binom_n1_pmf, aes(x, mass)) +
  geom_segment(aes(xend = x, yend = 0), linewidth = 1.5, alpha = 0) +
  geom_point(shape = 21, stroke = 2, fill = "white", alpha = 0) +
  scale_x_continuous(breaks = c(0, 1), limits = c(-0.2, 1.2)) +
  ylim(0, 1) +
  facet_grid(cols = vars(p))
ggsave(
  "course-materials/lecture-slides/scratch/week7/blank_pmf.png",
  width = 5,
  height = 5,
  units = "in"
)

# Statistical notation
set.seed(1)
x <- runif(20, 10, 30)
y <- rnorm(20, 300 - 3.5 * x, 40)
ggplot(tibble(x, y), aes(x, y)) +
  geom_point(shape = 21, stroke = 2)
ggsave(
  "course-materials/lecture-slides/scratch/week7/scatter.png",
  width = 5,
  height = 5,
  units = "in"
)


# Constrained parameters
beta0 <- 0.2
beta1 <- 0.01
tibble(PctHisp = seq(0, 100, by = 0.2), p = beta0 + beta1 * PctHisp) %>%
  ggplot(aes(PctHisp, p)) +
  geom_line() +
  labs(y = "Prob. of Violation") +
  ylim(0, NA)
ggsave(
  "course-materials/lecture-slides/scratch/week7/bad_p.png",
  width = 5,
  height = 5,
  units = "in"
)
tibble(PctHisp = seq(0, 100, by = 0.2), p = beta0 + beta1 * PctHisp) %>%
  ggplot(aes(PctHisp, p)) +
  geom_line(alpha = 0) +
  labs(y = "Prob. of Violation") +
  ylim(0, 1)
ggsave(
  "course-materials/lecture-slides/scratch/week7/how_p.png",
  width = 5,
  height = 5,
  units = "in"
)

beta0 <- -2
beta1 <- 0.05
tibble(PctHisp = seq(0, 100, by = 0.2), p = plogis(beta0 + beta1 * PctHisp)) %>%
  ggplot(aes(PctHisp, p)) +
  geom_line() +
  labs(y = "Prob. of Violation") +
  ylim(0, NA)

# Logit(p)
tibble(p = seq(0.001, 0.999, length.out = 1e4), logit_p = log(p / (1 - p))) %>%
  ggplot(aes(p, logit_p)) +
  geom_line(alpha = 0) +
  labs(x = expression(p), y = expression(log(frac(p, 1 - p))))
ggsave(
  "course-materials/lecture-slides/scratch/week7/logit_p.png",
  width = 5,
  height = 5,
  units = "in"
)

p <- seq(0.001, 0.999, length.out = 1e4)
logit_p <- log(p / (1 - p))
ggplot(tibble(p, logit_p), aes(p, logit_p)) +
  geom_line()

x <- seq(0, 100, length.out = 1e4)
beta0 <- -5
beta1 <- 0.1
logit_p <- beta0 + beta1 * x
ggplot(tibble(x, logit_p), aes(x, logit_p)) +
  geom_line()
ggplot(tibble(x, logit_p), aes(x, logit_p)) +
  geom_line(alpha = 0) +
  labs(x = expression(x), y = expression(log(frac(p, 1 - p))))
ggsave(
  "course-materials/lecture-slides/scratch/week7/logit_p_x.png",
  width = 5,
  height = 5,
  units = "in"
)


p <- exp(logit_p) /
  (1 + exp(logit_p))
ggplot(tibble(x, p), aes(x, p)) +
  geom_line()
ggplot(tibble(x, p), aes(x, p)) +
  geom_line(alpha = 0) +
  labs(x = expression(x), y = expression(p))
ggsave(
  "course-materials/lecture-slides/scratch/week7/p_x.png",
  width = 5,
  height = 5,
  units = "in"
)

PctHisp <- seq(0, 100, length.out = 1e3)
beta0 <- -7
beta1 <- 0.1
logit_p <- beta0 + beta1 * PctHisp
p <- exp(logit_p) / (1 + exp(logit_p))
ggplot(tibble(PctHisp, p), aes(PctHisp, p)) +
  geom_line()
