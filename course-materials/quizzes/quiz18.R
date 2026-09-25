library(tidyverse)
theme_set(theme_bw(14))

plot_p <- function(beta0, beta1, x) {
  logit_p <- beta0 + beta1 * x
  p <- exp(logit_p) / (exp(logit_p) + 1)
  ggplot(tibble(x, p), aes(x, p)) +
    geom_line() +
    ylim(0, 1)
}

x <- seq(-2, 2, length.out = 1e3)

cowplot::plot_grid(
  plot_p(1, -2, x),
  plot_p(-1, 1, x),
  plot_p(-1, -1, x),
  plot_p(1, -1, x),
  plot_p(-1, 2, x),
  plot_p(1, 1, x),
  nrow = 2,
  labels = "AUTO"
)
ggsave("course-materials/quizzes/quiz18.png", width = 6, height = 4)
