library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

restoration_n <- 100
restoration <- tibble(
  plot_id = 1:restoration_n,
  treatment = sample(
    c("Garden staple", "Popsicle stick"),
    size = restoration_n,
    replace = TRUE,
    prob = c(0.6, 0.4)
  ),
  success = rbinom(
    restoration_n,
    size = 1,
    p = ifelse(treatment == "Garden staple", 0.42, 0.55)
  ),
  shoot_density_m2 = rgamma(
    restoration_n,
    shape = ifelse(treatment == "Garden staple", 4, 5),
    rate = 0.05
  ),
  success_fct = factor(success, labels = c("Fail", "Succeed"))
)
write_csv(restoration, "course-materials/labs/data/eelgrass.csv")
summary(lm(success ~ treatment, restoration))
summary(lm(shoot_density_m2 ~ treatment, restoration))
ggplot(restoration, aes(treatment, fill = success_fct)) +
  geom_bar() +
  scale_fill_manual(values = c("firebrick", "cornflowerblue")) +
  labs(x = "Restoration method", y = "Sites", fill = "Outcome") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1)
  )
ggsave(
  "course-materials/lecture-slides/scratch/week4/restoration.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300
)

restoration %>%
  group_by(treatment) %>%
  summarize(
    n_success = sum(success),
    n_fail = sum(1 - success),
    prop_success = mean(success)
  )

grid_width <- 5
restoration_grid <- restoration %>%
  group_by(treatment) %>%
  mutate(i = row_number() - 1, x = i %% grid_width, y = i %/% grid_width) %>%
  ungroup() %>%
  mutate(x = ifelse(treatment == "Popsicle stick", x + grid_width + 1, x))
ggplot(restoration_grid, aes(x, y, color = success_fct)) +
  geom_point(size = 8) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  scale_x_continuous(
    breaks = c((grid_width - 1) / 2, (grid_width - 1) / 2 + grid_width),
    labels = c("Garden staple", "Popsicle stick")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  )

library(gganimate)
restoration_shuffle <- map(1:1000, \(t) {
  restoration_grid %>%
    mutate(treatment = sample(treatment, size = nrow(.))) %>%
    group_by(treatment) %>%
    mutate(i = row_number() - 1, x = i %% grid_width, y = i %/% grid_width) %>%
    ungroup() %>%
    mutate(
      x = ifelse(treatment == "Popsicle stick", x + grid_width + 1, x),
      trial = t
    )
}) %>%
  list_rbind()
p1 <- ggplot(
  filter(restoration_shuffle, trial <= 10),
  aes(x, y, color = success_fct)
) +
  geom_point(size = 10) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  scale_x_continuous(
    breaks = c((grid_width - 1) / 2, (grid_width - 1) / 2 + grid_width + 1),
    labels = c("Garden staple", "Popsicle stick")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  ) +
  transition_states(
    trial,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes("sine-in-out") +
  labs(title = "Shuffle {closest_state}")
p2 <- ggplot(
  filter(restoration_shuffle, trial <= 10),
  aes(treatment, fill = success_fct)
) +
  geom_bar() +
  scale_fill_manual(values = c("firebrick", "cornflowerblue")) +
  labs(x = "Restoration method", y = "Sites", fill = "Outcome") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  transition_states(
    trial,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes("sine-in-out") +
  labs(title = "Shuffle {closest_state}")
anim_save(
  "shuffle1.gif",
  animate(p1, nframes = 100, fps = 10),
  path = here::here("course-materials/lecture-slides/scratch/week4/")
)
anim_save(
  "shuffle2.gif",
  animate(p2, nframes = 100, fps = 10),
  path = here::here("course-materials/lecture-slides/scratch/week4/")
)

restoration_null <- restoration_shuffle %>%
  group_by(trial, treatment) %>%
  summarize(prop_succeess = mean(success), .groups = "drop_last") %>%
  summarize(diff_prop = prop_succeess[2] - prop_succeess[1])
restoration_observed <- restoration %>%
  group_by(treatment) %>%
  summarize(prop_succeess = mean(success), .groups = "drop") %>%
  summarize(diff_prop = prop_succeess[2] - prop_succeess[1]) %>%
  pull(diff_prop)
ggplot(restoration_null, aes(diff_prop)) +
  geom_histogram(binwidth = 0.05, color = "grey90", fill = "grey10") +
  geom_vline(
    xintercept = restoration_observed,
    color = "firebrick",
    linewidth = 2
  ) +
  labs(x = "Difference in proportion", y = "Count trials")
ggsave(
  "course-materials/lecture-slides/scratch/week4/diff_prop_dist.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300
)
restoration_p <- mean(restoration_null$diff_prop > restoration_observed)

# Bootstrapping
restoration %>%
  group_by(treatment) %>%
  mutate(success = sample(success, replace = TRUE)) %>%
  summarize(prop_success = mean(success)) %>%
  summarize(diff_prop = prop_success[2] - prop_success[1])


ggplot(restoration_grid, aes(x, y, color = success_fct)) +
  geom_point(size = 8) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  scale_x_continuous(
    breaks = c((grid_width - 1) / 2, (grid_width - 1) / 2 + grid_width),
    labels = c("Garden staple", "Popsicle stick")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  )

restoration_boot <- map(1:1000, \(t) {
  restoration %>%
    group_by(treatment) %>%
    mutate(
      plot_id = sample(plot_id, replace = TRUE),
      success = map_int(plot_id, \(id) {
        restoration$success[restoration$plot_id == id]
      }),
      success_fct = factor(success, labels = c("Fail", "Succeed")),
      i = row_number() - 1,
      x = i %% grid_width,
      y = i %/% grid_width
    ) %>%
    ungroup() %>%
    mutate(
      x = ifelse(treatment == "Popsicle stick", x + grid_width + 1, x),
      trial = t
    )
}) %>%
  list_rbind()

p3 <- ggplot(
  filter(restoration_boot, trial <= 10),
  aes(x, y, group = plot_id, color = success_fct)
) +
  geom_point(size = 8) +
  scale_color_manual(values = c("firebrick", "cornflowerblue")) +
  scale_x_continuous(
    breaks = c((grid_width - 1) / 2, (grid_width - 1) / 2 + grid_width + 1),
    labels = c("Garden staple", "Popsicle stick")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  ) +
  transition_states(
    trial,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes("sine-in-out") +
  labs(title = "Shuffle {closest_state}")

anim_save(
  "boot1.gif",
  animate(p3, nframes = 100, fps = 10),
  path = here::here("course-materials/lecture-slides/scratch/week4/")
)

p4 <- ggplot(
  filter(restoration_boot, trial <= 10),
  aes(treatment, fill = success_fct)
) +
  geom_bar() +
  scale_fill_manual(values = c("firebrick", "cornflowerblue")) +
  labs(x = "Restoration method", y = "Sites", fill = "Outcome") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1, 1)
  ) +
  transition_states(
    trial,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes("sine-in-out") +
  labs(title = "Shuffle {closest_state}")

anim_save(
  "boot2.gif",
  animate(p4, nframes = 100, fps = 10),
  path = here::here("course-materials/lecture-slides/scratch/week4/")
)

diff_prop_boot <- restoration_boot %>%
  group_by(trial, treatment) %>%
  summarize(prop_success = mean(success), .groups = "drop_last") %>%
  summarize(diff_prop = prop_success[2] - prop_success[1])
ggplot(diff_prop_boot, aes(diff_prop)) +
  geom_histogram(binwidth = 0.05, color = "grey90", fill = "grey10") +
  geom_vline(
    xintercept = restoration_observed,
    color = "firebrick",
    linewidth = 2
  ) +
  geom_vline(
    xintercept = quantile(diff_prop_boot$diff_prop, c(0.025, 0.975)),
    color = "firebrick",
    linewidth = 2,
    linetype = "dotted"
  ) +
  labs(x = "Difference in proportion", y = "Count trials")

ggsave(
  "course-materials/lecture-slides/scratch/week4/diff_prop_boot.png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300
)


# Quiz 9
binom_pmf <- tibble(
  x = 0:10,
  pmf = dbinom(x, 10, 0.70)
)
ggplot() +
  geom_segment(
    aes(x = x, y = pmf, xend = x, yend = 0),
    binom_pmf,
    linewidth = 2,
    color = "black"
  ) +
  geom_point(
    aes(x = x, y = pmf),
    binom_pmf,
    color = "black",
    size = 6,
    shape = 21,
    stroke = 2
  ) +
  labs(x = "Successes", y = "Mass") +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05)) +
  theme_bw(18)
