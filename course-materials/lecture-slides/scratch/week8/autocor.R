library(tidyverse)
library(rnoaa)
set.seed(123)
theme_set(theme_bw(14))

cch <- read_csv("~/Downloads/cch_2014-2024.csv") %>%
  mutate(
    datetime = `DATE TIME`,
    storage_af = zoo::na.approx(parse_number(VALUE), x = datetime)
  )
ggplot(cch, aes(datetime, storage_af)) +
  geom_line()

# Download precip data
precip <- map(2014:2024, \(yr) {
  ncdc(
    datasetid = "GHCND",
    stationid = "GHCND:USC00041253",
    datatypeid = "PRCP",
    limit = 366,
    startdate = paste0(yr, "-01-01"),
    enddate = paste0(yr, "-12-31")
  )$data
}) %>%
  list_rbind() %>%
  mutate(datetime = parse_datetime(date))

ggplot(precip, aes(datetime, value)) +
  geom_line()

precip_cch <- full_join(
  select(cch, datetime, storage_af),
  transmute(precip, datetime, precip_mm = value / 10),
  by = "datetime"
)
ggplot(precip_cch, aes(precip_mm, storage_af)) +
  geom_point()
precip_cch %>%
  pivot_longer(
    cols = c(storage_af, precip_mm),
    names_to = "var",
    values_to = "val"
  ) %>%
  ggplot(aes(datetime, val)) +
  geom_line() +
  facet_grid(var ~ ., scales = "free")

precip_cch_weekly <- precip_cch %>%
  mutate(datetime = round_date(datetime, unit = "week")) %>%
  group_by(datetime) %>%
  summarize(storage_af = mean(storage_af), precip_mm = sum(precip_mm))
precip_cch_weekly %>%
  pivot_longer(
    cols = c(storage_af, precip_mm),
    names_to = "var",
    values_to = "val"
  ) %>%
  ggplot(aes(datetime, val)) +
  geom_line() +
  facet_grid(var ~ ., scales = "free") +
  theme(axis.title = element_blank())
ggsave(
  "course-materials/lecture-slides/scratch/week8/precip_storage.png",
  width = 8,
  height = 4
)
ggplot(precip_cch_weekly, aes(precip_mm, storage_af)) +
  geom_point(shape = 21) +
  labs(x = "Precipitation (mm)", y = "Storage (acre ft)")
ggsave(
  "course-materials/lecture-slides/scratch/week8/precip_storage_scatter.png",
  width = 6,
  height = 4
)


acf(precip_cch_weekly$storage_af, lag.max = 100)
acf(precip_cch_weekly$precip_mm, lag.max = 10)
ccf(precip_cch_weekly$storage_af, precip_cch_weekly$precip_mm)

library(dynlm)
library(zoo)
cch_zoo <- zoo(
  select(precip_cch_weekly, -datetime),
  order.by = seq(nrow(precip_cch_weekly))
)
max_lag <- 4
cch_mod <- dynlm(
  storage_af ~ L(storage_af, 1) + L(precip_mm, 0:max_lag),
  data = cch_zoo
)
summary(cch_mod)

precip_cch_weekly %>%
  mutate(storage_af_pred = c(rep(NA, max_lag), predict(cch_mod))) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = storage_af), color = "black", linewidth = 2) +
  geom_line(aes(y = storage_af_pred), color = "cornflowerblue", linewidth = 1.2)


# autocorrelation ---------------------------------------------------------

set.seed(123)
x <- 1:50
y <- cumsum(rnorm(50))
df <- tibble(x, y)

p1 <- ggplot(df, aes(x, y)) +
  geom_line() +
  geom_point()
p2 <- ggplot(df, aes(y, y)) +
  geom_point()
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/lag0.png",
  width = 8,
  height = 4
)

p1 <- ggplot(df, aes(x, y)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = lag(y, 1)), color = "cornflowerblue") +
  geom_point(aes(y = lag(y, 1)), color = "cornflowerblue")
p2 <- ggplot(df, aes(y, lag(y, 1))) +
  geom_point()
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/lag1.png",
  width = 8,
  height = 4
)

p1 <- ggplot(df, aes(x, y)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = lag(y, 2)), color = "cornflowerblue") +
  geom_point(aes(y = lag(y, 2)), color = "cornflowerblue")
p2 <- ggplot(df, aes(y, lag(y, 2))) +
  geom_point()
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/lag2.png",
  width = 8,
  height = 4
)

p1 <- ggplot(df, aes(x, y)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = lag(y, 3)), color = "cornflowerblue") +
  geom_point(aes(y = lag(y, 3)), color = "cornflowerblue")
p2 <- ggplot(df, aes(y, lag(y, 3))) +
  geom_point()
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave(
  "course-materials/lecture-slides/scratch/week8/lag3.png",
  width = 8,
  height = 4
)

df_lags <- map(0:19, \(l) {
  mutate(df, lag_y = lag(y, l), lag = l)
}) %>%
  list_rbind()
ggplot(df_lags, aes(y, lag_y)) +
  geom_point(shape = 21) +
  facet_wrap(~lag, ncol = 5)
ggsave(
  "course-materials/lecture-slides/scratch/week8/lags_0_19.png",
  width = 8,
  height = 4
)
df_lags %>%
  drop_na(lag_y) %>%
  group_by(lag) %>%
  summarize(r = cor(y, lag_y)) %>%
  ggplot(aes(lag, r)) +
  geom_segment(aes(xend = lag, yend = 0))
ggsave(
  "course-materials/lecture-slides/scratch/week8/acf.png",
  width = 8,
  height = 4
)


# spatial autocorrelation -------------------------------------------------

library(terra)
library(ncf)
library(tidyterra)
ebird <- read_tsv("~/Downloads/0044039-251025141854904.csv")
ebird_vect <- vect(
  ebird,
  geom = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
) %>%
  project("EPSG:26910")
template <- rast(ext(-125, -115, 30, 47), crs = "EPSG:4326") %>%
  project("EPSG:26910")
res(template) <- 1e4
ebird_rast <- rasterize(
  ebird_vect,
  template,
  field = "individualCount",
  fun = "sum"
)
plot(ebird_rast)


ndvi_rast <- rast("~/Downloads/MOD13A2.A2025017.h08v05.061.2025035162811.hdf")[[
  1
]] *
  1e-8
plot(ndvi_rast)

# Extract values and coordinates
vals <- values(ndvi_rast, mat = FALSE)
coords <- crds(ndvi_rast, df = TRUE, na.rm = FALSE)

# Cross-sample with distances
set.seed(123)
ndvi_tbl <- tibble(ndvi = vals, x = coords[, 1], y = coords[, 2])
from <- ndvi_tbl %>%
  sample_n(5e6, replace = TRUE) %>%
  rename(ndvi1 = ndvi, x1 = x, y1 = y)
to <- ndvi_tbl %>%
  sample_n(5e6, replace = TRUE) %>%
  rename(ndvi2 = ndvi, x2 = x, y2 = y)

round_to <- function(x, to) {
  round(x / to) * to
}

from_to <- cbind(from, to) %>%
  mutate(
    distance = sqrt((x2 - x1)^2 + (y2 - y1)^2),
    lag = round_to(distance, 5e4)
  ) %>%
  filter(lag <= 1e6, !is.nan(ndvi1), !is.nan(ndvi2))

plot_spat_auto <- function(l) {
  set.seed(123)
  foo <- from_to %>%
    filter(lag == l) %>%
    arrange(((x2 - x1)^2 - (y2 - y1)^2)^2) %>%
    slice(1)
  foo_ext <- with(
    foo,
    ext(
      min(x1, x2) - 2e3,
      max(x1, x2) + 2e3,
      min(y1, y2) - 2e3,
      max(y1, y2) + 2e3
    )
  ) *
    1.5
  bar <- vect(
    tibble(x = c(foo$x1, foo$x2), y = c(foo$y1, foo$y2)),
    geom = c("x", "y"),
    crs = crs(ndvi_rast)
  )
  ggplot() +
    geom_spatraster(data = crop(ndvi_rast, foo_ext)) +
    geom_spatvector(data = bar, color = "cyan", size = 2) +
    scale_fill_viridis_c("NDVI", limits = c(-0.2, 1.0)) +
    theme(legend.position = "none")
}
cowplot::plot_grid(
  plot_spat_auto(50000),
  plot_spat_auto(200000),
  plot_spat_auto(350000),
  plot_spat_auto(500000),
  plot_spat_auto(650000),
  plot_spat_auto(800000),
  ncol = 3
)
ggsave(
  "course-materials/lecture-slides/scratch/week8/ndvi_pairs.png",
  width = 8,
  height = 4
)


ndvi_correlog <- from_to %>%
  group_by(lag) %>%
  summarize(r = cor(ndvi1, ndvi2))

ggplot(ndvi_correlog, aes(lag / 1e3, r)) +
  geom_segment(aes(xend = lag / 1e3, yend = 0)) +
  labs(x = "lag (km)")
ggsave(
  "course-materials/lecture-slides/scratch/week8/ndvi_correlog.png",
  width = 8,
  height = 4
)
