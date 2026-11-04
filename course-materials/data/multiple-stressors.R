# Tidy data for the Week 6 Wednesday multiple stressor case studies. Reads the
# PANGAEA downloads in pre-class/data and writes one CSV per case study here.

library(tidyverse)

raw_dir <- here::here("course-materials/pre-class/data")
out_dir <- here::here("course-materials/data")

# Skip the metadata block, which ends with a line containing only */
read_pangaea <- function(file) {
  path <- file.path(raw_dir, file)
  header_end <- which(read_lines(path) == "*/")
  read_tsv(path, skip = header_end, show_col_types = FALSE)
}

# Pacific cod (Slesinger et al. 2024, PANGAEA.971545) -------------------------

# Growth from 0 to 11 days post-hatch, the only period that includes all three
# temperatures (the 10 C tanks ended at day 11). The tank is the replicate.
cod <- read_pangaea("Slesinger-etal_2024_MB.tab") |>
  transmute(
    day = `Time d [days] (post hatch)`,
    temp = `T:temp [°C]`,
    co2 = if_else(Treat == "ambient", "ambient", "high"),
    tank = `ID (Tank)`,
    mass_mg = `Dry m [mg]`
  ) |>
  filter(day %in% c(0, 11)) |>
  pivot_wider(names_from = day, values_from = mass_mg, names_prefix = "day_") |>
  transmute(
    tank,
    temp,
    co2,
    growth = 100 * (log(day_11) - log(day_0)) / 11
  ) |>
  arrange(temp, co2, tank)

write_csv(cod, file.path(out_dir, "pacific-cod-growth.csv"))

# Spider crabs (Walther et al. 2010, PANGAEA.752281) --------------------------

# Svalbard population only. Rows without a zoea II development time hold other
# measurements. The archive doesn't record the female or flask behind each value.
crabs <- read_pangaea("Walther-etal_2010.tab") |>
  filter(Site == "Svalbard", !is.na(`H. araneus dev time Z II [days]`)) |>
  transmute(
    temp = `Temp [°C]`,
    co2 = as.numeric(str_remove(`Exp treat`, "pCO2=")),
    dev_time = `H. araneus dev time Z II [days]`
  )

write_csv(crabs, file.path(out_dir, "spider-crab-development.csv"))

# Oysters (Lemasson and Knights 2021, PANGAEA.950894) -------------------------

# Rows with a valve category hold shell density, not whole-oyster measurements.
# The oyster number restarts within each treatment. Shell strength wasn't
# recorded for 2 oysters.
oysters <- read_pangaea("Lemasson-Knights_2021.tab") |>
  filter(`Species UID` == "Magallana gigas", is.na(`Cat (Valve)`)) |>
  transmute(
    oyster = `n [#] (Oyster)`,
    temp = str_to_lower(`Treat (Temperature)`),
    co2 = recode_values(
      `Treat (pCO2)`,
      "Control" ~ "ambient",
      "750ppm" ~ "750 ppm",
      "1000ppm" ~ "1000 ppm"
    ),
    shell_strength = `Shell str [N]`
  ) |>
  filter(!is.na(shell_strength))

write_csv(oysters, file.path(out_dir, "oyster-shell-strength.csv"))
