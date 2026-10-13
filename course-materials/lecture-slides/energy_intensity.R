# =============================================================================
# Wind + solar + hydro + nuclear share vs. carbon intensity of electricity
# eGRID subregions, 2023
#
# Data: EPA eGRID2023 (rev 2), subregion sheet (SRL23)
#   - SRWIPR:   wind generation share of resource mix (fraction)
#   - SRSOPR:   solar generation share of resource mix (fraction)
#   - SRHYPR:   hydro generation share of resource mix (fraction)
#   - SRNCPR:   nuclear generation share of resource mix (fraction)
#   - SRCO2RTA: annual CO2 total output emission rate (lb/MWh), converted to
#               g/kWh (1 lb = 453.592 g; 1 MWh = 1000 kWh)
# =============================================================================

library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)

# ---- 1. Read eGRID subregion sheet ------------------------------------------

# Row 1 holds long descriptions; row 2 holds the short column codes
egrid <- read_excel(
  "course-materials/lecture-slides/egrid2023_data_rev2.xlsx",
  sheet = "SRL23",
  skip = 1
)

# ---- 2. Region, low-carbon share, carbon intensity --------------------------

# Drop Alaska, Hawaii, and Puerto Rico (isolated island/off-grid systems)
energy <- egrid %>%
  filter(!SUBRGN %in% c("AKGD", "AKMS", "HIMS", "HIOA", "PRMS")) %>%
  transmute(
    region = SUBRGN,
    low_carbon_pct = (SRWIPR + SRSOPR + SRHYPR + SRNCPR + SRGTPR) * 100,
    co2_g_kwh = SRCO2RTA * 453.592 / 1000
  )

# ---- 3. Linear model ----------------------------------------------------------

energy_lm <- lm(co2_g_kwh ~ low_carbon_pct, data = energy)
summary(energy_lm)

# ---- 4. Scatter plot ----------------------------------------------------------

ggplot(energy, aes(low_carbon_pct, co2_g_kwh)) +
  geom_smooth(method = "lm", formula = y ~ x, color = "grey40", se = FALSE) +
  geom_point() +
  geom_text_repel(aes(label = region), size = 3, seed = 222, force = 5) +
  labs(
    x = "Non-combustion share of generation (%)",
    y = expression("Carbon intensity (g" ~ CO[2] ~ kWh^-1 * ")")
  ) +
  theme_classic(base_size = 14)
