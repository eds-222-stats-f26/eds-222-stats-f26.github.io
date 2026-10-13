# =============================================================================
# April 1 Sierra snowpack vs. April-July unimpaired runoff
# San Joaquin Valley, California
#
# Data (all from CDEC, California Dept. of Water Resources):
#   - Runoff:   WSIHIST report, Apr-Jul unimpaired runoff (million acre-feet)
#   - Snowpack: monthly snow course measurements (sensor 3 = snow water
#               content, inches), April 1 survey
#
# Snowpack index: each snow course's April 1 SWE is expressed as a percent of
# that course's own long-term April 1 mean, then averaged across all courses in
# the valley's contributing basins. This is the same idea DWR uses for its
# "percent of average" statewide numbers.
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(rvest)
library(ggplot2)

# ---- Settings ---------------------------------------------------------------

start_year <- 1970 # snow course coverage is sparse before ~1940
end_year <- as.integer(format(Sys.Date(), "%Y"))
min_years <- 40 # keep only snow courses with >= this many April 1 records
min_frac_reporting <- 0.5 # need >= this fraction of courses to report in a year

# CDEC river basin names that drain to the San Joaquin Valley runoff index
basins <- c("STANISLAUS R", "TUOLUMNE R", "MERCED R", "SAN JOAQUIN R")

# Optional: if the automatic station search fails, list snow course IDs here
# (look them up on CDEC -> Snow -> snow course list, filtered by basin).
manual_stations <- character(0)

# ---- 1. Runoff: parse the WSIHIST report ------------------------------------

get_runoff <- function() {
  url <- "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST"
  txt <- readLines(url, warn = FALSE)
  txt <- gsub("<[^>]+>", " ", txt) # strip any HTML tags
  rows <- grep("^\\s*(19|20)\\d{2}\\s", txt, value = TRUE)

  # Full rows have 11 tokens:
  #   WY | Sac: Oct-Mar Apr-Jul WYsum Index Type | SJ: Oct-Mar Apr-Jul WYsum Index Type
  # Early years (Sacramento blank) have 6 tokens: WY | SJ columns only
  parsed <- lapply(strsplit(trimws(rows), "\\s+"), function(tok) {
    yr <- suppressWarnings(as.integer(tok[1]))
    sj_col <- if (length(tok) == 11) {
      8
    } else if (length(tok) == 6) {
      3
    } else {
      NA
    }
    if (is.na(sj_col)) {
      return(NULL)
    }
    tibble(wy = yr, apr_jul = suppressWarnings(as.numeric(tok[sj_col])))
  })

  bind_rows(parsed) %>%
    filter(!is.na(wy), !is.na(apr_jul)) %>%
    distinct(wy, .keep_all = TRUE) # keep first (main) table only
}

# ---- 2. Snow courses: find stations in each basin ---------------------------

find_snow_courses <- function(basin) {
  url <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/staSearch?sta=&sensor_chk=on&sensor=3",
    "&dur=M&active=&lon1=&lon2=&lat1=&lat2=&elev1=-5&elev2=99000&nearby=",
    "&basin_chk=on&basin=",
    URLencode(basin, reserved = TRUE),
    "&hydro=&county=&agency=&display=sta"
  )
  tryCatch(
    {
      tbl <- read_html(url) %>% html_element("table") %>% html_table()
      id_col <- grep("^ID$", names(tbl), ignore.case = TRUE, value = TRUE)[1]
      unique(as.character(tbl[[id_col]]))
    },
    error = function(e) {
      message("  Station search failed for ", basin, ": ", conditionMessage(e))
      character(0)
    }
  )
}

# ---- 3. Snow courses: download April 1 SWE ----------------------------------

get_april_swe <- function(ids, chunk_size = 20) {
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  out <- lapply(chunks, function(chunk) {
    url <- paste0(
      "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
      paste(chunk, collapse = ","),
      "&SensorNums=3&dur_code=M&Start=",
      start_year,
      "-01-01&End=",
      end_year,
      "-12-31"
    )
    d <- tryCatch(
      read_csv(url, col_types = cols(.default = "c"), show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) {
      return(NULL)
    }
    names(d) <- toupper(gsub("[^A-Za-z]+", "_", trimws(names(d))))
    d %>%
      transmute(
        station = STATION_ID,
        year = as.integer(substr(DATE_TIME, 1, 4)),
        month = as.integer(substr(DATE_TIME, 5, 6)),
        swe = suppressWarnings(as.numeric(VALUE))
      )
  })
  bind_rows(out) %>% filter(month == 4, !is.na(swe), swe >= 0)
}

# ---- 4. Build the regional snowpack index -----------------------------------

snow_index <- function(swe) {
  good <- swe %>% count(station) %>% filter(n >= min_years) %>% pull(station)
  if (length(good) == 0) {
    stop("No snow courses with >= ", min_years, " April records.")
  }
  message("  Using ", length(good), " snow courses")

  swe %>%
    filter(station %in% good) %>%
    group_by(station) %>%
    mutate(pct = 100 * swe / mean(swe)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarise(n_reporting = n(), snow_pct = mean(pct), .groups = "drop") %>%
    filter(n_reporting >= min_frac_reporting * length(good))
}

# ---- Run --------------------------------------------------------------------

message("Downloading runoff (WSIHIST)...")
runoff <- get_runoff()

message("Snow courses for San Joaquin Valley...")
ids <- manual_stations
if (length(ids) == 0) {
  ids <- unique(unlist(lapply(basins, find_snow_courses)))
}
if (length(ids) == 0) {
  stop("No stations found. Fill in manual_stations at the top of the script.")
}
snow <- snow_index(get_april_swe(ids))

dat <- inner_join(snow, runoff, by = c("year" = "wy")) %>%
  filter(year >= start_year)

write_csv(dat, "snowpack_runoff_data.csv")

# ---- 5. Fit linear model ----------------------------------------------------

fit <- lm(apr_jul ~ snow_pct, data = dat)
print(summary(fit))

cf <- coef(fit)
label <- sprintf(
  "y = %.2f + %.3f x\nR² = %.2f, n = %d",
  cf[1],
  cf[2],
  summary(fit)$r.squared,
  nobs(fit)
)

# ---- 6. Plot ----------------------------------------------------------------

notable <- c(2017, 2008, 1982, 2023)

p <- ggplot(dat, aes(snow_pct, apr_jul)) +
  geom_point(size = 2, alpha = 0.8, shape = 21) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(year %in% notable, year, "")),
    box.padding = 1.5,
    point.padding = 0,
    force = 5,
    min.segment.length = 1,
    max.overlaps = Inf,
    size = 3.2,
    color = "grey25"
  ) +
  expand_limits(y = c(0, 10)) +
  labs(
    x = "April 1 snowpack (% of average, snow course mean)",
    y = "April–July runoff (million acre-feet)",
    title = "San Joaquin Valley snowpack and spring–summer runoff",
    caption = "Data: California DWR, CDEC (snow courses; WSIHIST)"
  ) +
  theme_bw(base_size = 14)

print(p)
ggsave("snowpack_runoff_scatter.png", p, width = 6, height = 5, dpi = 200)
message("\nSaved snowpack_runoff_scatter.png and snowpack_runoff_data.csv")
