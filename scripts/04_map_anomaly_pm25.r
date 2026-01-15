# ==========================================
# 04_map_anomaly_pm25.R  (IJC445)
# Seasonal anomaly map (examiner favourite)
# Dataset: city-level daily averages (PM2.5 + weather)
# Focus: PM2.5 seasonal anomaly = (seasonal mean) - (city annual mean)
#
# Input:
#   data/processed/pm25_weather_daily.csv
#
# Output:
#   outputs/figures/Fig13_map_pm25_seasonal_anomaly.png
#   outputs/tables/Table_pm25_seasonal_anomaly.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ---------- Folders ----------
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ---------- Load data ----------
path <- "data/processed/pm25_weather_daily.csv"
stopifnot(file.exists(path))

df <- readr::read_csv(path, show_col_types = FALSE) %>%
  clean_names()

required_cols <- c("city", "date", "pm25_mean")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))

df_viz <- df %>%
  transmute(
    city = as.character(city),
    date = as.Date(date),
    pm25_mean = as.numeric(pm25_mean)
  ) %>%
  filter(!is.na(city), !is.na(date), !is.na(pm25_mean), pm25_mean >= 0) %>%
  mutate(
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE                   ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

stopifnot(nrow(df_viz) > 0)

# ---------- City coordinates (centres) ----------
city_centres <- tibble::tribble(
  ~city,         ~latitude, ~longitude,
  "London",       51.5074,   -0.1278,
  "Manchester",   53.4808,   -2.2426,
  "Birmingham",   52.4862,   -1.8904,
  "Sheffield",    53.3811,   -1.4701
)

# ---------- Compute seasonal anomaly ----------
# City annual mean (across full time range)
city_annual <- df_viz %>%
  group_by(city) %>%
  summarise(pm25_annual_mean = mean(pm25_mean, na.rm = TRUE), .groups = "drop")

# City seasonal mean
city_seasonal <- df_viz %>%
  group_by(city, season) %>%
  summarise(
    pm25_season_mean = mean(pm25_mean, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  left_join(city_annual, by = "city") %>%
  mutate(
    pm25_anomaly = pm25_season_mean - pm25_annual_mean
  ) %>%
  left_join(city_centres, by = "city")

# Save table for report
readr::write_csv(city_seasonal, "outputs/tables/Table_pm25_seasonal_anomaly.csv")

# ---------- Map base (UK) ----------
uk <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "United Kingdom")

# Convert points to sf
pts <- st_as_sf(
  city_seasonal,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# ---------- Fix scale: single diverging legend across all seasons ----------
# Make limits symmetric so red/blue meaning is consistent
max_abs <- max(abs(pts$pm25_anomaly), na.rm = TRUE)
lim <- c(-max_abs, max_abs)

# ---------- Plot ----------
p <- ggplot() +
  geom_sf(data = uk, fill = "grey95", colour = "grey70", linewidth = 0.4) +
  geom_sf(data = pts, aes(colour = pm25_anomaly), size = 4.2) +
  # City labels (small, readable)
  geom_text(
    data = city_seasonal,
    aes(x = longitude, y = latitude, label = city),
    nudge_y = 0.28,
    size = 3.2
  ) +
  facet_wrap(~ season, ncol = 2) +
  scale_colour_gradient2(
    low = "#2b8cbe",
    mid = "white",
    high = "#d7301f",
    midpoint = 0,
    limits = lim,
    name = expression(paste("PM"[2.5], " anomaly (", mu, "g/m"^3, ")"))
  ) +
  coord_sf(xlim = c(-6.5, 2.5), ylim = c(50.0, 56.5), expand = FALSE) +
  labs(
    title = expression(paste("Seasonal anomaly in city-level PM"[2.5], " (relative to each city's annual mean)")),
    subtitle = "Blue = cleaner than the city's typical level; Red = more polluted than typical",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey92"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = "outputs/figures/Fig13_map_pm25_seasonal_anomaly.png",
  plot = p,
  width = 10.8,
  height = 7.6,
  dpi = 300
)

message("DONE Saved:\n- outputs/figures/Fig13_map_pm25_seasonal_anomaly.png\n- outputs/tables/Table_pm25_seasonal_anomaly.csv")



