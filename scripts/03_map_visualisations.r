# ==========================================
# 03_map_visualisations.R  (IJC445)
# Maps: City-level PM2.5 + Wind Speed (UK)
# Dataset: pm25_weather_daily.csv (city-level daily)
# Outputs:
#  - outputs/figures/Fig09_map_pm25_mean_by_city.png
#  - outputs/figures/Fig10_map_wind_mean_by_city.png
#  - outputs/figures/Fig11_map_pm25_wind_quadrants.png
#  - outputs/figures/Fig12_map_pm25_seasonal_smallmultiples.png
#  - outputs/tables/Table_city_map_summary.csv
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)

# Spatial packages
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) install.packages("rnaturalearthdata")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# ---------- Folders ----------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# ---------- Load data ----------
path <- "data/processed/pm25_weather_daily.csv"
stopifnot(file.exists(path))

df <- readr::read_csv(path, show_col_types = FALSE) %>%
  clean_names()

# ---------- Validate columns ----------
required_cols <- c("city", "date", "pm25_mean", "wind_mean")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

df_viz <- df %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city),
    pm25_mean = as.numeric(pm25_mean),
    wind_mean = as.numeric(wind_mean),
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE                   ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  ) %>%
  filter(!is.na(date), !is.na(city)) %>%
  filter(!is.na(pm25_mean), pm25_mean >= 0) %>%
  filter(!is.na(wind_mean), wind_mean >= 0)

stopifnot(nrow(df_viz) > 0)

# ---------- City coordinates (fixed & transparent choice) ----------
# (City-centre coordinates; used only for map context)
city_coords <- tibble::tribble(
  ~city,        ~lon,     ~lat,
  "London",     -0.1278,  51.5074,
  "Manchester", -2.2426,  53.4808,
  "Birmingham", -1.8904,  52.4862,
  "Sheffield",  -1.4701,  53.3811
)

# ---------- Summaries for map ----------
city_summary <- df_viz %>%
  group_by(city) %>%
  summarise(
    pm25_mean_city = mean(pm25_mean, na.rm = TRUE),
    wind_mean_city = mean(wind_mean, na.rm = TRUE),
    n_days = n(),
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  ) %>%
  left_join(city_coords, by = "city")

readr::write_csv(city_summary, "outputs/tables/Table_city_map_summary.csv")

# Convert to sf points
city_sf <- city_summary %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ---------- UK basemap ----------
uk_map <- ne_countries(
  country = "United Kingdom",
  scale = "medium",
  returnclass = "sf"
)

# A small bbox crop (keeps map clean and focused)
uk_bbox <- st_as_sfc(st_bbox(c(xmin = -7.5, ymin = 49.5, xmax = 2.5, ymax = 56.5), crs = st_crs(4326)))

# ==========================================
# FIG 09 — Mean PM2.5 by city
# ==========================================
p_map_pm25 <- ggplot() +
  geom_sf(data = st_intersection(uk_map, uk_bbox), fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_sf(
    data = city_sf,
    aes(size = pm25_mean_city, fill = pm25_mean_city),
    shape = 21, colour = "black", alpha = 0.95
  ) +
  scale_fill_viridis_c(option = "C") +
  scale_size_continuous(range = c(4, 11)) +
  labs(
    title = "Mean PM2.5 by city (full period)",
    subtitle = "City-level daily averages aggregated over the full study range",
    fill = expression(PM[2.5]~(mu*g/m^3)),
    size = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig09_map_pm25_mean_by_city.png", p_map_pm25, width = 7.8, height = 8.6, dpi = 300)

# ==========================================
# FIG 10 — Mean wind speed by city
# ==========================================
p_map_wind <- ggplot() +
  geom_sf(data = st_intersection(uk_map, uk_bbox), fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_sf(
    data = city_sf,
    aes(size = wind_mean_city, fill = wind_mean_city),
    shape = 21, colour = "black", alpha = 0.95
  ) +
  scale_fill_viridis_c(option = "D") +
  scale_size_continuous(range = c(4, 11)) +
  labs(
    title = "Mean wind speed by city (full period)",
    subtitle = "Wind speed at 10m aggregated from hourly Open-Meteo to daily, then averaged over time",
    fill = "Wind speed (10m)",
    size = "Wind speed (10m)"
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig10_map_wind_mean_by_city.png", p_map_wind, width = 7.8, height = 8.6, dpi = 300)

# ==========================================
# FIG 11 — Quadrant map: High/Low PM2.5 vs High/Low Wind
# (Good “TELL” visual: quick story)
# ==========================================
pm25_med <- median(city_summary$pm25_mean_city, na.rm = TRUE)
wind_med <- median(city_summary$wind_mean_city, na.rm = TRUE)

city_quad_sf <- city_summary %>%
  mutate(
    pm25_level = if_else(pm25_mean_city >= pm25_med, "High PM2.5", "Low PM2.5"),
    wind_level = if_else(wind_mean_city >= wind_med, "High wind", "Low wind"),
    quadrant = paste(pm25_level, "&", wind_level)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

p_quad <- ggplot() +
  geom_sf(data = st_intersection(uk_map, uk_bbox), fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_sf(data = city_quad_sf, aes(shape = quadrant, fill = quadrant), size = 5, colour = "black", alpha = 0.95) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "City comparison: PM2.5 vs wind speed (quadrants)",
    subtitle = "Cities are classified by median PM2.5 and median wind (full-period averages)",
    fill = "Group",
    shape = "Group"
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig11_map_pm25_wind_quadrants.png", p_quad, width = 7.8, height = 8.6, dpi = 300)

# ==========================================
# FIG 12 — Seasonal small multiples map (PM2.5)
# (Adds strong ASSERT context: “does it change by season?”)
# ==========================================
season_city <- df_viz %>%
  group_by(city, season) %>%
  summarise(
    pm25_season = mean(pm25_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(city_coords, by = "city") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

p_season <- ggplot() +
  geom_sf(data = st_intersection(uk_map, uk_bbox), fill = "grey95", colour = "grey60", linewidth = 0.25) +
  geom_sf(
    data = season_city,
    aes(size = pm25_season, fill = pm25_season),
    shape = 21, colour = "black", alpha = 0.95
  ) +
  facet_wrap(~ season) +
  scale_fill_viridis_c(option = "C") +
  scale_size_continuous(range = c(3.5, 9.5)) +
  labs(
    title = "Seasonal mean PM2.5 by city",
    subtitle = "Small multiples show how inter-city differences shift across seasons",
    fill = expression(PM[2.5]~(mu*g/m^3)),
    size = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig12_map_pm25_seasonal_smallmultiples.png", p_season, width = 10.5, height = 7.5, dpi = 300)

message("DONE Map figures saved to outputs/figures and summary table saved to outputs/tables.")
