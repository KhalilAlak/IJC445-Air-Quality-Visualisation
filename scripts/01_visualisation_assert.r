# ==========================================
# 01_visualisation_assert.R  (IJC445)
# Dataset: Air Quality (OpenAQ) + Weather (Open-Meteo)
# Level: city-level DAILY averages (full range)
# Variable focus: wind speed
# Framework: ASSERT + Grammar of Graphics (ggplot2)
# Outputs: outputs/figures/*.png + outputs/tables/*.csv
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)

# ---------- Folders ----------
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ==========================================
# ASSERT: ASK
# Q1: How does PM2.5 vary across cities and time?
# Q2: What is the relationship between wind speed and PM2.5?
# Q3: Does the wind–PM2.5 relationship differ by city or season?
# ==========================================

# ==========================================
# ASSERT: SEARCH (load data)
# Expected file copied from IJC437:
# data/processed/pm25_weather_daily.csv
# ==========================================
path <- "data/processed/pm25_weather_daily.csv"
stopifnot(file.exists(path))

df <- readr::read_csv(path, show_col_types = FALSE) %>%
  clean_names()

# ==========================================
# ASSERT: STRUCTURE (validate + clean minimal)
# ==========================================

# ---- Required columns ----
required_cols <- c("city", "date", "pm25_mean", "wind_mean")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# ---- Parse types ----
df <- df %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city),
    pm25_mean = as.numeric(pm25_mean),
    wind_mean = as.numeric(wind_mean)
  )

# ---- Basic sanity filters (no over-cleaning for viz) ----
df_viz <- df %>%
  filter(!is.na(date), !is.na(city)) %>%
  filter(!is.na(pm25_mean), pm25_mean >= 0) %>%
  filter(!is.na(wind_mean), wind_mean >= 0)

# ---- Quick ASSERT checks (fail fast) ----
stopifnot(nrow(df_viz) > 0)
stopifnot(all(is.finite(df_viz$pm25_mean)))
stopifnot(all(is.finite(df_viz$wind_mean)))

# ---- Coverage summary (useful in write-up) ----
qa_tbl <- df_viz %>%
  group_by(city) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    n_days     = n(),
    pm25_missing_pct = 100 * mean(is.na(df$pm25_mean[df$city == first(city)])),
    wind_missing_pct = 100 * mean(is.na(df$wind_mean[df$city == first(city)])),
    .groups = "drop"
  )

readr::write_csv(qa_tbl, "outputs/tables/qa_city_coverage.csv")

message("Rows (viz): ", nrow(df_viz))
message("Date range: ", paste(range(df_viz$date), collapse = " -> "))
print(qa_tbl)

# Create season variable for clearer storytelling
df_viz <- df_viz %>%
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

# ==========================================
# ASSERT: ENVISION (choose encodings)
# - Time-series: x=date, y=pm25_mean, facet=city
# - Relationship: x=wind_mean, y=pm25_mean, smoothing + faceting
# - Seasonal comparison: facets by season, colour by city
# ==========================================

# ==========================================
# ASSERT: REPRESENT (build visuals with Grammar of Graphics)
# ==========================================

# -------- Visual 1: PM2.5 over time by city (smooth monthly for readability) --------
pm25_monthly <- df_viz %>%
  mutate(month_start = floor_date(date, "month")) %>%
  group_by(city, month_start) %>%
  summarise(
    pm25_mean = mean(pm25_mean, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(pm25_monthly, aes(x = month_start, y = pm25_mean)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Monthly mean PM2.5 by city (city-level daily averages aggregated to monthly)",
    subtitle = "Full available range; smoothing via monthly aggregation improves interpretability",
    x = "Month",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig01_pm25_trend_monthly_by_city.png", p1, width = 12, height = 7, dpi = 300)

# -------- Visual 2: Distribution of PM2.5 by city (log x optional removed; use clean boxplot) --------
# Use coord_cartesian to avoid outliers crushing the plot (keeps them, just zooms view)
y_cap <- quantile(df_viz$pm25_mean, 0.98, na.rm = TRUE)

p2 <- ggplot(df_viz, aes(x = city, y = pm25_mean)) +
  geom_boxplot(outlier.alpha = 0.20, na.rm = TRUE) +
  coord_cartesian(ylim = c(0, y_cap)) +
  labs(
    title = "Distribution of daily PM2.5 by city",
    subtitle = "View is capped at the 98th percentile to prevent extreme days from compressing the boxes",
    x = "City",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig02_pm25_boxplot_by_city.png", p2, width = 10, height = 6, dpi = 300)

# -------- Visual 3: Wind speed vs PM2.5 (scatter + smooth), faceted by city --------
# Sample for plotting to reduce overplotting, without changing analysis
set.seed(42)
df_scatter <- df_viz %>%
  group_by(city) %>%
  group_modify(~{
    n_take <- min(6000, nrow(.x))
    dplyr::slice_sample(.x, n = n_take)
  }) %>%
  ungroup()

p3 <- ggplot(df_scatter, aes(x = wind_mean, y = pm25_mean)) +
  geom_point(alpha = 0.18, size = 0.9, na.rm = TRUE) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Relationship between wind speed and PM2.5 (daily, city-level)",
    subtitle = "Higher wind speed generally corresponds to lower PM2.5 due to dispersion (smooth = LOESS)",
    x = "Mean wind speed (10m)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig03_wind_vs_pm25_by_city.png", p3, width = 12, height = 7, dpi = 300)

# -------- Visual 4: Seasonal wind–PM2.5 pattern (same relationship, faceted by season) --------
df_scatter_season <- df_viz %>%
  group_by(city, season) %>%
  group_modify(~{
    n_take <- min(2500, nrow(.x))
    dplyr::slice_sample(.x, n = n_take)
  }) %>%
  ungroup()

p4 <- ggplot(df_scatter_season, aes(x = wind_mean, y = pm25_mean, colour = city)) +
  geom_point(alpha = 0.15, size = 0.85, na.rm = TRUE) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title = "Wind speed vs PM2.5 by season",
    subtitle = "Season can moderate the relationship (e.g., winter stability/inversions vs summer mixing)",
    x = "Mean wind speed (10m)",
    y = expression(PM[2.5]~(mu*g/m^3)),
    colour = "City"
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig04_wind_vs_pm25_by_season.png", p4, width = 12, height = 7, dpi = 300)

# -------- Visual 5: Simple “Tell” figure — weekly profile of PM2.5 by city --------
dow_tbl <- df_viz %>%
  mutate(dow = wday(date, label = TRUE, abbr = TRUE, week_start = 1)) %>%
  group_by(city, dow) %>%
  summarise(pm25_mean = mean(pm25_mean, na.rm = TRUE), .groups = "drop")

p5 <- ggplot(dow_tbl, aes(x = dow, y = pm25_mean, group = city, colour = city)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  labs(
    title = "Average PM2.5 by day of week",
    subtitle = "A compact summary for storytelling (commuting patterns often show weekday effects)",
    x = "Day of week",
    y = expression(PM[2.5]~(mu*g/m^3)),
    colour = "City"
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig05_pm25_day_of_week.png", p5, width = 11, height = 6, dpi = 300)

# ==========================================
# ASSERT: TELL (export small summary tables for writing)
# ==========================================

summary_tbl <- df_viz %>%
  group_by(city) %>%
  summarise(
    mean_pm25 = mean(pm25_mean, na.rm = TRUE),
    median_pm25 = median(pm25_mean, na.rm = TRUE),
    sd_pm25 = sd(pm25_mean, na.rm = TRUE),
    mean_wind = mean(wind_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_pm25))

readr::write_csv(summary_tbl, "outputs/tables/city_summary_pm25_wind.csv")

message("\nDONE IJC445 visuals saved to outputs/figures/")
message("Tables saved to outputs/tables/")
