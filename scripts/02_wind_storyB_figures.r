# ==========================================
# 02_wind_storyB_figures.R  (IJC445)
# Story B: Wind–PM2.5 relationship differs by city and season
# Dataset: city-level daily averages (full range)
# Input:  data/processed/pm25_weather_daily.csv
# Output: outputs/figures/Fig06_*.png, Fig07_*.png, Fig08_*.png
#         outputs/tables/Table_wind_bins_effect.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)

# geom_hex needs hexbin
if (!requireNamespace("hexbin", quietly = TRUE)) install.packages("hexbin")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# LOAD + STRUCTURE (ASSERT)
# ----------------------------
path <- "data/processed/pm25_weather_daily.csv"
stopifnot(file.exists(path))

df <- readr::read_csv(path, show_col_types = FALSE) %>% clean_names()

required_cols <- c("city", "date", "pm25_mean", "wind_mean")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

df_viz <- df %>%
  transmute(
    city = factor(city),
    date = as.Date(date),
    pm25_mean = as.numeric(pm25_mean),
    wind_mean = as.numeric(wind_mean)
  ) %>%
  filter(!is.na(city), !is.na(date)) %>%
  filter(!is.na(pm25_mean), pm25_mean >= 0) %>%
  filter(!is.na(wind_mean), wind_mean >= 0) %>%
  mutate(
    month  = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE                   ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

stopifnot(nrow(df_viz) > 0)

# ----------------------------
# FIG06: Density relationship (hex) by city
# ----------------------------
y_cap <- quantile(df_viz$pm25_mean, 0.99, na.rm = TRUE)

p6 <- ggplot(df_viz, aes(x = wind_mean, y = pm25_mean)) +
  geom_hex(bins = 35) +
  facet_wrap(~city, scales = "free_y") +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = TRUE,
    linewidth = 0.9
  ) +
  coord_cartesian(ylim = c(0, y_cap)) +
  labs(
    title = "Wind speed vs PM2.5 (hex density by city)",
    subtitle = "Hex-binning avoids overplotting; smooth shows overall trend",
    x = "Daily mean wind speed (10m)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig06_pm25_wind_hex_by_city.png", p6, width = 12, height = 7, dpi = 300)

# ----------------------------
# BIN WIND + EFFECT TABLE (for Fig07 + Fig08)
# ----------------------------
# Percentile-based bins so each bin has data
wind_breaks <- quantile(df_viz$wind_mean, probs = seq(0, 1, by = 0.05), na.rm = TRUE)
wind_breaks <- unique(as.numeric(wind_breaks))
stopifnot(length(wind_breaks) >= 3)

df_binned <- df_viz %>%
  mutate(
    wind_bin = cut(wind_mean, breaks = wind_breaks, include.lowest = TRUE, right = TRUE)
  ) %>%
  filter(!is.na(wind_bin)) %>%
  group_by(city, season, wind_bin) %>%
  summarise(
    n = n(),
    wind_mean_bin = mean(wind_mean, na.rm = TRUE),
    pm25_mean_bin = mean(pm25_mean, na.rm = TRUE),
    pm25_sd_bin   = sd(pm25_mean, na.rm = TRUE),
    pm25_se_bin   = pm25_sd_bin / sqrt(n),
    pm25_ci_low   = pm25_mean_bin - 1.96 * pm25_se_bin,
    pm25_ci_high  = pm25_mean_bin + 1.96 * pm25_se_bin,
    .groups = "drop"
  ) %>%
  filter(n >= 30) %>%        # keeps uncertainty meaningful
  arrange(city, season, wind_mean_bin)

readr::write_csv(df_binned, "outputs/tables/Table_wind_bins_effect.csv")

# ----------------------------
# FIG07: Overall binned effect curve (by city)
# ----------------------------
p7 <- ggplot(df_binned, aes(x = wind_mean_bin, y = pm25_mean_bin, group = city, linetype = city)) +
  geom_ribbon(aes(ymin = pm25_ci_low, ymax = pm25_ci_high), alpha = 0.15) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Binned wind–PM2.5 effect with 95% CI (by season)",
    subtitle = "Each point is the mean PM2.5 within wind-speed bins; CI shows uncertainty",
    x = "Wind speed (binned mean)",
    y = expression(PM[2.5]~(mu*g/m^3)),
    linetype = "City"
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig07_pm25_wind_binned_effect_by_season.png", p7, width = 12, height = 7, dpi = 300)

# ----------------------------
# FIG08: Story B main figure (city × season small multiples)
# ----------------------------
p8 <- ggplot(df_binned, aes(x = wind_mean_bin, y = pm25_mean_bin)) +
  geom_ribbon(aes(ymin = pm25_ci_low, ymax = pm25_ci_high), alpha = 0.18) +
  geom_line(linewidth = 0.95) +
  facet_grid(season ~ city, scales = "free_y") +
  labs(
    title = "How the wind–PM2.5 relationship changes by city and season",
    subtitle = "Small multiples highlight context-dependent differences (Story B)",
    x = "Wind speed (binned mean)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave("outputs/figures/Fig08_wind_effect_city_by_season.png", p8, width = 13, height = 9, dpi = 300)

message("DONE Figures saved to outputs/figures and table saved to outputs/tables.")
