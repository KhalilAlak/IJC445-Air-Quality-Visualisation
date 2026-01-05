# ==========================================
# 02_visualisation_assert_advanced.R  (IJC445)
# Dataset: Air Quality (OpenAQ) + Weather (Open-Meteo)
# Level: city-level DAILY averages (full range)
# Variable focus: wind speed
# Framework: ASSERT + Grammar of Graphics
#
# Focus:
#  - Strong ASSERT story: ASK → SEARCH → STRUCTURE → ENVISION → REPRESENT → TELL
#  - Reduce overplotting properly (hex / binning / summarise) + show uncertainty
#  - Explicit design choices that you can explain in the write-up
#
# Inputs:
#  - data/processed/pm25_weather_daily.csv
#
# Outputs:
#  - outputs/figures/Fig06_pm25_wind_hex_by_city.png
#  - outputs/figures/Fig07_pm25_wind_binned_effect.png
#  - outputs/figures/Fig08_wind_effect_by_city_month.png
#  - outputs/tables/Table_wind_bins_effect.csv
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)

# hexbin is required for geom_hex
if (!requireNamespace("hexbin", quietly = TRUE)) {
  install.packages("hexbin")
}

# ---------- Folders ----------
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ==========================================
# ASSERT: ASK
# Q1: Is higher wind speed consistently associated with lower PM2.5?
# Q2: Does the relationship differ by city and by season/month?
# Q3: Can we summarise the relationship without overplotting and with uncertainty?
# ==========================================

# ==========================================
# ASSERT: SEARCH
# Use merged daily city dataset from IJC437
# ==========================================
path <- "data/processed/pm25_weather_daily.csv"
stopifnot(file.exists(path))

df <- readr::read_csv(path, show_col_types = FALSE) %>%
  janitor::clean_names()

# ==========================================
# ASSERT: STRUCTURE (minimal but explicit)
# ==========================================
required_cols <- c("city", "date", "pm25_mean", "wind_mean")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df_viz <- df %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city),
    pm25_mean = as.numeric(pm25_mean),
    wind_mean = as.numeric(wind_mean),
    month = lubridate::month(date),
    month_lbl = lubridate::month(date, label = TRUE, abbr = TRUE),
    season = dplyr::case_when(
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

# ==========================================
# ASSERT: ENVISION
# Design choices:
#  - Hex density plot: shows relationship while avoiding overplotting
#  - Binned wind → mean PM2.5 with CI: interpretable effect curve
#  - Month-level small multiples: shows seasonal shifts per city
# ==========================================

# ==========================================
# ASSERT: REPRESENT
# ==========================================

# -------- Visual 6: Hex/bin density relationship (minimal overplotting) --------
# Keep y readable by capping view at 99th percentile (does NOT remove data)
y_cap <- quantile(df_viz$pm25_mean, 0.99, na.rm = TRUE)

p6 <- ggplot(df_viz, aes(x = wind_mean, y = pm25_mean)) +
  geom_hex(bins = 35) +
  facet_wrap(~ city, scales = "free_y") +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = TRUE,
    linewidth = 0.9,
    colour = "white",
    na.rm = TRUE
  ) +
  coord_cartesian(ylim = c(0, y_cap)) +
  labs(
    title = "Wind speed vs PM2.5 (density view)",
    subtitle = "Hex-binning reduces overplotting; GAM smooth shows central tendency with uncertainty",
    x = "Mean wind speed (10m)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/Fig06_pm25_wind_hex_by_city.png",
  plot = p6,
  width = 12,
  height = 7,
  dpi = 300
)

# -------- Build binned effect table (TELL-ready) --------
# Use percentile-based breaks so bins are populated.
wind_breaks <- quantile(df_viz$wind_mean, probs = seq(0, 1, by = 0.05), na.rm = TRUE)
wind_breaks <- unique(as.numeric(wind_breaks))

# Safety: need at least 2 unique breakpoints
stopifnot(length(wind_breaks) >= 2)

# Helper to compute bin midpoint robustly
bin_mid <- function(x_factor) {
  labs <- levels(x_factor)
  # labs look like "[a,b]" or "(a,b]" etc.
  parse_mid <- function(s) {
    s2 <- gsub("\\[|\\]|\\(|\\)", "", s)
    parts <- strsplit(s2, ",")[[1]]
    lo <- suppressWarnings(as.numeric(trimws(parts[1])))
    hi <- suppressWarnings(as.numeric(trimws(parts[2])))
    (lo + hi) / 2
  }
  mids <- vapply(labs, parse_mid, numeric(1))
  mids[as.integer(x_factor)]
}

# Bin and summarise
df_binned_raw <- df_viz %>%
  mutate(
    wind_bin = cut(wind_mean, breaks = wind_breaks, include.lowest = TRUE, right = TRUE)
  ) %>%
  filter(!is.na(wind_bin)) %>%
  mutate(wind_bin_mid = bin_mid(wind_bin))

# Summaries with CI by city and season
# (CI uses normal approx; OK here because each bin has decent n after filtering)
df_bins <- df_binned_raw %>%
  group_by(city, season, wind_bin) %>%
  summarise(
    n = n(),
    pm25_mean = mean(pm25_mean, na.rm = TRUE),
    pm25_sd = sd(pm25_mean, na.rm = TRUE),
    pm25_se = pm25_sd / sqrt(n),
    pm25_ci_low = pm25_mean - 1.96 * pm25_se,
    pm25_ci_high = pm25_mean + 1.96 * pm25_se,
    wind_mean_bin = mean(wind_mean, na.rm = TRUE),
    wind_mid_bin = mean(wind_bin_mid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 30) %>%
  arrange(city, season, wind_mean_bin)

readr::write_csv(df_bins, "outputs/tables/Table_wind_bins_effect.csv")

# -------- Visual 7: Binned wind effect with uncertainty (by city, faceted by season) --------
# Use wind_mean_bin for x ordering; ribbons show CI.
p7 <- ggplot(df_bins, aes(x = wind_mean_bin, y = pm25_mean, colour = city, group = city)) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(
    aes(ymin = pm25_ci_low, ymax = pm25_ci_high, fill = city),
    alpha = 0.12,
    colour = NA
  ) +
  facet_wrap(~ season, scales = "free_y") +
  labs(
    title = "Average PM2.5 across wind-speed bins",
    subtitle = "Lines show mean PM2.5; ribbons show 95% CI (bins with n ≥ 30)",
    x = "Wind speed (binned mean)",
    y = expression(PM[2.5]~(mu*g/m^3)),
    colour = "City",
    fill = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/Fig07_pm25_wind_binned_effect.png",
  plot = p7,
  width = 12,
  height = 7,
  dpi = 300
)

# -------- Visual 8: City × Month: wind–PM2.5 relationship (monthly means) --------
# Monthly aggregation reduces noise; linear fits summarise direction per city-month.
monthly_city <- df_viz %>%
  mutate(month_start = floor_date(date, "month")) %>%
  group_by(city, month_start) %>%
  summarise(
    pm25_mean = mean(pm25_mean, na.rm = TRUE),
    wind_mean = mean(wind_mean, na.rm = TRUE),
    month_lbl = month(month_start, label = TRUE, abbr = TRUE),
    .groups = "drop"
  )

p8 <- ggplot(monthly_city, aes(x = wind_mean, y = pm25_mean)) +
  geom_point(alpha = 0.65, size = 1.1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, na.rm = TRUE) +
  facet_grid(city ~ month_lbl) +
  labs(
    title = "Wind–PM2.5 relationship across months (monthly means)",
    subtitle = "Monthly aggregation reduces noise; linear fits summarise direction per city-month",
    x = "Mean wind speed (monthly)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 9)
  )

ggsave(
  filename = "outputs/figures/Fig08_wind_effect_by_city_month.png",
  plot = p8,
  width = 16,
  height = 9,
  dpi = 300
)

message("\nDONE ✅ Advanced wind visualisations saved to outputs/figures/")
message("Table saved to outputs/tables/Table_wind_bins_effect.csv")
