# IJC445 – Air Quality Visualisation  
**ASSERT-Driven Visual Analytics of PM2.5 and Meteorology in UK Cities**

---

## 📌 Module Information
- **Module:** IJC445 – Data Visualisation  
- **Programme:** MSc Data Science  
- **Institution:** University of Sheffield  
- **Student:** Khalil Alakbarzade  
- **Academic Year:** 2025–2026  

This repository contains the complete coursework for **IJC445 Data Visualisation**, focusing on **PM2.5 air pollution** and its relationship with **meteorological conditions**, using the **ASSERT framework** and the **Grammar of Graphics**.

The work builds on the same dataset used in **IJC437 (Introduction to Data Science)** but shifts the focus from modelling to **knowledge generation through visualisation**.

---

## 🎯 Project Overview

Air pollution, particularly **PM2.5**, poses significant public health risks in urban environments.  
This project explores:

- Temporal patterns in PM2.5 across UK cities  
- The relationship between **wind speed** and PM2.5  
- Seasonal and spatial differences in pollution  
- How **visualisation choices influence interpretation**

The analysis uses **city-level daily averages** and produces a **composite visualisation** supported by theoretical discussion on visual design, accessibility, and ethics.

---

## 🧠 Visualisation Frameworks Used

### ASSERT Framework
All visualisations follow the **ASSERT** process:

1. **Ask** – Define analytical and narrative questions  
2. **Search** – Identify relevant data and context  
3. **Structure** – Clean, aggregate, and validate data  
4. **Envision** – Choose visual encodings and layouts  
5. **Represent** – Implement visuals using ggplot2  
6. **Tell** – Communicate insights and implications  

### Grammar of Graphics
Visuals are constructed using:
- Aesthetic mappings (`x`, `y`, `colour`, `size`)
- Geometries (lines, points, hex bins, ribbons)
- Faceting for comparison
- Scales and coordinate systems
- Minimal themes for clarity

---

## 🗂 Repository Structure
```
IJC445-Air-Quality-Visualisation/
│
├── data/
│   └── processed/
│       └── pm25_weather_daily.csv
│
├── scripts/
│   ├── 01_visualisation_assert.R
│   ├── 02_visualisation_assert_advanced.R
│   ├── 02_wind_storyB_figures.R
│   ├── 03_map_visualisations.R
│   └── 04_map_anomaly_pm25.R
│
├── outputs/
│   ├── figures/
│   │   ├── Fig01_pm25_trend_monthly_by_city.png
│   │   ├── Fig02_pm25_boxplot_by_city.png
│   │   ├── Fig03_wind_vs_pm25_by_city.png
│   │   ├── Fig04_wind_vs_pm25_by_season.png
│   │   ├── Fig05_pm25_day_of_week.png
│   │   ├── Fig06_pm25_wind_hex_by_city.png
│   │   ├── Fig07_pm25_wind_binned_effect.png
│   │   ├── Fig08_wind_effect_city_by_season.png
│   │   ├── Fig09_map_pm25_mean_by_city.png
│   │   ├── Fig10_map_wind_mean_by_city.png
│   │   ├── Fig11_map_pm25_wind_quadrants.png
│   │   ├── Fig12_map_pm25_seasonal_smallmultiples.png
│   │   └── Fig13_map_pm25_seasonal_anomaly.png
│   │
│   └── tables/
│       ├── city_summary_pm25_wind.csv
│       ├── qa_city_coverage.csv
│       ├── Table_wind_bins_effect.csv
│       └── Table_pm25_seasonal_anomaly.csv
│
├── README.md
└── .gitignore
```

---

## 🗺 Cities Analysed
- London  
- Manchester  
- Birmingham  
- Sheffield  

---

## 📊 Key Visualisations

The coursework includes:
- Time-series trends with rolling means  
- Distributional comparisons (boxplots, hex bins)  
- Seasonal and city-level small multiples  
- Map-based spatial summaries  
- Seasonal anomaly maps (examiner-focused visual)  

Each figure is explicitly discussed in the written report using theory from visualisation literature.

---

## ⚙️ How to Run the Code

### 1️⃣ Install required packages
```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "janitor",
  "scales",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "hexbin"
))
```

### 2️⃣ Set working directory
```r
setwd("path/to/IJC445-Air-Quality-Visualisation")
```

### 3️⃣ Run scripts in order
```r
source("scripts/01_visualisation_assert.R")
source("scripts/02_visualisation_assert_advanced.R")
source("scripts/02_wind_storyB_figures.R")
source("scripts/03_map_visualisations.R")
source("scripts/04_map_anomaly_pm25.R")
```

All figures will be saved automatically to:  
outputs/figures/

🎓 Learning Outcomes Demonstrated
- Application of ASSERT framework
- Effective use of Grammar of Graphics
- Ethical and accessible visual design
- Spatial and temporal data visualisation
- Storytelling with composite visuals
- Reproducible analytical workflow

⸻

⚠️ Notes
- Figures are designed for static academic reporting, not dashboards
- Data are aggregated to city-level daily means
- Interpretations are descriptive, not causal

⸻

📬 Contact

Khalil Alakbarzade  
MSc Data Science  
University of Sheffield

GitHub: https://github.com/KhalilAlak

---

## License

This repository is for academic coursework and portfolio demonstration.