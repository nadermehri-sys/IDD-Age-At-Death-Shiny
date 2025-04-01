# IDD Age-at-Death Data Tracker

**Author:** Nader Mehri, PhD  
**Date:** 2025-04-01  

This repository contains an **R Shiny** application designed to visualize and analyze age-at-death trends among adults with Intellectual and Developmental Disabilities (IDD) in the United States. Using interactive maps, visualizations, and downloadable datasets, it provides insights into disparities by geography, year, sex, and race/ethnicity.

## Overview

The **IDD Age-at-Death Data Tracker** offers interactive tools and visuals to explore publicly available mortality data, aimed at researchers, policymakers, and practitioners to better understand and address disparities affecting IDD populations.

---

## Features

### Interactive Map (Leaflet)
- **State-level quartiles**: Age-at-death statistics displayed geographically.
- **Interactive Tooltips**: Hover to see detailed state-level data and rankings.

### Interactive Visualizations (Plotly)
- **Time Trends**: Annual age-at-death data (2008–2017).
- **Sex-based Comparison**: Male vs. female age-at-death differences.
- **Race/Ethnicity Distribution**: Breakdown across racial/ethnic categories.

### Data Export
- CSV downloads available for each dataset (map, year, sex, race).

---

## Static App PDF

A detailed, static description and overview of the app with sample visuals is available here:

📄 [Static IDD Data Tracker PDF](https://surface.syr.edu/cgi/viewcontent.cgi?article=1125&amp;context=lerner&_gl=1*16m7rtq*_gcl_au*Njc2NjI2ODU3LjE3NDM0ODMwMTk.*_ga*MTQyNTcwMzc2MS4xNzQzNDgzMDE5*_ga_QT13NN6N9S*MTc0MzQ4MzAxOS4xLjAuMTc0MzQ4MzAxOS42MC4wLjA.)


---

## Repository Structure

```plaintext
IDD_DataTracker/
├── app.R                  # Main Shiny application (UI + Server logic)
├── data.RData             # Preprocessed datasets (e.g., IDD_nhmap, IDD_plot_year)
├── static_map.pdf         # Static PDF overview of the application
├── [optional scripts]     # Any additional helper scripts or resources
└── README.md              # Documentation and usage instructions
