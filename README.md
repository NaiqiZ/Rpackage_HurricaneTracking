# HurricaneTracking_Rpackage

This repository contains an R package developed for hurricane tracking and visualization using official storm records from the HURDAT Atlantic basin tropical cyclone dataset.

---

## Project Overview

We created an R package to model and analyze tropical storm activity from 2020 to 2022. The tool supports spatial interpolation, visualization, and analysis of hurricane behavior and trends.

- Built with data from the **HURDAT Atlantic tropical cyclone database**
- Focused on tracking storm progression, estimating storm energy, and visualizing landfall patterns
- Designed for use in climate research and storm modeling

---

## Technical Features

- **30-Minute Track Interpolation:** Custom functions interpolate storm trajectories to 30-minute intervals for more granular visualization.
- **Storm Track Visualization:** Vignette includes plots of storm size and paths from 2020 to 2022 using spatial data.
- **Energy & Climate Analysis:** The package supports analysis of landfall frequency, storm intensity, and seasonal climate signals.

---

## Usage

The package includes a vignette that demonstrates how to:

- Use interpolation functions to transform raw HURDAT storm track data into high-resolution 30-minute intervals
- Visualize interpolated tracks on geographic plots with storm size and strength encoded with three concentric circles
- Analyze storm behavior across years with built-in plotting and summary tools

To access the vignette in R:
```r
# Load the package
library(Rpackage_HurricaneTracking)

# Open the vignette
vignette("StormTracking_Vignette")
```
## Team
#Members: Naiqi Zhang, Kayla Yang
#The product is developed for Cornell Statistical Computing Project

