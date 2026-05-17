# GNSS Interference Detection — Estonian Reference Stations

R scripts for detecting and visualizing GNSS signal interference in
Estonian permanent reference stations, based on 15-minute SNR data
across the L1, L2 and L5 frequency bands. Developed as part of a
master's thesis on analyzing ducting occurrence in Estonia.

## Scripts

- **`GNSS_püsijaama_andmete_andmetöötlus.R`** — single-station analysis. Reads one
  station's SNR CSV file, detects interference events per signal and
  observation regime, and outputs an interactive HTML visualization,
  a reference-level table (CSV) and per-epoch detection flags (`.rds`).

- **`Kõigi_GNSS_püsijaamade_andmete_andmetöötlus.R`** — multi-station aggregation.
  Reads `.rds` files produced by the single-station script and
  generates six country-wide visualizations (timeline, duration
  distribution, diurnal patterns, intensity variability, daily totals,
  seasonal breakdown).

## Requirements

R 4.0 or later. Packages: `plotly`, `htmlwidgets`, `htmltools`,
`svDialogs`, `dplyr`, `lubridate`, `tidyr`.

## Data and results

The input SNR data, detection results (per-station and country-wide)
and all generated visualizations are archived separately on Zenodo:

> https://doi.org/10.5281/zenodo.20260127
