#!/usr/bin/env Rscript
# Setup
# Load packages

suppressPackageStartupMessages({
  install.packages("pacman")
  library(pacman)
  p_load("dplyr", "httr2", "readr", "countrycode", "jsonlite", "ggplot2", "tibble", "future", "future.apply", "naniar", "lubridate", "tidyr", "knitr")
})

# Build a consistent list of ISO2 country codes for all European countries.
get_european_iso2 <- function() {
  countrycode::codelist_panel |>
    dplyr::filter(continent == "Europe", !is.na(iso2c)) |>
    dplyr::distinct(iso2c) |>
    dplyr::pull(iso2c) |>
    toupper() |>
    sort()
}

european_iso2 <- get_european_iso2()

micro_path <- "data/raw/ess_micro.csv"
macro_path <- "data/raw/eurostat_macro.csv"

# Master pipeline script that runs data collection, merge, and analysis outline.
pipeline_steps <- c(
  "scripts/01_collect_eurostat_macro.R",
  "scripts/02_collect_ess_micro.R",
  "scripts/03_merge_micro_macro.R",
  "scripts/04_analysis_outline.R"
)

for (step in pipeline_steps) {
  message("Running: ", step)
  source(step, local = new.env())
}

message("Pipeline finished successfully.")
