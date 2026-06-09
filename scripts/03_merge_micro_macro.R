#!/usr/bin/env Rscript

# Merge ESS micro data and Eurostat macro data on country and year.
# Keeps only European countries and writes data/processed/analysis_dataset.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(countrycode)
})

get_european_iso2 <- function() {
  countrycode::codelist_panel |>
    dplyr::filter(continent == "Europe", !is.na(iso2c)) |>
    dplyr::distinct(iso2c) |>
    dplyr::pull(iso2c) |>
    toupper() |>
    sort()
}

main <- function() {
  micro_path <- "data/raw/ess_micro.csv"
  macro_path <- "data/raw/eurostat_macro.csv"
  output_path <- "data/processed/analysis_dataset.csv"

  if (!file.exists(micro_path) || !file.exists(macro_path)) {
    stop("Run scripts/01_collect_eurostat_macro.R and scripts/02_collect_ess_micro.R first.")
  }

  european_iso2 <- get_european_iso2()

  micro <- readr::read_csv(micro_path, show_col_types = FALSE) |>
    mutate(country_code = toupper(country_code)) |>
    filter(country_code %in% european_iso2)

  macro <- readr::read_csv(macro_path, show_col_types = FALSE) |>
    mutate(country_code = toupper(country_code)) |>
    filter(country_code %in% european_iso2)

  merged <- micro |>
    left_join(macro, by = c("country_code", "year"))

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(merged, output_path)
  message("Saved merged micro/macro data to ", output_path)
}

main()
