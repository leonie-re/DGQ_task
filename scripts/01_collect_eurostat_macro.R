#!/usr/bin/env Rscript

# Collect macro-level indicators from Eurostat SDMX 3.0 API.
# Data are filtered to European countries and saved to data/raw/eurostat_macro.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(readr)
  library(countrycode)
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

# Download one SDMX-CSV endpoint and keep selected columns.
fetch_eurostat_indicator <- function(dataset_id, filters, value_name) {
  base_url <- sprintf("https://api.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/%s/1.0", dataset_id)
  req <- request(base_url)

  # Add all filters as query parameters.
  for (n in names(filters)) {
    req <- req |> req_url_query(!!n := filters[[n]])
  }

  # Request CSV to simplify downstream handling in R scripts.
  response <- req |>
    req_url_query(format = "csvdata") |>
    req_perform()

  tmp <- tempfile(fileext = ".csv")
  resp_body_file(response, tmp)

  indicator_data <- readr::read_csv(tmp, show_col_types = FALSE)
  value_col <- if ("OBS_VALUE" %in% names(indicator_data)) {
    "OBS_VALUE"
  } else if ("obs_value" %in% names(indicator_data)) {
    "obs_value"
  } else {
    stop("Eurostat response did not include an OBS_VALUE column.")
  }

  indicator_data |>
    dplyr::mutate(value = .data[[value_col]]) |>
    dplyr::transmute(
      country_code = geo,
      year = as.integer(TIME_PERIOD),
      !!value_name := value
    )
}

main <- function() {
  european_iso2 <- get_european_iso2()
  output_file <- "data/raw/eurostat_macro.csv"
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Use an env switch so local checks can run without hitting external APIs.
  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS is not true. Writing an empty macro template to ", output_file)
    readr::write_csv(tibble::tibble(country_code = character(), year = integer(), gini = double(), unemployment_rate = double()), output_file)
    return(invisible(NULL))
  }

  # Indicator 1: Income inequality (Gini coefficient, disposable income).
  gini <- fetch_eurostat_indicator(
    dataset_id = "ilc_di12",
    filters = list(geo = paste(european_iso2, collapse = ","), unit = "PC", indic_il = "GINI", time = "2010:2025"),
    value_name = "gini"
  )

  # Indicator 2: Unemployment rate (% of active population).
  unemployment <- fetch_eurostat_indicator(
    dataset_id = "une_rt_a",
    filters = list(geo = paste(european_iso2, collapse = ","), unit = "PC_ACT", sex = "T", age = "Y15-74", time = "2010:2025"),
    value_name = "unemployment_rate"
  )

  macro <- gini |>
    full_join(unemployment, by = c("country_code", "year")) |>
    arrange(country_code, year)

  readr::write_csv(macro, output_file)
  message("Saved Eurostat macro data to ", output_file)
}

main()
