#!/usr/bin/env Rscript

# Collect micro-level public happiness data from ESS API.
# Data are saved to data/raw/ess_micro.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(readr)
  library(jsonlite)
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

fetch_ess_round <- function(round = 10) {
  # ESS JSON API endpoint (replace/extend with specific endpoint as needed).
  url <- sprintf("https://api.europeansocialsurvey.org/v2/rounds/%s", round)
  response <- request(url) |> req_perform()
  payload <- resp_body_string(response)
  data <- jsonlite::fromJSON(payload, flatten = TRUE)
  data_frame <- as.data.frame(data$data)

  # The exact schema varies by endpoint; this keeps the script as a clear outline.
  # Update field names below if your selected ESS endpoint differs.
  extract_or_na <- function(df, name, default = NA) {
    if (name %in% names(df)) {
      df[[name]]
    } else {
      rep(default, nrow(df))
    }
  }

  tibble::tibble(
    respondent_id = as.character(extract_or_na(data_frame, "id")),
    country_code = as.character(extract_or_na(data_frame, "cntry")),
    year = as.integer(extract_or_na(data_frame, "inwyr")),
    happiness = as.numeric(extract_or_na(data_frame, "happy")),
    weight = as.numeric(extract_or_na(data_frame, "dweight"))
  )
}

main <- function() {
  european_iso2 <- get_european_iso2()
  output_file <- "data/raw/ess_micro.csv"
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS is not true. Writing an empty micro template to ", output_file)
    readr::write_csv(tibble::tibble(respondent_id = character(), country_code = character(), year = integer(), happiness = double(), weight = double()), output_file)
    return(invisible(NULL))
  }

  ess_micro <- fetch_ess_round(round = 10) |>
    filter(country_code %in% european_iso2)

  readr::write_csv(ess_micro, output_file)
  message("Saved ESS micro data to ", output_file)
}

main()
