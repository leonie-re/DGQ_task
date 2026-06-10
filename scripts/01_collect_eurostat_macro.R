#!/usr/bin/env Rscript
# Eurostat macro indicators (Gini + unemployment) -> data/raw/eurostat_macro.csv

parse_jsonstat <- function(js) {
  dims <- js$id
  dim_info <- js$dimension

  cats <- lapply(dims, function(d) {
    idx <- dim_info[[d]]$category$index
    if (is.null(names(idx))) names(dim_info[[d]]$category$label) else names(idx)
  })

  n_cells <- prod(lengths(cats))
  if (n_cells == 0L) stop("parse_jsonstat: empty cube")

  grid <- do.call(expand.grid, c(rev(cats), stringsAsFactors = FALSE))
  grid <- grid[, rev(seq_along(grid)), drop = FALSE]
  names(grid) <- dims

  vals <- js$value
  grid$value <- NA_real_
  if (length(vals) > 0) {
    idx_numeric <- as.integer(names(vals))
    grid$value[idx_numeric + 1L] <- unlist(vals)
  }

  grid
}

fetch_eurostat <- function(dataset_id, extra_params = character()) {
  base_url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
    dataset_id
  )

  geo_codes <- c(
    "EU27_2020", "EA20",
    "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT",
    "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI",
    "SK", "FI", "SE", "IS", "NO", "CH", "UK", "ME", "MK", "AL", "RS", "TR", "XK"
  )

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      format = "JSON",
      lang = "EN",
      sinceTimePeriod = "2014",
      .multi = "explode"
    ) |>
    httr2::req_url_query(
      !!!setNames(as.list(geo_codes), rep("geo", length(geo_codes))),
      .multi = "explode"
    )

  if (length(extra_params) > 0) {
    req <- req |> httr2::req_url_query(!!!as.list(extra_params), .multi = "explode")
  }

  message("Fetching Eurostat dataset: ", dataset_id)
  resp <- req |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200L) {
    warning(
      "HTTP ", httr2::resp_status(resp), " for dataset '", dataset_id, "': ",
      httr2::resp_body_string(resp)
    )
    return(tibble::tibble())
  }

  js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df <- parse_jsonstat(js)

  names(df) <- tolower(names(df))
  if ("time" %in% names(df)) df <- dplyr::rename(df, year = time)
  if ("geo" %in% names(df)) df <- dplyr::rename(df, country_code = geo)

  df
}

eurostat_load <- function() {
  output_path <- file.path("data", "raw", "eurostat_macro.csv")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS is false. Writing empty template to ", output_path)
    readr::write_csv(
      tibble::tibble(
        country_code = character(),
        year = integer(),
        gini = double(),
        unemployment_rate = double()
      ),
      output_path
    )
    return(invisible(NULL))
  }

  df_gini_raw <- fetch_eurostat(
    dataset_id = "ilc_di12",
    extra_params = c(age = "TOTAL", statinfo = "GINI_HND")
  )
  df_gini <- df_gini_raw |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(country_code, year, gini = value)

  df_unemp_raw <- fetch_eurostat(
    dataset_id = "une_rt_a",
    extra_params = c(age = "Y15-74", sex = "T", unit = "PC_ACT")
  )
  df_unemp <- df_unemp_raw |>
    dplyr::filter(!is.na(value)) |>
    dplyr::select(country_code, year, unemployment_rate = value)

  df_macro <- dplyr::full_join(df_gini, df_unemp, by = c("country_code", "year")) |>
    dplyr::mutate(year = as.integer(year), country_code = toupper(country_code)) |>
    dplyr::arrange(country_code, year)

  readr::write_csv(df_macro, output_path)
  message("Saved: ", output_path, " (", nrow(df_macro), " rows)")

  invisible(df_macro)
}

eurostat_load()
