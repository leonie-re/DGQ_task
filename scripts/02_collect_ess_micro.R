#!/usr/bin/env Rscript
# ESS micro data -> data/raw/ess_micro.csv

ESS_VARS <- c(
  "idno", "cntry", "essround", "proddate", "agea", "happy", "evmar", "hinctnta"
)

fetch_ess_round <- function(doi, user_id) {
  url <- paste0("https://api.ess.sikt.no/v1/data/dataFile/", doi)
  message("Fetching ESS DOI: ", doi)

  resp <- httr2::request(url) |>
    httr2::req_url_query(userId = user_id, fileFormat = "csv") |>
    httr2::req_timeout(120) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200L) {
    warning(
      "HTTP ", httr2::resp_status(resp), " for DOI '", doi, "': ",
      httr2::resp_body_string(resp)
    )
    return(tibble::tibble())
  }

  raw_csv <- httr2::resp_body_string(resp)
  df <- readr::read_csv(I(raw_csv), show_col_types = FALSE, na = c("", "NA"))

  missing_vars <- setdiff(ESS_VARS, names(df))
  if (length(missing_vars) > 0) {
    warning("Missing variables in ", doi, ": ", paste(missing_vars, collapse = ", "))
  }
  for (v in missing_vars) df[[v]] <- NA

  dplyr::select(df, dplyr::all_of(ESS_VARS))
}

ess_load <- function() {
  output_path <- file.path("data", "raw", "ess_micro.csv")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS is false. Writing empty template to ", output_path)
    readr::write_csv(
      tibble::tibble(
        idno = integer(),
        cntry = character(),
        essround = integer(),
        proddate = character(),
        agea = integer(),
        happy = integer(),
        evmar = integer(),
        hinctnta = integer()
      ),
      output_path
    )
    return(invisible(NULL))
  }

  user_id <- Sys.getenv("ESS_USER_ID", "62ac71b7-13fc-458d-b147-34543a42669b")

  rounds <- list(
    list(doi = "10.21338/ess6e02_6", round = 6),
    list(doi = "10.21338/ess7e02_2", round = 7),
    list(doi = "10.21338/ess8e02_3", round = 8),
    list(doi = "10.21338/ess9e03_3", round = 9),
    list(doi = "10.21338/ess10e03_3", round = 10),
    list(doi = "10.21338/ess11e04_1", round = 11)
  )

  future::plan(future::multisession, workers = 3)
  on.exit(future::plan(future::sequential), add = TRUE)

  dfs <- future.apply::future_lapply(rounds, function(r) {
    fetch_ess_round(doi = r$doi, user_id = user_id)
  })

  ess_micro <- dplyr::bind_rows(dfs) |>
    dplyr::arrange(cntry, essround, idno)

  readr::write_csv(ess_micro, output_path)
  message("Saved: ", output_path, " (", nrow(ess_micro), " rows)")

  invisible(ess_micro)
}

ess_load()
