#!/usr/bin/env Rscript

assert_required_columns <- function(df, cols, df_name) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required columns: ", paste(missing, collapse = ", "))
  }
}

round_to_year <- c(`6` = 2012L, `7` = 2014L, `8` = 2016L, `9` = 2018L, `10` = 2020L, `11` = 2023L)
target_ess_rounds <- 7:11
age_group_breaks <- c(15, 30, 45, 60, 120)
age_group_labels <- c("15-29", "30-44", "45-59", "60+")

analysis_scope <- tibble::tribble(
  ~topic, ~value,
  "objective", "Observational association (non-causal) between inequality and happiness",
  "primary_outcome", "happy",
  "primary_exposure", "gini",
  "macro_control", "unemployment_rate",
  "micro_controls", "agea,hinctnta,evmar",
  "target_population", "European countries, ESS rounds 7-11, survey years 2014+",
  "missing_data_policy", "Keep all rows in analysis dataset; use complete-case sample for model variables"
)

macro_input <- "data/raw/eurostat_macro.csv"
micro_input <- "data/raw/ess_micro.csv"

if (!file.exists(macro_input) || !file.exists(micro_input)) {
  stop("Run scripts/01_collect_eurostat_macro.R and scripts/02_collect_ess_micro.R first.")
}

if (exists("european_iso2")) {
  european_iso2_codes <- european_iso2
} else {
  european_iso2_codes <- countrycode::codelist_panel |>
    dplyr::filter(continent == "Europe", !is.na(iso2c)) |>
    dplyr::distinct(iso2c) |>
    dplyr::pull(iso2c) |>
    toupper() |>
    sort()
}

macro_raw <- readr::read_csv(macro_input, show_col_types = FALSE)
ess_raw <- readr::read_csv(micro_input, show_col_types = FALSE)

assert_required_columns(macro_raw, c("country_code", "year", "gini", "unemployment_rate"), "macro_raw")
assert_required_columns(ess_raw, c("idno", "cntry", "essround", "agea", "happy", "evmar", "hinctnta"), "ess_raw")

macro <- macro_raw |>
  dplyr::transmute(
    country_code = toupper(country_code),
    year = as.integer(year),
    gini = as.numeric(gini),
    unemployment_rate = as.numeric(unemployment_rate)
  ) |>
  dplyr::filter(country_code %in% european_iso2_codes)

micro <- ess_raw |>
  dplyr::transmute(
    idno = idno,
    country_code = toupper(cntry),
    essround = as.integer(essround),
    proddate = proddate,
    agea = dplyr::if_else(agea == 999 | agea < 15 | agea > 120, NA_real_, as.numeric(agea)),
    happy = dplyr::if_else(happy < 0 | happy > 10, NA_real_, as.numeric(happy)),
    evmar = dplyr::if_else(evmar %in% c(77, 88, 99), NA_real_, as.numeric(evmar)),
    hinctnta = dplyr::if_else(hinctnta < 1 | hinctnta > 10, NA_real_, as.numeric(hinctnta))
  ) |>
  dplyr::mutate(
    survey_year = unname(round_to_year[as.character(essround)]),
    macro_year = survey_year,
    macro_year_lag = survey_year - 1L,
    country_code = toupper(country_code)
  ) |>
  dplyr::filter(
    country_code %in% european_iso2_codes,
    essround %in% target_ess_rounds,
    !is.na(survey_year)
  )

assert_required_columns(micro, c("country_code", "survey_year", "macro_year", "macro_year_lag"), "micro")
if (nrow(micro) == 0) stop("No ESS rows after filtering. Check round/year mapping and country filters.")
if (nrow(macro) == 0) stop("No macro rows after filtering. Check Eurostat extraction.")

macro_main <- macro |>
  dplyr::rename(macro_year = year)

macro_lag <- macro |>
  dplyr::rename(
    macro_year_lag = year,
    gini_lag1 = gini,
    unemployment_rate_lag1 = unemployment_rate
  )

analysis_data <- micro |>
  dplyr::left_join(macro_main, by = c("country_code", "macro_year")) |>
  dplyr::left_join(macro_lag, by = c("country_code", "macro_year_lag")) |>
  dplyr::mutate(
    year = survey_year,
    gini_centered = gini - mean(gini, na.rm = TRUE),
    gini_scaled = as.numeric(scale(gini)),
    happy_scaled = as.numeric(scale(happy)),
    age_group = cut(agea, breaks = age_group_breaks, right = FALSE, labels = age_group_labels),
    income_decile = hinctnta,
    model_complete_case = stats::complete.cases(happy, gini, unemployment_rate, agea, hinctnta, evmar)
  ) |>
  dplyr::arrange(country_code, essround, idno)

country_year_pairs <- micro |>
  dplyr::distinct(country_code, macro_year)
matched_country_year_pairs <- analysis_data |>
  dplyr::filter(!is.na(gini) & !is.na(unemployment_rate)) |>
  dplyr::distinct(country_code, macro_year)

coverage_share <- if (nrow(country_year_pairs) == 0) {
  NA_real_
} else {
  nrow(matched_country_year_pairs) / nrow(country_year_pairs)
}

diagnostics <- tibble::tribble(
  ~metric, ~value,
  "n_macro_rows", as.numeric(nrow(macro)),
  "n_micro_rows_filtered", as.numeric(nrow(micro)),
  "n_analysis_rows", as.numeric(nrow(analysis_data)),
  "country_year_match_share", as.numeric(coverage_share),
  "missing_rate_gini_main", mean(is.na(analysis_data$gini)),
  "missing_rate_unemployment_main", mean(is.na(analysis_data$unemployment_rate)),
  "missing_rate_gini_lag1", mean(is.na(analysis_data$gini_lag1)),
  "missing_rate_unemployment_lag1", mean(is.na(analysis_data$unemployment_rate_lag1)),
  "n_complete_case_model", as.numeric(sum(analysis_data$model_complete_case, na.rm = TRUE))
)

output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(macro, file.path(output_dir, "macro_clean.csv"))
readr::write_csv(micro, file.path(output_dir, "micro_clean.csv"))
readr::write_csv(analysis_data, file.path(output_dir, "analysis_dataset.csv"))
readr::write_csv(diagnostics, file.path(output_dir, "analysis_diagnostics.csv"))
readr::write_csv(analysis_scope, file.path(output_dir, "analysis_scope.csv"))

# Backward-compatible paths
readr::write_csv(macro, "data/macro.csv")
readr::write_csv(micro, "data/micro.csv")
readr::write_csv(analysis_data, "data/analysis_data.csv")

message("Saved processed datasets and diagnostics in data/processed/")
