#!/usr/bin/env Rscript

input_path <- "data/processed/analysis_dataset.csv"
if (!file.exists(input_path)) {
  stop("Run scripts/03_merge_micro_macro.R first.")
}

data <- readr::read_csv(input_path, show_col_types = FALSE)
required_cols <- c(
  "country_code", "year", "happy", "gini", "unemployment_rate",
  "agea", "hinctnta", "evmar", "model_complete_case", "gini_lag1", "unemployment_rate_lag1"
)
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop("Analysis dataset missing columns: ", paste(missing_cols, collapse = ", "))
}

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

analysis_data <- data |> dplyr::filter(model_complete_case)
if (nrow(analysis_data) == 0) stop("No complete-case observations for modelling.")

tidy_lm <- function(model, model_name) {
  sm <- summary(model)
  coefs <- as.data.frame(sm$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  names(coefs) <- c("estimate", "std_error", "statistic", "p_value", "term")
  coefs$model <- model_name
  coefs$r_squared <- sm$r.squared
  coefs$adj_r_squared <- sm$adj.r.squared
  coefs$n <- stats::nobs(model)
  coefs[, c("model", "term", "estimate", "std_error", "statistic", "p_value", "r_squared", "adj_r_squared", "n")]
}

# Stage A: descriptive diagnostics
missingness <- data |>
  dplyr::summarise(
    dplyr::across(c(happy, gini, unemployment_rate, agea, hinctnta, evmar, gini_lag1),
      ~ mean(is.na(.x)))
  ) |>
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "missing_rate")
readr::write_csv(missingness, "output/tables/missingness_summary.csv")

descriptive <- data |>
  dplyr::group_by(country_code, year) |>
  dplyr::summarise(
    mean_happy = mean(happy, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    mean_unemployment = mean(unemployment_rate, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )
readr::write_csv(descriptive, "output/tables/country_year_descriptives.csv")

p_scatter <- ggplot2::ggplot(descriptive, ggplot2::aes(x = mean_gini, y = mean_happy, color = country_code)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, color = "black") +
  ggplot2::labs(
    title = "Country-year happiness and inequality",
    x = "Mean Gini",
    y = "Mean happiness"
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave("output/figures/happiness_vs_gini_country_year.png", p_scatter, width = 9, height = 6)

p_hist_happy <- ggplot2::ggplot(data, ggplot2::aes(x = happy)) +
  ggplot2::geom_histogram(bins = 11, fill = "steelblue", color = "white") +
  ggplot2::labs(title = "Distribution of happiness", x = "Happiness", y = "Count") +
  ggplot2::theme_minimal()
ggplot2::ggsave("output/figures/happiness_distribution.png", p_hist_happy, width = 8, height = 5)

# Stage B/C/D: inferential models
m1 <- stats::lm(happy ~ gini, data = analysis_data)
m2 <- stats::lm(happy ~ gini + unemployment_rate, data = analysis_data)
m3 <- stats::lm(happy ~ gini + unemployment_rate + agea + hinctnta + factor(evmar), data = analysis_data)
m4_fe <- stats::lm(happy ~ gini + unemployment_rate + agea + hinctnta + factor(evmar) + factor(country_code) + factor(year), data = analysis_data)

# Robustness
analysis_data <- analysis_data |> dplyr::mutate(log_gini = log(gini))
m5_log <- stats::lm(happy ~ log_gini + unemployment_rate + agea + hinctnta + factor(evmar), data = analysis_data)
m6_quad <- stats::lm(happy ~ gini + I(gini^2) + unemployment_rate + agea + hinctnta + factor(evmar), data = analysis_data)
m7_lag <- stats::lm(happy ~ gini_lag1 + unemployment_rate_lag1 + agea + hinctnta + factor(evmar),
  data = analysis_data |> dplyr::filter(!is.na(gini_lag1), !is.na(unemployment_rate_lag1)))

country_year_counts <- analysis_data |>
  dplyr::distinct(country_code, year) |>
  dplyr::count(country_code, name = "n_years")
max_years <- max(country_year_counts$n_years, na.rm = TRUE)
balanced_countries <- country_year_counts |>
  dplyr::filter(n_years == max_years) |>
  dplyr::pull(country_code)
m8_balanced <- stats::lm(happy ~ gini + unemployment_rate + agea + hinctnta + factor(evmar),
  data = analysis_data |> dplyr::filter(country_code %in% balanced_countries))

leave_one_country_out <- lapply(sort(unique(analysis_data$country_code)), function(cty) {
  mod <- stats::lm(happy ~ gini + unemployment_rate + agea + hinctnta + factor(evmar),
    data = analysis_data |> dplyr::filter(country_code != cty))
  tibble::tibble(country_left_out = cty, gini_coef = coef(mod)[["gini"]])
}) |>
  dplyr::bind_rows()
readr::write_csv(leave_one_country_out, "output/tables/leave_one_country_out.csv")

p_loo <- ggplot2::ggplot(leave_one_country_out, ggplot2::aes(x = reorder(country_left_out, gini_coef), y = gini_coef)) +
  ggplot2::geom_col(fill = "gray40") +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Leave-one-country-out: gini coefficient stability", x = "Country left out", y = "Estimated gini coefficient") +
  ggplot2::theme_minimal()
ggplot2::ggsave("output/figures/leave_one_country_out_gini_coef.png", p_loo, width = 9, height = 8)

m9_income_interaction <- stats::lm(happy ~ gini * hinctnta + unemployment_rate + agea + factor(evmar), data = analysis_data)
m10_age_interaction <- stats::lm(happy ~ gini * agea + unemployment_rate + hinctnta + factor(evmar), data = analysis_data)

model_results <- dplyr::bind_rows(
  tidy_lm(m1, "M1_bivariate"),
  tidy_lm(m2, "M2_macro_control"),
  tidy_lm(m3, "M3_micro_macro_controls"),
  tidy_lm(m4_fe, "M4_country_year_FE"),
  tidy_lm(m5_log, "M5_log_gini"),
  tidy_lm(m6_quad, "M6_quadratic_gini"),
  tidy_lm(m7_lag, "M7_lagged_macro"),
  tidy_lm(m8_balanced, "M8_balanced_sample"),
  tidy_lm(m9_income_interaction, "M9_income_interaction"),
  tidy_lm(m10_age_interaction, "M10_age_interaction")
)
readr::write_csv(model_results, "output/tables/model_results.csv")

analysis_summary <- tibble::tribble(
  ~stage, ~description,
  "A", "Descriptive diagnostics produced (missingness, distributions, country-year summaries)",
  "B", "Bivariate inequality-happiness relationship estimated",
  "C", "Multivariable models estimated with macro and micro controls",
  "D", "Country/year fixed-effects model estimated for within-country temporal association",
  "Robustness", "Log, nonlinear, lagged, balanced-panel, leave-one-country-out, and heterogeneity checks estimated"
)
readr::write_csv(analysis_summary, "output/tables/analysis_stages_summary.csv")

message("Analysis outputs written to output/figures and output/tables")
