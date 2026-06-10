#!/usr/bin/env Rscript

input_path <- "data/analysis_data.csv"
if (!file.exists(input_path)) {
  stop("Run scripts/03_merge_micro_macro.R first.")
} else {
df <- readr::read_csv(input_path, show_col_types = FALSE)
}

required_cols <- c(
  "country_code", "year", "happy", "gini", "unemployment_rate",
  "agea", "hinctnta"
)

missing_cols <- setdiff(required_cols, names(data))

data <- df %>% 
  select(all_of(required_cols)) %>% 
  drop_na() %>%  
  mutate(log_gini = if_else(gini > 0, log(gini), NA_real_))


descriptive <- data %>%
  dplyr::group_by(country_code, year) %>%
  dplyr::summarise(
    mean_happy = mean(happy, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    mean_unemployment = mean(unemployment_rate, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

readr::write_csv(descriptive, "data/country_year_descriptives.csv")

p_scatter <- ggplot2::ggplot(descriptive, ggplot2::aes(x = mean_gini, y = mean_happy, color = country_code)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_smooth(method = "loess", se = F, color = "darkgreen", linetype = 1) +
  ggplot2::geom_smooth(method = "lm", se = F, color = "lightgreen", linetype = 1) +
  ggplot2::labs(
    title = "Country-year happiness and inequality",
    x = "Mean Gini",
    y = "Mean happiness"
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave("output/figures/happiness_vs_gini_country_year.png", p_scatter, width = 9, height = 6)

p_hist_happy <- ggplot2::ggplot(data, ggplot2::aes(x = happy)) +
  ggplot2::geom_histogram(bins = 11, fill = "darkgreen", color = "white") +
  ggplot2::labs(title = "Distribution of happiness", x = "Happiness", y = "Count") +
  ggplot2::theme_minimal()
ggplot2::ggsave("output/figures/happiness_distribution.png", p_hist_happy, width = 8, height = 5)

#------------------------------------------------------------------------------
# Inferential models: Country and Year Random intercept Models (Multi-level Models)
#------------------------------------------------------------------------------
m1 <- lmer(happy ~ gini + (1 | country_code) + (1 | year), data = data)
m2 <- lmer(happy ~ gini + unemployment_rate + (1 | country_code) + (1 | year), data = data)
m3 <- lmer(happy ~ gini + unemployment_rate + hinctnta + (1 | country_code) + (1 | year), data = data)

modelsummary(list(m1,m2,m3), stars = T)

# Pooled (Robustness)
m1_pooled <- lm_robust(happy ~ gini + factor(year), 
                       data = data, 
                       clusters = country_code)
m2_pooled <- lm_robust(happy ~ gini + unemployment_rate + factor(year), 
                       data = data, 
                       clusters = country_code)
m3_pooled <- lm_robust(happy ~ gini + unemployment_rate + hinctnta + factor(year), 
                       data = data, 
                       clusters = country_code)

modelsummary(list(m1_pooled,m2_pooled,m3_pooled), stars = T)

# Gini should be strictly positive for log transform; non-positive values should be dropped for the log specification.

m1 <- lmer(happy ~ log_gini + (1 | country_code) + (1 | year), data = data)
m2 <- lmer(happy ~ log_gini + unemployment_rate + (1 | country_code) + (1 | year), data = data)
m3 <- lmer(happy ~ log_gini + unemployment_rate + hinctnta + (1 | country_code) + (1 | year), data = data)

modelsummary(list(m1,m2,m3), stars = T)

# Pooled
m1_pooled <- lm_robust(happy ~ log_gini + factor(year), 
                       data = data, 
                       clusters = country_code)
m2_pooled <- lm_robust(happy ~ log_gini + unemployment_rate + factor(year), 
                       data = data, 
                       clusters = country_code)
m3_pooled <- lm_robust(happy ~ log_gini + unemployment_rate + hinctnta + factor(year), 
                       data = data, 
                       clusters = country_code)

# -------------------------------------------------------------------------
# 3. VISUALIZE EFFECT SIZES (COEFFICIENT PLOTS)
# -------------------------------------------------------------------------
# This creates a plot comparing the point estimates and confidence intervals.

# Compare the full models (Model 3 variants)
model_list <- list(
  "Multilevel (Linear)" = m3_lin,
  "Pooled OLS (Linear)" = m3_pooled_lin,
  "Multilevel (Log)"    = m3_log,
  "Pooled OLS (Log)"    = m3_pooled_log
)

# Plot coefficients (excluding intercepts and year fixed effects for clarity)
modelplot(model_list, coef_omit = "Intercept|factor\\(year\\)") +
  theme_minimal() +
  labs(title = "Effect Sizes on Happiness Across Specifications",
       x = "Coefficient Estimate (Effect Size)",
       y = "Predictor")


# -------------------------------------------------------------------------
# 4. PREDICTED VALUES (MARGINAL EFFECTS PLOTS)
# -------------------------------------------------------------------------
# Let's predict how happiness changes across the actual range of your data using your most complete multilevel models (m3_lin and m3_log).

# --- Effect of Gini (Linear vs Log) ---
plot_predictions(m3_lin, condition = "gini", re.form = NA) + 
  theme_minimal() + labs(title = "Predicted Happiness by Gini (Linear Model)", x = "Gini", y = "Predicted Happiness")

plot_predictions(m3_log, condition = "log_gini", re.form = NA) + 
  theme_minimal() + labs(title = "Predicted Happiness by Log Gini (Log Model)", x = "Log(Gini)", y = "Predicted Happiness")

# --- Effect of Unemployment Rate ---
plot_predictions(m3_lin, condition = "unemployment_rate", re.form = NA) + 
  theme_minimal() + labs(title = "Predicted Happiness by Unemployment Rate", x = "Unemployment Rate", y = "Predicted Happiness")

# --- Effect of Income (hinctnta) ---
plot_predictions(m3_lin, condition = "hinctnta", re.form = NA) + 
  theme_minimal() + labs(title = "Predicted Happiness by Income Level", x = "Household Income Scale", y = "Predicted Happiness")


# -------------------------------------------------------------------------
# 5. BONUS: EXTRACT EXACT NUMERICAL MARGINAL EFFECTS
# -------------------------------------------------------------------------
# If you want a clean table showing the average slope/effect size for each variable:
summary(avg_slopes(m3_lin, re.form = NA))
summary(avg_slopes(m3_log, re.form = NA))
