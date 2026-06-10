#!/usr/bin/env Rscript

# Analysis outline script.
# Includes descriptive visualisations that can be re-used in Quarto presentations.

  input_path <- "data/processed/analysis_dataset.csv"

  if (!file.exists(input_path)) {
    stop("Run scripts/03_merge_micro_macro.R first.")
  }

  data <- readr::read_csv(input_path, show_col_types = FALSE)

  # Descriptive summary by country-year.
  descriptive <- data |>
    group_by(country_code, year) |>
    summarise(
      mean_happiness = mean(happiness, na.rm = TRUE),
      mean_gini = mean(gini, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

  p1 <- ggplot(descriptive, aes(x = mean_gini, y = mean_happiness, color = country_code)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(
      title = "Happiness and income inequality across Europe",
      x = "Gini coefficient",
      y = "Mean happiness"
    ) +
    theme_minimal()

  ggsave("output/figures/happiness_vs_gini.png", p1, width = 9, height = 6)
