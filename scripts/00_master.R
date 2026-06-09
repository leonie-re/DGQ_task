#!/usr/bin/env Rscript

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
