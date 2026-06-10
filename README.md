# DGQ_task

Project outline for collecting Eurostat (SDMX 3.0) macro data and ESS micro data,
merging by European country codes, and producing descriptive analyses in R and Quarto.

## Scripts

- `scripts/01_collect_eurostat_macro.R`
- `scripts/02_collect_ess_micro.R`
- `scripts/03_merge_micro_macro.R`
- `scripts/04_analysis_outline.R`
- `scripts/00_master.R`

## Run pipeline

```r
Sys.setenv(RUN_API_CALLS = "true") # set to "false" for local dry-run templates
source("scripts/00_master.R")
```

## Quarto presentation

- `presentation.qmd`
- Render with: `quarto render presentation.qmd`
