# DGQ_task

Pipeline for collecting Eurostat macro data and ESS micro data, merging by European country-year, and estimating observational inequality-happiness associations in R + Quarto.

## Scripts

- `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/scripts/01_collect_eurostat_macro.R`
- `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/scripts/02_collect_ess_micro.R`
- `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/scripts/03_merge_micro_macro.R`
- `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/scripts/04_analysis_outline.R`
- `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/scripts/00_master.R`

## Analytical scope

- Objective: observational association (non-causal)
- Outcome: `happy`
- Exposure: `gini`
- Controls: `unemployment_rate`, `agea`, `hinctnta`, `evmar`
- Population: European countries, ESS rounds 7-11 (survey years 2014+)
- Missing-data policy: full sample for descriptives, complete-case sample for models

## Canonical datasets and outputs

- Canonical analysis data: `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/data/processed/analysis_dataset.csv`
- Diagnostics: `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/data/processed/analysis_diagnostics.csv`
- Scope metadata: `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/data/processed/analysis_scope.csv`
- Figures: `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/output/figures/`
- Tables: `/home/runner/work/DGQ_task/DGQ_task/leonie-re/DGQ_task/output/tables/`

## Run pipeline

```r
Sys.setenv(RUN_API_CALLS = "true")
source("scripts/00_master.R")
```

Set `RUN_API_CALLS = "false"` for dry-run template outputs.

## Render presentation

```bash
quarto render presentation.qmd
```

## Validation / acceptance criteria

- Master pipeline runs end-to-end and writes all expected output files.
- Canonical path handoff is consistent (`data/processed/analysis_dataset.csv`).
- Diagnostics include sample sizes, merge coverage, and missingness rates.
- Model results are reproducible from generated pipeline outputs.
