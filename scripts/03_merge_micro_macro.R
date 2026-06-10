#!/usr/bin/env Rscript

# -----------------------------------------------------------------------------
# Data Quality Check: EUROSTAT
# -----------------------------------------------------------------------------
macro = readr::read_csv(macro_path, show_col_types = FALSE) %>% 
  mutate(country_code = toupper(country_code)) %>% 
  filter(country_code %in% european_iso2)

# Missings
miss_var_summary(macro %>% group_by(year)) %>% data.frame()

write_csv(macro, "data/macro.csv")

# -----------------------------------------------------------------------------
# Data Quality Check: ESS
# -----------------------------------------------------------------------------
ess = readr::read_csv(micro_path, show_col_types = FALSE) 

# Missings
miss_var_summary(ess %>% group_by(essround)) %>% data.frame()

# Missing Values laut Codebook: 77, 88, 99 für die meisten Variablen
sapply(ess, max, na.rm = TRUE)

# Recode Missings:
micro = ess %>% 
  mutate(across(c(happy, hinctnta), ~ ifelse(. >10, NA, .)),
         agea = ifelse(agea %in% 999, NA, agea),
         evmar = case_when(evmar > 2 ~ NA,
                           evmar == 2 ~ 1),
         year = lubridate::year(dmy(proddate))
  ) %>% 
  #rename(country_code = cntry) %>% 
  mutate(country_code = toupper(country_code)) |>
  filter(country_code %in% european_iso2) 

write_csv(micro, "data/micro.csv")


# -----------------------------------------------------------------------------
# Merge ESS micro data and Eurostat macro data on country and year.
# -----------------------------------------------------------------------------

df <- micro |>
  left_join(macro, by = c("country_code", "year"))

write_csv(df, "data/analysis_data.csv")

