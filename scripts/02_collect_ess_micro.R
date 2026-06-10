#!/usr/bin/env Rscript
# =============================================================================
# ess_micro.R
# Ruft Mikrodaten aus dem European Social Survey (ESS) über die Sikt-API ab.
# Gespeichert unter data/raw/ess_micro.csv.
# =============================================================================
ESS_VARS <- c(
  "idno",     # Respondenten-ID
  "cntry",    # Ländercode
  "essround", # ESS-Runde (für den späteren Merge mit Makrodaten)
  "proddate",
  "agea",     # Alter
  "happy",    # Glück (0–10)
  #"evmar",  # Familienstand
  "hinctnta" # Haushaltseinkommen (Dezile)
)


url <- "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess6e02_6?userId=62ac71b7-13fc-458d-b147-34543a42669b&fileFormat=csv"
df_micro <- read.csv(url)

output_data = "data/raw/ess_micro.csv"


# Fetch einzelner Rounds ------------
fetch_ess_round <- function(doi, user_id) {
  
  url <- paste0(
    "https://api.ess.sikt.no/v1/data/dataFile/",
    doi
  )
  
  message("Abruf ESS DOI: ", doi, " ...")
  
  resp <- request(url) %>%
    req_url_query(
      userId     = user_id,
      fileFormat = "csv"
    ) %>%
    req_timeout(120) %>%                        # große Dateien brauchen Zeit
    req_error(is_error = \(r) FALSE) %>%
    req_perform()
  
  if (resp_status(resp) != 200L) {
    warning("HTTP ", resp_status(resp), " für DOI '", doi,
            "': ", resp_body_string(resp))
    return(tibble())
  }
  
  # CSV direkt aus dem Response-Body einlesen
  raw_csv <- resp_body_string(resp)
  df      <- read_csv(I(raw_csv), show_col_types = FALSE, na = c("", "NA"))
  
  # Nur gewünschte Spalten behalten; fehlende mit NA auffüllen
  missing_vars <- setdiff(ESS_VARS, names(df))
  if (length(missing_vars) > 0) {
    warning("Fehlende Variablen in ", doi, ": ",
            paste(missing_vars, collapse = ", "), " — werden als NA gesetzt.")
  }
  for (v in missing_vars) df[[v]] <- NA
  
  df %>% select(all_of(ESS_VARS))
}

# -----------------------------------------------------------------------------
# Alle konfigurierten Runden abrufen, zusammenführen, speichern
# -----------------------------------------------------------------------------
ess_load <- function() {
  
  output_path <- file.path("data", "raw", "ess_micro.csv")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS ist nicht 'aktiv'. Schreibe leere Vorlage nach ", output_path)
    write_csv(
      tibble(idno     = integer(),
             cntry    = character(),
             essround = integer(),
             proddate = date(),
             agea     = integer(),
             happy    = integer(),
             ev_mar  = integer(),
             hinctnta = integer()),
      output_path
    )
    return(invisible(NULL))
  }
  
  user_id <- Sys.getenv("ESS_USER_ID", "62ac71b7-13fc-458d-b147-34543a42669b")
  
  # Zu ladende Runden: DOI → Runden-Nummer
  # Entferne Runden, die du nicht benötigst, oder ergänze weitere.
  rounds <- list(
    list(doi = "10.21338/ess6e02_6",   round = 6),
    list(doi = "10.21338/ess7e02_2",   round = 7),
    list(doi = "10.21338/ess8e02_3",   round = 8),
    list(doi = "10.21338/ess9e03_3",   round = 9),
    list(doi = "10.21338/ess10e03_3",  round = 10),
    list(doi = "10.21338/ess11e04_1", round = 11)
  )
  
  # Parallele Downloads:
  plan(multisession, workers = 3) 
  
  dfs <- future_lapply(rounds, function(r) {
    fetch_ess_round(doi = r$doi, user_id = user_id)
  })
  # Zurücksetzen
  on.exit(plan(sequential))
  
  # Leere Ergebnisse entfernen und zusammenführen
  dfs       <- Filter(Negate(is.null), dfs)
  ess_micro <- bind_rows(dfs) %>%
    arrange(cntry, essround, idno) 
  
  message("Zeilen gesamt: ", nrow(ess_micro),
          " | Länder: ", n_distinct(ess_micro$cntry),
          " | Runden: ", n_distinct(ess_micro$essround))
  
  write_csv(ess_micro, output_path)
  message("Gespeichert: ", output_path)
  
  invisible(ess_micro) 
}

ess_load()
