#!/usr/bin/env Rscript
# =============================================================================
# eurostat_macro.R
# Ruft makroökonomische Indikatoren über die Eurostat Statistics API (JSON-stat) ab und speichert das Ergebnis unter data/raw/eurostat_macro.csv.
# =============================================================================
# -----------------------------------------------------------------------------
# Hilfsfunktion: JSON-stat-Antwort der Eurostat API in einen langen data.frame
# umwandeln.
# Die Eurostat Statistics API gibt JSON-stat 2.0 zurück. Der Kern ist:
#   $id      – Reihenfolge der Dimensionen (z. B. c("freq","unit","geo","time"))
#   $size    – Anzahl der Kategorien je Dimension
#   $value   – Werte als benannter Vektor (Index = linearisierte Position)

# Aus id/size wird ein vollständiges kartesisches Gitter erzeugt; anschließend werden die $value-Einträge per Position zugeordnet.
# -----------------------------------------------------------------------------
# Ich sehe mir zunächst die Struktur genauer um die Funktion richtig aufzubauen. 
meta_url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/ilc_di12?format=JSON&lang=EN&lastTimePeriod=1&geoLevel=aggregate"

meta_url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/une_rt_a?format=JSON&lang=EN&lastTimePeriod=1&geoLevel=aggregate"

resp <- request(meta_url) %>%
  req_error(is_error = \(r) FALSE) %>%
  req_perform()

cat("Status:", resp_status(resp), "\n")
js <- resp_body_json(resp, simplifyVector = TRUE)
cat("Dimensionen:", paste(js$id, collapse = ", "), "\n")

lapply(js$id, function(d) {
  cat("\n---", d, "---\n")
  print(names(js$dimension[[d]]$category$index))
})

# -----------------------------------------------------------------------------

parse_jsonstat <- function(js) {
  dims     <- js$id          # z. B. c("freq","age","statinfo","geo","time")
  dim_info <- js$dimension   # Liste mit Index & Label je Dimension
  
  # Kategorien-Codes je Dimension (in korrekter Reihenfolge).
  # $category$index kann je nach simplifyVector ein benannter Integer-Vektor
  # ODER eine Liste sein — names() funktioniert in beiden Fällen.
  cats <- lapply(dims, function(d) {
    idx <- dim_info[[d]]$category$index
    if (is.null(names(idx))) {
      # Fallback: Labels verwenden
      names(dim_info[[d]]$category$label)
    } else {
      names(idx)
    }
  })
  
  # Gesamtzahl der Zellen im Hyperwürfel
  n_cells <- prod(lengths(cats))
  if (n_cells == 0L) stop("parse_jsonstat: leeres Gitter — alle Dimensionen prüfen.")
  
  # Kartesisches Gitter (expand.grid arbeitet in umgekehrter Reihenfolge)
  grid <- do.call(expand.grid, c(rev(cats), stringsAsFactors = FALSE))
  grid <- grid[, rev(seq_along(grid)), drop = FALSE]
  names(grid) <- dims
  
  # Werte zuordnen: $value ist ein benannter Vektor, Namen = 0-basierter Index
  vals <- js$value
  grid$value <- NA_real_
  if (length(vals) > 0) {
    idx_numeric <- as.integer(names(vals))   # 0-basiert → +1 für R
    grid$value[idx_numeric + 1L] <- unlist(vals)
  }
  
  grid
}


# -----------------------------------------------------------------------------
# Kernfunktion: Einen Eurostat-Datensatz herunterladen und als data.frame zurückgeben.
# Parameter:
#   dataset_id   – Eurostat-Datensatz-Code (z. B. "ilc_di12")
#   extra_params – benannter character-Vektor zusätzlicher Query-Parameter
#                  (z. B. c(age = "TOTAL", unit = "PC"))
# -----------------------------------------------------------------------------
fetch_eurostat <- function(dataset_id, extra_params = character()) {
  
  base_url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
    dataset_id
  )
  
  # Standardfilter
  geo_codes <- c(
    "EU27_2020", "EA20",
    "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT",
    "CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI",
    "SK","FI","SE","IS","NO","CH","UK","ME","MK","AL","RS","TR","XK"
  )
  
  # httr2-Request aufbauen
  req <- request(base_url) %>%
    req_url_query(
      format          = "JSON",
      lang            = "EN",
      sinceTimePeriod = "2014",
      # Mehrere Werte für denselben Parameter: als Liste übergeben
      .multi = "explode"
    ) %>%
    # geo-Parameter einzeln anhängen
    req_url_query(!!!setNames(as.list(geo_codes), rep("geo", length(geo_codes))),
                  .multi = "explode")
  
  # Zusätzliche benutzerdefinierte Filter anhängen
  if (length(extra_params) > 0) {
    req <- req %>%
      req_url_query(!!!as.list(extra_params), .multi = "explode")
  }
  
  message("Abruf: ", dataset_id, " ...")
  
  resp <- req %>%
    req_error(is_error = \(r) FALSE) %>%   # Fehler manuell prüfen
    req_perform()
  
  if (resp_status(resp) != 200L) {
    warning("HTTP ", resp_status(resp), " für Datensatz '", dataset_id,
            "': ", resp_body_string(resp))
    return(tibble())
  }
  
  js  <- resp_body_json(resp, simplifyVector = TRUE)
  df  <- parse_jsonstat(js)
  
  # Einheitliche Spaltennamen: "time" → "year", Kleinschreibung
  names(df) <- tolower(names(df))
  if ("time" %in% names(df)) df <- rename(df, year = time)
  if ("geo"  %in% names(df)) df <- rename(df, country_code = geo)
  
  df
}

# -----------------------------------------------------------------------------
# main(): Daten abrufen, zusammenführen, speichern
# -----------------------------------------------------------------------------
eurostat_load <- function() {
  
  output_path <- file.path("data", "raw", "eurostat_macro.csv")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Umgebungsvariable schützt vor versehentlichen API-Aufrufen (z. B. in Tests)
  if (!identical(tolower(Sys.getenv("RUN_API_CALLS", "false")), "true")) {
    message("RUN_API_CALLS ist nicht 'aktiv'. Schreibe leere Vorlage nach ", output_path)
    write_csv(
      tibble(country_code = character(), year = character(),
             gini = double(), unemployment_rate = double()),
      output_path
    )
    return(invisible(NULL))
  }
  
  # ------------------------------------------------------------------
  # Indikator 1: Gini-Koeffizient (ilc_di12)
  #   Relevante Dimensionen: geo, time, indic_il (GINI = Gini-Koeffizient)
  # ------------------------------------------------------------------
  df_gini_raw <- fetch_eurostat(
    dataset_id   = "ilc_di12",
    extra_params = c(age = "TOTAL", statinfo = "GINI_HND")
  )
  
  
  df_gini <- df_gini_raw %>%
    filter(!is.na(value)) %>%
    select(country_code, year, gini = value)
  
  # ------------------------------------------------------------------
  # Indikator 2: Arbeitslosenquote (une_rt_a)
  #   Relevante Dimensionen: geo, time, age=TOTAL, sex=T, unit=PC_ACT
  # ------------------------------------------------------------------
  df_unemp_raw <- fetch_eurostat(
    dataset_id   = "une_rt_a",
    extra_params = c(age = "Y15-74", sex = "T", unit = "PC_ACT")
  )
  
  
  df_unemp <- df_unemp_raw %>%
    filter(!is.na(value)) %>%
    select(country_code, year, unemployment_rate = value)
  
  # ------------------------------------------------------------------
  # Zusammenführen & speichern
  # ------------------------------------------------------------------
  df_macro <- full_join(df_gini, df_unemp, by = c("country_code", "year")) %>%
    arrange(country_code, year)
  
  write_csv(df_macro, output_path)
  message("Gespeichert: ", output_path, " (", nrow(df_macro), " Zeilen)")
  
  invisible(df_macro)
}

# Skript direkt ausführbar (nicht nur als Quelle nutzbar)
  eurostat_load()

  