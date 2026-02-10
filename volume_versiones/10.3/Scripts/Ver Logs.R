# Script para ver y analizar logs de SmartModel
# Uso: source("Scripts/Ver Logs.R")

library(dplyr)
library(readr)
library(stringr)

# Configuración
log_dir <- Sys.getenv("BSM_LOG_DIR", "")
if (log_dir == "") {
  log_dir <- fs::path(Sys.getenv("HOME"), "Documents/besmart/10.3/Logs")
}

if (!fs::dir_exists(log_dir)) {
  stop("El directorio de logs no existe: ", log_dir)
}

# Función para leer el log más reciente
read_latest_log <- function(n_lines = 100) {
  log_files <- fs::dir_ls(log_dir, regexp = "smartmodel_.*\\.log$")

  if (length(log_files) == 0) {
    message("No se encontraron archivos de log")
    return(invisible(NULL))
  }

  # Ordenar por fecha de modificación
  log_files <- log_files[order(fs::file_info(log_files)$modification_time, decreasing = TRUE)]
  latest_log <- log_files[1]

  message("Leyendo: ", latest_log)

  # Leer últimas n líneas
  lines <- readr::read_lines(latest_log)
  n_total <- length(lines)

  if (n_total > n_lines) {
    lines <- tail(lines, n_lines)
    message("Mostrando últimas ", n_lines, " de ", n_total, " líneas")
  } else {
    message("Mostrando todas las ", n_total, " líneas")
  }

  cat(paste(lines, collapse = "\n"), "\n")
  invisible(lines)
}

# Función para parsear y analizar logs
parse_log_file <- function(log_file = NULL) {
  if (is.null(log_file)) {
    log_files <- fs::dir_ls(log_dir, regexp = "smartmodel_.*\\.log$")
    if (length(log_files) == 0) return(tibble())
    log_file <- log_files[order(fs::file_info(log_files)$modification_time, decreasing = TRUE)][1]
  }

  lines <- readr::read_lines(log_file)

  # Parsear líneas con regex
  parsed <- tibble(raw = lines) |>
    filter(str_detect(raw, "^\\[")) |>
    mutate(
      timestamp = str_extract(raw, "(?<=\\[)[^\\]]+(?=\\])"),
      level = str_extract(raw, "(?<=\\] \\[)[^\\]]+(?=\\])"),
      session_id = str_extract(raw, "(?<=\\] \\[)[a-zA-Z0-9_]+(?=\\] \\[)"),
      notebook = str_extract(raw, "(?<=\\] \\[)[^\\]]+(?=\\] [^\\[])"),
      message = str_extract(raw, "(?<=\\] ).*$"),
      code = str_extract(message, "(?<=Cod: )[0-9]+")
    ) |>
    mutate(
      timestamp = lubridate::ymd_hms(timestamp),
      level = factor(level, levels = c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"))
    ) |>
    select(-raw)

  return(parsed)
}

# Función para resumen de logs
log_summary <- function(log_file = NULL) {
  df <- parse_log_file(log_file)

  if (nrow(df) == 0) {
    message("No hay logs para analizar")
    return(invisible(NULL))
  }

  cat("\n=== RESUMEN DE LOGS ===\n\n")

  # Rango temporal
  cat("Período:", format(min(df$timestamp, na.rm = TRUE)), "a",
      format(max(df$timestamp, na.rm = TRUE)), "\n\n")

  # Por nivel
  cat("Distribución por Nivel:\n")
  df |>
    count(level) |>
    arrange(level) |>
    print()

  cat("\n")

  # Por session
  cat("Sessions:\n")
  df |>
    filter(!is.na(session_id)) |>
    distinct(session_id) |>
    print()

  cat("\n")

  # Por notebook
  cat("Notebooks ejecutados:\n")
  df |>
    filter(!is.na(notebook), notebook != "-") |>
    count(notebook, sort = TRUE) |>
    print()

  cat("\n")

  # Errores con código
  errors <- df |>
    filter(level %in% c("ERROR", "FATAL")) |>
    select(timestamp, level, notebook, message, code)

  if (nrow(errors) > 0) {
    cat("ERRORES ENCONTRADOS:\n")
    print(errors, n = Inf)
  } else {
    cat("No se encontraron errores\n")
  }

  invisible(df)
}

# Función para filtrar logs
filter_logs <- function(session_id = NULL, notebook = NULL, level = NULL,
                       pattern = NULL, log_file = NULL) {
  df <- parse_log_file(log_file)

  if (!is.null(session_id)) {
    df <- df |> filter(session_id == !!session_id)
  }

  if (!is.null(notebook)) {
    df <- df |> filter(notebook == !!notebook)
  }

  if (!is.null(level)) {
    level_num <- which(levels(df$level) == toupper(level))
    df <- df |> filter(as.numeric(level) >= level_num)
  }

  if (!is.null(pattern)) {
    df <- df |> filter(str_detect(message, !!pattern))
  }

  return(df)
}

# Menú interactivo
cat("\n=== Visor de Logs SmartModel ===\n\n")
cat("Funciones disponibles:\n")
cat("  read_latest_log(n = 100)  - Ver últimas n líneas\n")
cat("  log_summary()              - Resumen de logs\n")
cat("  parse_log_file()           - Parsear log a tibble\n")
cat("  filter_logs(...)           - Filtrar logs por criterios\n\n")

cat("Ejemplos:\n")
cat("  read_latest_log(50)\n")
cat("  log_summary()\n")
cat("  filter_logs(level = 'ERROR')\n")
cat("  filter_logs(notebook = 'Modelling.qmd')\n")
cat("  filter_logs(pattern = 'variable')\n\n")

# Auto-ejecutar resumen si es interactivo
if (interactive()) {
  cat("Generando resumen automático...\n\n")
  log_summary()
}
