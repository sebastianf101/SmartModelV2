# Funciones logging.R
# Sistema de logging centralizado para SmartModel
# Implementa log levels estándar, rotación de archivos y contexto de sesión

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Global logging state
.bsm_log_state <- new.env(parent = emptyenv())
.bsm_log_state$initialized <- FALSE
.bsm_log_state$session_id <- NULL
.bsm_log_state$log_dir <- NULL
.bsm_log_state$log_file <- NULL
.bsm_log_state$log_to_console <- TRUE
.bsm_log_state$current_notebook <- NULL
.bsm_log_state$is_batch <- FALSE

# Log levels
LOG_LEVELS <- list(
  TRACE = 0,
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4,
  FATAL = 5
)

.bsm_log_state$min_level_file <- LOG_LEVELS$DEBUG
.bsm_log_state$min_level_console <- LOG_LEVELS$INFO

# Inicializa el sistema de logging
log_init <- function(log_dir = NULL,
                     session_id = NULL,
                     is_batch = !interactive(),
                     min_level_console = "INFO",
                     min_level_file = "DEBUG") {

  # Determinar directorio de logs
  if (is.null(log_dir)) {
    log_dir <- Sys.getenv("BSM_LOG_DIR", "")
    if (log_dir == "") {
      log_dir <- fs::path(Sys.getenv("HOME"), "Documents/besmart/10.3/Logs")
    }
  }

  # Crear directorio si no existe
  if (!fs::dir_exists(log_dir)) {
    fs::dir_create(log_dir, recurse = TRUE)
  }

  # Generar session ID si no se proporciona
  if (is.null(session_id)) {
    session_id <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3") |>
      stringr::str_replace_all("\\.", "") |>
      paste0("_", stringr::str_sub(digest::digest(Sys.time()), 1, 6))
  }

  # Configurar archivo de log
  log_file <- fs::path(log_dir, paste0("smartmodel_", format(Sys.Date(), "%Y%m%d"), ".log"))

  # Rotar archivo si supera 10MB
  if (fs::file_exists(log_file)) {
    file_size_mb <- fs::file_size(log_file) / 1024^2
    if (file_size_mb > 10) {
      rotate_log_file(log_file)
    }
  }

  # Guardar estado
  .bsm_log_state$initialized <- TRUE
  .bsm_log_state$session_id <- session_id
  .bsm_log_state$log_dir <- log_dir
  .bsm_log_state$log_file <- log_file
  .bsm_log_state$is_batch <- is_batch
  .bsm_log_state$log_to_console <- !is_batch  # En batch no mostrar en consola
  .bsm_log_state$min_level_file <- LOG_LEVELS[[toupper(min_level_file)]]
  .bsm_log_state$min_level_console <- LOG_LEVELS[[toupper(min_level_console)]]

  # Log inicial
  log_write_direct(
    level = "INFO",
    message = paste0(
      "=== SmartModel Session Started ===\n",
      "  Session ID: ", session_id, "\n",
      "  Mode: ", if(is_batch) "Batch" else "Interactive", "\n",
      "  Log File: ", log_file, "\n",
      "  R Version: ", R.version.string
    ),
    skip_check = TRUE
  )

  invisible(session_id)
}

# Obtener session ID actual
log_get_session_id <- function() {
  if (!.bsm_log_state$initialized) {
    log_init()
  }
  .bsm_log_state$session_id
}

# Establecer notebook actual
log_set_notebook <- function(notebook_name) {
  if (!.bsm_log_state$initialized) {
    log_init()
  }
  .bsm_log_state$current_notebook <- notebook_name
  log_info(glue::glue("Iniciando cuaderno: {notebook_name}"))
}

# Limpiar notebook actual
log_clear_notebook <- function() {
  if (.bsm_log_state$initialized && !is.null(.bsm_log_state$current_notebook)) {
    notebook <- .bsm_log_state$current_notebook
    .bsm_log_state$current_notebook <- NULL
    log_info(glue::glue("Finalizando cuaderno: {notebook}"))
  }
}

# Rotar archivo de log
rotate_log_file <- function(log_file) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  base_name <- fs::path_ext_remove(log_file)
  ext <- fs::path_ext(log_file)
  rotated_file <- paste0(base_name, "_", timestamp, ".", ext)

  tryCatch({
    fs::file_move(log_file, rotated_file)

    # Comprimir archivo rotado
    if (requireNamespace("zip", quietly = TRUE)) {
      zip_file <- paste0(rotated_file, ".zip")
      zip::zip(zip_file, rotated_file, mode = "cherry-pick")
      fs::file_delete(rotated_file)
    }
  }, error = function(e) {
    warning("Error al rotar archivo de log: ", e$message)
  })
}

# Limpiar logs antiguos (más de 7 días)
log_cleanup_old <- function(log_dir = NULL, days = 7) {
  if (is.null(log_dir)) {
    if (!.bsm_log_state$initialized) return(invisible(NULL))
    log_dir <- .bsm_log_state$log_dir
  }

  if (!fs::dir_exists(log_dir)) return(invisible(NULL))

  cutoff_date <- Sys.Date() - days
  log_files <- fs::dir_ls(log_dir, regexp = "smartmodel_.*\\.(log|zip)$")

  for (file in log_files) {
    file_date <- fs::file_info(file)$modification_time |> as.Date()
    if (file_date < cutoff_date) {
      tryCatch({
        fs::file_delete(file)
        log_debug(glue::glue("Archivo de log antiguo eliminado: {file}"))
      }, error = function(e) {
        log_warn(glue::glue("Error al eliminar archivo antiguo {file}: {e$message}"))
      })
    }
  }

  invisible(NULL)
}

# Función interna para escribir al log
log_write_direct <- function(level, message, code = NULL, skip_check = FALSE) {
  if (!skip_check && !.bsm_log_state$initialized) {
    log_init()
  }

  level_num <- LOG_LEVELS[[level]]

  # Formatear mensaje
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  session_id <- .bsm_log_state$session_id %||% "UNINIT"
  notebook <- .bsm_log_state$current_notebook %||% "-"

  # Construir línea de log
  log_line <- sprintf(
    "[%s] [%s] [%s] [%s] %s%s\n",
    timestamp,
    level,
    session_id,
    notebook,
    message,
    if (!is.null(code)) paste0(" (Cod: ", code, ")") else ""
  )

  # Escribir a archivo si el nivel es suficiente
  if (level_num >= .bsm_log_state$min_level_file) {
    tryCatch({
      cat(log_line, file = .bsm_log_state$log_file, append = TRUE)
    }, error = function(e) {
      warning("Error escribiendo al archivo de log: ", e$message, call. = FALSE)
    })
  }

  # Mostrar en consola si está habilitado y el nivel es suficiente
  if (.bsm_log_state$log_to_console && level_num >= .bsm_log_state$min_level_console) {
    show_in_console(level, message, code)
  }

  invisible(NULL)
}

# Mostrar mensaje en consola con formato cli
show_in_console <- function(level, message, code = NULL) {
  full_msg <- if (!is.null(code)) {
    paste0(message, " (Cod: ", code, ")")
  } else {
    message
  }

  switch(level,
    TRACE = cli::cli_alert(full_msg),
    DEBUG = cli::cli_alert_info(full_msg),
    INFO = cli::cli_alert_success(full_msg),
    WARN = cli::cli_alert_warning(full_msg),
    ERROR = cli::cli_alert_danger(full_msg),
    FATAL = cli::cli_alert_danger(full_msg),
    cli::cli_alert(full_msg)
  )
}

# Funciones públicas de logging con interpolación
log_trace <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("TRACE", message, code)
}

log_debug <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("DEBUG", message, code)
}

log_info <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("INFO", message, code)
}

log_warn <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("WARN", message, code)
}

log_error <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("ERROR", message, code)
}

log_fatal <- function(..., code = NULL) {
  if (!.bsm_log_state$initialized) log_init()
  message <- cli::format_inline(...)
  log_write_direct("FATAL", message, code)
}

# Wrapper para capturar errores y loguear
log_catch <- function(expr, context = "", code = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- if (context != "") {
        paste0(context, ": ", e$message)
      } else {
        e$message
      }
      log_error(msg, code = code)
      stop(e)
    },
    warning = function(w) {
      msg <- if (context != "") {
        paste0(context, ": ", w$message)
      } else {
        w$message
      }
      log_warn(msg, code = code)
      warning(w)
    }
  )
}

# Finalizar sesión de logging
log_finalize <- function() {
  if (.bsm_log_state$initialized) {
    elapsed <- difftime(Sys.time(),
                       .bsm_log_state$session_start %||% Sys.time(),
                       units = "secs")

    session_id <- .bsm_log_state$session_id

    log_info("=== SmartModel Session Ended ===")
    log_info(glue::glue("Session ID: {session_id}"))

    # Limpiar logs antiguos
    log_cleanup_old()
  }
  invisible(NULL)
}

# Registrar finalizador automático
reg.finalizer(.bsm_log_state, function(e) {
  if (.bsm_log_state$initialized) {
    log_finalize()
  }
}, onexit = TRUE)
