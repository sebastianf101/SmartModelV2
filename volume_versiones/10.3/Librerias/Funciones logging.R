# Funciones logging.R
# Sistema de logging centralizado para SmartModel
# Implementa log levels estándar, rotación de archivos y contexto de sesión
# Usa R6 para manejo limpio de estado mutable

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Log levels
LOG_LEVELS <- list(
  TRACE = 0,
  DEBUG = 1,
  INFO = 2,
  WARN = 3,
  ERROR = 4,
  FATAL = 5
)

# Clase R6 para el estado del logger
BsmLogger <- R6::R6Class(

"BsmLogger",
public = list(
  initialized = FALSE,
  session_id = NULL,
  session_start = NULL,
  is_child_process = FALSE,

  log_dir = NULL,
  log_file = NULL,
  log_to_console = TRUE,
  current_notebook = NULL,
  is_batch = FALSE,
  min_level_file = LOG_LEVELS$DEBUG,
  min_level_console = LOG_LEVELS$INFO,

  initialize = function() {
    self$initialized <- FALSE
    # Check if we're a child process with inherited session
    self$is_child_process <- nzchar(Sys.getenv("BSM_SESSION_ID", ""))
  },

  init = function(log_dir = NULL,
                  session_id = NULL,
                  is_batch = !interactive(),
                  min_level_console = "INFO",
                  min_level_file = "DEBUG") {

    # Check for inherited session from parent process
    inherited_session <- Sys.getenv("BSM_SESSION_ID", "")
    inherited_log_dir <- Sys.getenv("BSM_LOG_DIR", "")

    # Determinar directorio de logs
    if (is.null(log_dir)) {
      if (nzchar(inherited_log_dir)) {
        log_dir <- inherited_log_dir
      } else {
        log_dir <- fs::path(Sys.getenv("HOME"), "Documents/besmart/10.3/Logs")
      }
    }

    # Crear directorio si no existe
    if (!fs::dir_exists(log_dir)) {
      fs::dir_create(log_dir, recurse = TRUE)
    }

    # Use inherited session ID if available, otherwise generate new one
    if (is.null(session_id) && nzchar(inherited_session)) {
      session_id <- inherited_session
      self$is_child_process <- TRUE
    } else if (is.null(session_id)) {
      session_id <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3") |>
        stringr::str_replace_all("\\.", "") |>
        paste0("_", stringr::str_sub(digest::digest(Sys.time()), 1, 6))
      # Export session ID for child processes
      Sys.setenv(BSM_SESSION_ID = session_id)
      Sys.setenv(BSM_LOG_DIR = log_dir)
    }

    # Configurar archivo de log
    log_file <- fs::path(log_dir, paste0("smartmodel_", format(Sys.Date(), "%Y%m%d"), ".log"))

    # Rotar archivo si supera 10MB (only for parent process)
    if (!self$is_child_process && fs::file_exists(log_file)) {
      file_size_mb <- fs::file_size(log_file) / 1024^2
      if (file_size_mb > 10) {
        self$rotate_log_file(log_file)
      }
    }

    # Guardar estado
    self$initialized <- TRUE
    self$session_id <- session_id
    self$session_start <- Sys.time()
    self$log_dir <- log_dir
    self$log_file <- log_file
    self$is_batch <- is_batch
    self$log_to_console <- !is_batch
    self$min_level_file <- LOG_LEVELS[[toupper(min_level_file)]]
    self$min_level_console <- LOG_LEVELS[[toupper(min_level_console)]]

    # Log inicial only for new sessions (not inherited)
    if (!self$is_child_process) {
      self$write_direct(
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
    }

    invisible(session_id)
  },

  set_notebook = function(notebook_name) {
    if (!self$initialized) self$init()
    self$current_notebook <- notebook_name
    self$info(glue::glue("Iniciando cuaderno: {notebook_name}"))
  },

  clear_notebook = function() {
    if (self$initialized && !is.null(self$current_notebook)) {
      notebook <- self$current_notebook
      self$current_notebook <- NULL
      self$info(glue::glue("Finalizando cuaderno: {notebook}"))
    }
  },

  rotate_log_file = function(log_file) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_name <- fs::path_ext_remove(log_file)
    ext <- fs::path_ext(log_file)
    rotated_file <- paste0(base_name, "_", timestamp, ".", ext)

    tryCatch({
      fs::file_move(log_file, rotated_file)
      if (requireNamespace("zip", quietly = TRUE)) {
        zip_file <- paste0(rotated_file, ".zip")
        zip::zip(zip_file, rotated_file, mode = "cherry-pick")
        fs::file_delete(rotated_file)
      }
    }, error = function(e) {
      warning("Error al rotar archivo de log: ", e$message)
    })
  },

  cleanup_old = function(log_dir = NULL, days = 7) {
    if (is.null(log_dir)) {
      if (!self$initialized) return(invisible(NULL))
      log_dir <- self$log_dir
    }

    if (!fs::dir_exists(log_dir)) return(invisible(NULL))

    cutoff_date <- Sys.Date() - days
    log_files <- fs::dir_ls(log_dir, regexp = "smartmodel_.*\\.(log|zip)$")

    for (file in log_files) {
      file_date <- fs::file_info(file)$modification_time |> as.Date()
      if (file_date < cutoff_date) {
        tryCatch({
          fs::file_delete(file)
          self$debug(glue::glue("Archivo de log antiguo eliminado: {file}"))
        }, error = function(e) {
          self$warn(glue::glue("Error al eliminar archivo antiguo {file}: {e$message}"))
        })
      }
    }
    invisible(NULL)
  },

  write_direct = function(level, message, code = NULL, skip_check = FALSE) {
    if (!skip_check && !self$initialized) {
      self$init()
    }

    level_num <- LOG_LEVELS[[level]]
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
    session_id <- self$session_id %||% "UNINIT"
    notebook <- self$current_notebook %||% "-"

    log_line <- sprintf(
      "[%s] [%s] [%s] [%s] %s%s\n",
      timestamp, level, session_id, notebook, message,
      if (!is.null(code)) paste0(" (Cod: ", code, ")") else ""
    )

    if (level_num >= self$min_level_file) {
      tryCatch({
        cat(log_line, file = self$log_file, append = TRUE)
      }, error = function(e) {
        warning("Error escribiendo al archivo de log: ", e$message, call. = FALSE)
      })
    }

    if (self$log_to_console && level_num >= self$min_level_console) {
      self$show_in_console(level, message, code)
    }

    invisible(NULL)
  },

  show_in_console = function(level, message, code = NULL) {
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
  },

  # Logging methods
  # Note: caller_env parameter allows wrapper functions to pass the correct environment
  trace = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("TRACE", message, code)
  },

  debug = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("DEBUG", message, code)
  },

  info = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("INFO", message, code)
  },

  warn = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("WARN", message, code)
  },

  error = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("ERROR", message, code)
  },

  fatal = function(..., code = NULL, caller_env = parent.frame()) {
    if (!self$initialized) self$init()
    message <- glue::glue(..., .envir = caller_env)
    self$write_direct("FATAL", message, code)
  },

  catch = function(expr, context = "", code = NULL) {
    tryCatch(
      expr,
      error = function(e) {
        msg <- if (context != "") paste0(context, ": ", e$message) else e$message
        self$error(msg, code = code)
        stop(e)
      },
      warning = function(w) {
        msg <- if (context != "") paste0(context, ": ", w$message) else w$message
        self$warn(msg, code = code)
        warning(w)
      }
    )
  },

  finalize_session = function() {
    if (self$initialized && !self$is_child_process) {
      session_id <- self$session_id
      self$info("=== SmartModel Session Ended ===")
      self$info(glue::glue("Session ID: {session_id}"))
      self$cleanup_old()
    }
    invisible(NULL)
  }
)
)

# Instancia global del logger (preservar si ya existe para evitar re-inicialización)
if (!exists("bsm_logger") || !inherits(bsm_logger, "BsmLogger")) {
  bsm_logger <- BsmLogger$new()
}

# Funciones wrapper para compatibilidad con código existente
log_init <- function(log_dir = NULL, session_id = NULL, is_batch = !interactive(),
                     min_level_console = "INFO", min_level_file = "DEBUG") {
  bsm_logger$init(log_dir, session_id, is_batch, min_level_console, min_level_file)
}

log_get_session_id <- function() {
  if (!bsm_logger$initialized) bsm_logger$init()
  bsm_logger$session_id
}

log_set_notebook <- function(notebook_name) bsm_logger$set_notebook(notebook_name)
log_clear_notebook <- function() bsm_logger$clear_notebook()
log_cleanup_old <- function(log_dir = NULL, days = 7) bsm_logger$cleanup_old(log_dir, days)
log_finalize <- function() bsm_logger$finalize_session()

log_trace <- function(..., code = NULL) bsm_logger$trace(..., code = code, caller_env = parent.frame())
log_debug <- function(..., code = NULL) bsm_logger$debug(..., code = code, caller_env = parent.frame())
log_info <- function(..., code = NULL) bsm_logger$info(..., code = code, caller_env = parent.frame())
log_warn <- function(..., code = NULL) bsm_logger$warn(..., code = code, caller_env = parent.frame())
log_error <- function(..., code = NULL) bsm_logger$error(..., code = code, caller_env = parent.frame())
log_fatal <- function(..., code = NULL) bsm_logger$fatal(..., code = code, caller_env = parent.frame())
log_catch <- function(expr, context = "", code = NULL) bsm_logger$catch(expr, context, code)
