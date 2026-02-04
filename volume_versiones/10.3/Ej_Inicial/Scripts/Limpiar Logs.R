# Script para limpieza de logs antiguos
# Puede ejecutarse manualmente o como tarea programada

# Carga funciones de logging (from local bsm_path)
source(fs::path(bsm_path, "Librerias/Funciones logging.R"))

# Configuración
log_dir <- Sys.getenv("BSM_LOG_DIR", "")
if (log_dir == "") {
  log_dir <- fs::path(Sys.getenv("HOME"), "Documents/besmart/10.3/Logs")
}

# Asegurar que el directorio existe
if (!fs::dir_exists(log_dir)) {
  message("El directorio de logs no existe: ", log_dir)
  quit(status = 1)
}

message("Limpiando logs en: ", log_dir)

# Limpiar logs más antiguos que 7 días
log_cleanup_old(log_dir, days = 7)

message("Limpieza completada")

# Mostrar estadísticas
log_files <- fs::dir_ls(log_dir, regexp = "smartmodel_.*\\.(log|zip)$")
message("Archivos de log actuales: ", length(log_files))

if (length(log_files) > 0) {
  total_size <- sum(fs::file_size(log_files))
  message("Tamaño total: ", scales::label_bytes()(total_size))
}
