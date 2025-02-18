# Lectura de parámetros
# Asume que esta cargado version_path
clean_knit_cache()
sys.source(fs::path(version_path, "Librerias/Setup.R"), envir = globalenv(), keep.source = FALSE)
