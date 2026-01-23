# Lectura de parámetros
# Requiere que .Rprofile.site haya inicializado el entorno (bsm_path, logging, etc.)
clean_knit_cache()
sys.source(fs::path(bsm_path, "Librerias/Setup.R"), envir = globalenv(), keep.source = TRUE)
