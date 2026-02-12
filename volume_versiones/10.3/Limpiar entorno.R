# Script para setear la estructura inicial.
# Ojo que limpia todos los archivos de las carpetas nombradas abajo.
# Conserva las otras carpetas.
#
version_bsm <- "10.3"
bsm_dir <- Sys.getenv("BSM_DIR", unset = fs::path("~/Documents/besmart", version_bsm))
bsm_path <- fs::path(bsm_dir)
setwd(bsm_path)
options(digits=3) # Fija en la sesión el nro. de digitos decimales a visualizar.

unlink("Auxil", recursive = TRUE, force = TRUE)
unlink("Logs", recursive = TRUE, force = TRUE)
unlink("Reportes", recursive = TRUE, force = TRUE)
unlink("Trabajo", recursive = TRUE, force = TRUE)
fs::path(bsm_path, "Auxil") |> fs::dir_create()
fs::path(bsm_path, "Logs") |> fs::dir_create()
fs::path(bsm_path, "Reportes") |> fs::dir_create()
fs::path(bsm_path, "Trabajo") |> fs::dir_create()



