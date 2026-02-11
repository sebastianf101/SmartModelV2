# Script para setear la estructura inicial.
# Ojo que limpia todos los archivos de las carpetas nombradas abajo.
# Conserva las otras carpetas.
#
setwd("~/Documents/besmart/10.3/")
version_bsm <- "10.3"
bsm_path <- fs::path("~/Documents/besmart", version_bsm)
options(digits=3) # Fija en la sesión el nro. de digitos decimales a visualizar.

msg <- paste("Esta a punto de actualizar a la version", version_bsm,
             "y de borrar los contenidos de los directorios Auxil, Logs, Reportes y Trabajo.",
             "Esta seguro de querer continuar?. Responda S o N (por Si o No)", collapse = " ")

unlink("Auxil", recursive = TRUE, force = TRUE)
unlink("Logs", recursive = TRUE, force = TRUE)
unlink("Reportes", recursive = TRUE, force = TRUE)
unlink("Trabajo", recursive = TRUE, force = TRUE)
fs::path(bsm_path, "Auxil") |> fs::dir_create()
fs::path(bsm_path, "Logs") |> fs::dir_create()
fs::path(bsm_path, "Reportes") |> fs::dir_create()
fs::path(bsm_path, "Trabajo") |> fs::dir_create()
fs::path(bsm_path, "Scripts/Tejer Cuadernos.R")


