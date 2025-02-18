# Script para setear la estructura inicial.  
# Ojo que limpia todos los archivos de las carpetas nombradas abajo. 
# Conserva las otras carpetas. 
# 
setwd("~/Documents/besmart/10.3/")
version_bsm <- "10.3"
paste0("/var/data/besmart/versiones/", version_bsm) -> version_path
options(digits=3) # Fija en la sesión el nro. de digitos decimales a visualizar. 

msg <- paste("Esta a punto de actualizar a la version", version_bsm, 
             "y de borrar los contenidos de los directorios Cuadernos, Datos, Params, Scripts y Trabajo.", 
             "Esta seguro de querer continuar?. Responda S o N (por Si o No)", collapse = " ")

rstudioapi::showPrompt("ATENCION!", message = msg, default = "N") -> resp
if (resp == "S" | resp == "s") {
  unlink("Auxil", recursive = TRUE, force = TRUE)
  unlink("Cuadernos", recursive = TRUE, force = TRUE)
  unlink("Datos", recursive = TRUE, force = TRUE)
  unlink("Ayuda", recursive = TRUE, force = TRUE)
  unlink("Librerias", recursive = TRUE, force = TRUE)
  unlink("Params", recursive = TRUE, force = TRUE)
  unlink("Reportes", recursive = TRUE, force = TRUE)
  unlink("Scripts", recursive = TRUE, force = TRUE)
  unlink("Trabajo", recursive = TRUE, force = TRUE)
  fs::path_wd("Trabajo") |> fs::dir_create()
  fs::path_wd("Reportes") |> fs::dir_create()  
  fs::path(version_path, "Ej_Inicial") |> 
    fs::dir_copy(new_path = fs::path_wd(), overwrite = TRUE)
  fs::path_wd() |> 
    fs::path("Scripts/Tejer Cuadernos.R") |> 
    rstudioapi::navigateToFile()
}


