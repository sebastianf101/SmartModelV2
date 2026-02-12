# Script para correr todos los cuardernos de SmartModel en secuencia.
# Se puede ejecutar apretando el botón de source o llamando a source("~/Documents/besmart/Scripts/10.x/Tejer Cuadernos.R")
# Este script asume que los archivos tienen codificación UTF-8
# Requiere que .Rprofile.site haya inicializado el entorno (bsm_path, logging, etc.)

log_set_notebook("Clean-Transf.qmd")
fs::path(bsm_path, "Cuadernos/Clean-Transf.qmd") |> quarto::quarto_render()
log_clear_notebook()

log_set_notebook("Modelling.qmd")
fs::path(bsm_path, "Cuadernos/Modelling.qmd") |> quarto::quarto_render()
log_clear_notebook()

log_set_notebook("Validation_InS.qmd")
fs::path(bsm_path, "Cuadernos/Validation_InS.qmd") |> quarto::quarto_render()
log_clear_notebook()

log_set_notebook("Scoring.qmd")
fs::path(bsm_path, "Cuadernos/Scoring.qmd") |> quarto::quarto_render()
log_clear_notebook()

log_set_notebook("Validation_OoS.qmd")
fs::path(bsm_path, "Cuadernos/Validation_OoS.qmd") |> quarto::quarto_render()
log_clear_notebook()

log_info("Todos los cuadernos se tejieron exitosamente")
