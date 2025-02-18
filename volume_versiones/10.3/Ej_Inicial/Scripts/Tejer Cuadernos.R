# Script para correr todos los cuardernos de SmartModel en secuencia.
# Se puede ejecutar apretando el botón de source o llamando a source("~/Documents/besmart/Scripts/10.x/Tejer Cuadernos.R")
# Este script asume que los archivos tienen codificación UTF-8

# Asume que ya está preparado el entorno

"Cuadernos/Clean-Transf.qmd" |> quarto::quarto_render()
"Cuadernos/Modelling.qmd" |> quarto::quarto_render()
"Cuadernos/Validation_InS.qmd" |> quarto::quarto_render()
"Cuadernos/Scoring.qmd" |> quarto::quarto_render()
"Cuadernos/Validation_OoS.qmd" |> quarto::quarto_render()
