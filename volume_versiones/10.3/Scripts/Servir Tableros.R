# Script para correr Dashboards o Tableros.
# Se puede ejecutar apretando el botón de source o con CTRL-Enter sobre la línea
# Este script asume que los archivos tienen codificación UTF-8
# Recordar cortar el servicio apretando el "Salir" en la UI, botón rojo ó con CTRL-C

# Asume que se corrió antes Validation_InS.qmd
"Tableros/Explorador_Estab_ivs.qmd" |> 
  quarto::quarto_serve(host= "0.0.0.0", port = 3838, browse = interactive())

