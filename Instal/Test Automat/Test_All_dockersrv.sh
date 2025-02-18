#!/bin/bash
#
# Valida todos los cuadernos
# 
# Parte de tener en Entrada sólo 
# Muestra_Desarrollo.txt, Muestra_Validación.txt
# Control de SmartModelStudio.xlsx

./Test_Modelar_dockersrv.sh
cp ./Salida/Modelo.zip ./Entrada/
./Test_Validar_Desa_dockersrv.sh
cp ./Salida/Modelo.zip ./Entrada/
./Test_Scoring_dockersrv.sh
./Test_Validar_Nueva_dockersrv.sh
