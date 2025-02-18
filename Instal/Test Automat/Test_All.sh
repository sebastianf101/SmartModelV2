#!/bin/bash
#
# Valida todos los cuadernos
# 
# Parte de tener en Entrada sólo 
# Muestra_Desarrollo.txt, Muestra_Validación.txt
# Control de SmartModelStudio.xlsx

./Test_Modelar.sh
cp ./Salida/Modelo.zip ./Entrada/
./Test_Validar_Desa.sh
# Ojo que Validar Desa actualiza Modelo!
cp ./Salida/Modelo.zip ./Entrada/
./Test_Validar_Nueva.sh
cp ./Salida/Estab_ivs_mod_OoS.Rdat ./Entrada/
./Test_Scoring.sh
./Test_Tablero_IVs.sh


