#!/bin/bash
#
# Test Modelar.sh en DockerSRV
# 
echo $PWD
DIR_BASE="/mnt/c/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelStudioV1/Instal"
DIR_SCRIPT="$DIR_BASE/Automat"
DIR_ENTRADA="$DIR_BASE/Test Automat/Entrada"
DIR_SALIDA="$DIR_BASE/Test Automat/Salida"
source "$DIR_SCRIPT/Ejecutar_dockersrv.sh" \
  --remote_script Scoring.sh \
  --dir_in "$DIR_ENTRADA" \
  --dir_out "$DIR_SALIDA" \
   2>&1 | tee "$DIR_SALIDA/scoring.out"
