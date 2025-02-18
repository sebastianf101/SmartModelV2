#!/bin/bash
#
# Test Validar_Nueva.sh en DockerSRV
# 
echo $PWD
DIR_BASE="/mnt/c/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelStudioV1/Instal"
DIR_SCRIPT="$DIR_BASE/Test Automat"
DIR_ENTRADA="$DIR_BASE/Test Automat/Entrada"
DIR_SALIDA="$DIR_BASE/Test Automat/Salida"
echo "${DIR_ENTRADA}"
echo "${DIR_SALIDA}"
#ls "${DIR_ENTRADA}"
#ls "${DIR_SALIDA}"
source "$DIR_SCRIPT/Debug2.sh" \
  --remote_script Modelar.sh \
  --dir_in "$DIR_ENTRADA" \
  --dir_out "$DIR_SALIDA"

