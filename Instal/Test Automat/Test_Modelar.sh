#!/bin/bash
#
# Test Modelar.sh
# 
#
SCRIPT_FILE="Modelar.sh"
BASE_DIR="/mnt/c/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelStudioV1/Instal"
SCRIPT_DIR="$BASE_DIR/Automat"
SCRIPT_PATH="$SCRIPT_DIR/$SCRIPT_FILE"
DIR_ENTRADA="$BASE_DIR/Test Automat/Entrada"
DIR_SALIDA="$BASE_DIR/Test Automat/Salida"

"$SCRIPT_PATH" --input-dir "$DIR_ENTRADA" --output-dir "$DIR_SALIDA" 2>&1 | tee "$DIR_SALIDA/modelling.out"
