#!/bin/bash
# Quarto no tiene flexibilidad en output-dir
# Lo resuelvo con un script
for r in $QUARTO_PROJECT_OUTPUT_FILES
do
  if [ "$r" == Cuadernos/*.html ]; then
    echo "Moviendo ${r} a Reportes/"
    mv ${r} Reportes/
  fi
done
