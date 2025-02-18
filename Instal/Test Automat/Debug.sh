#!/bin/bash

# Debug de problemas de carga del entorno

BSM_USER="user"
BSM_VERSION="10.3"
BSM_DIR="/home/$BSM_USER/Documents/besmart/$BSM_VERSION"
#BSM_CONTAINER_SERVICE=sm-svc-demo
BSM_CONTAINER_SERVICE=sm-svc-554vou8rv6

docker exec --user user --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
      Rscript -e "stopifnot(exists('bin_monot', mode = 'function')); print('Hola R')"

docker exec --user $BSM_USER --workdir $BSM_DIR \
      $BSM_CONTAINER_SERVICE quarto render "Cuadernos/Clean-Transf_v10.qmd" --log "$BSM_DIR/Trabajo/results_clean_transf.log" --log-format json-stream
