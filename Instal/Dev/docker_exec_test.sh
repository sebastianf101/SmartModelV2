##!/bin/bash


export BSM_NAME="desa"
# El contenedor tiene prefijo sm-cont-
export BSM_CONTAINER="sm-cont-${BSM_NAME}"
export BSM_CONTAINER_SERVICE="sm-svc-${BSM_NAME}"

# Resto de las variables asume default. 
source ./envvars_defaults.sh

echo "Ejecutando comando"
echo "docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE"
echo 

docker exec --user "user" --workdir "/home/user/Documents/besmart/10.3" sm-svc-desa \
  Rscript -e "stopifnot(exists('error_custom', mode = 'function'))"

echo "Ejecutando comando con envs"

docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
  Rscript -e "stopifnot(exists('error_custom', mode = 'function'))"
if [ $? -ne 0 ]; then 
  echo "Fallo en Pooling_RServer!"
  echo "Librerias o R no disponibles!"
  return 1
fi
