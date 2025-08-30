# 
# El contenedor tiene prefijo sm-cont-
export BSM_CONTAINER="sm-cont-${BSM_NAME}"
export BSM_CONTAINER_SERVICE="sm-svc-${BSM_NAME}"

echo "Variables asignadas en la sesión"
printenv | grep "BSM" | grep -v "BSM_PWD"

echo "Levantando contenedor SM efímero ${BSM_CONTAINER} en localhost:${BSM_PORT}"
docker compose --file ./config-contenedor-bsm-app.yml up \
  --remove-orphans --detach --wait --wait-timeout 30
if [ $? -ne 0 ]; then return 1; fi

# Pooling RServer
# Work around para evitar problema de carga del entorno desde .Rprofile. 
# El quarto a veces fallaba porque arrancaba sin el entorno totalmente cargado. 
# Antes lo solucionaba con sleep 1s. 
# sleep 1s 
# Pero parece ser que el problema es que el Rstudio Server tarda en ponerse en línea
# Lo resolví con el siguiente ciclo de pruebas. 

until docker exec "$BSM_CONTAINER_SERVICE" wget -qO- http://localhost:8787 > /dev/null; do
  echo "Esperando RStudio Server..."
  sleep 1
done

docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
  Rscript -e "stopifnot(exists('error_custom', mode = 'function'))"
if [ $? -ne 0 ]; then 
  echo "Fallo en Pooling_RServer!"
  echo "Librerias o R no disponibles!"
  return 1
fi

docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
  Rscript -e "if (fs::path_real(bsm_path) != getwd()) error_custom('Setup de R fallido!')"
if [ $? -ne 0 ]; then 
  echo "Fallo en Pooling_RServer!"
  echo "R con directorios SM no disponible!"
  return 1
else 
  echo "Hola SmartModel ${BSM_CONTAINER} en localhost:${BSM_PORT}"
fi


