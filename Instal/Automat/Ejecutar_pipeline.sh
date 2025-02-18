## Ejecuta pipeline.
## Espera que el PRIMER y único parámetro sea el nombre de la función bash 
## que implementa el pipeline como secuencia de comandos docker. 
## Incorpora lógica de reintentos.  Hasta 3 por defecto. 

my_func=$1

baja_contenedor() {
  echo "Inicio baja contenedor SM efimero ${BSM_CONTAINER}"
  echo "y el servicio provisto en el puerto $BSM_PORT"
  docker compose --file ./config-contenedor-bsm.yml down
  echo "Contenedor bajado. Chau SM"
  return 0
}

# Number of retries
retries=${BSM_RETRIES:-3}
echo "Número de intentos: $retries"
if [ -n "$BSM_RETRIES" ]; then
    echo "valor tomado de variable de entorno BSM_RETRIES"
fi

# Retry logic
for ((i=1; i<=retries; i++)); do
  echo "Intento $i de $retries de ejecución de Cuadernos"
  ## Pipeline in first parameter. 
  $my_func
  if [ $? -eq 0 ]; then
    break
  else
    echo "Intento $i de $retries de ejecución de $my_func falló. Bajando contenedor..."
    baja_contenedor
    sleep 1
  fi
done

# Check if all attempts failed
if [ $i -gt $retries ]; then
echo "Error: Ejecución de cuadernos falló luego de $retries intentos!"
baja_contenedor
exit 1
fi

baja_contenedor
