#!/bin/bash
# Cambiar las variables de entorno sólo si se sabe que hacen
# Ejecutar con source para que las variables tengan efecto durante toda la sesión. 
# source ./envvars_efimeras.sh

# Define the range (MIN_PORT and MAX_PORT)
MIN_PORT=10000
MAX_PORT=49151
# Function to check if a port is free
is_port_free() {
  local port=$1
  if lsof -i:$port > /dev/null; then
    return 1
  else
    return 0
  fi
}

# Main script
max_attempts=10
attempts=0
ports_found=false
while [ $attempts -lt $max_attempts ]; do
  # Generate a random number within the range
  port=$((MIN_PORT + RANDOM % (MAX_PORT - MIN_PORT + 1)))
  next_port=$((port + 1))
  # Si is_port_free retorna 0 es true, 1 es false
  if is_port_free $port && is_port_free $next_port; then 
    echo "Both ports $port and $next_port are free." 
    ports_found=true
    break
  else 
    echo "At least one of the ports $port or $next_port is in use." 
  fi
  attempts=$((attempts + 1))
done

if ! $port_found; then
  echo "Failed to find a free port after $max_attempts attempts."
  # SF: Uso exit y no return porque en ese punto no hay contenedor levantado. 
  exit 1
fi

export BSM_PORT=$port
export BSM_DASHBOARD_PORT=$next_port

# Si USER_ID está seteado, usalo como nombre
if [ -n "$USER_ID" ]; then
  export BSM_NAME=$USER_ID
else
  # Fallback: nombre aleatorio si no se pasó USER_ID
  # Generate a random string of 10 characters
  export BSM_NAME=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 10 | head -n 1)
fi

echo "Puerto SM efímero: $BSM_PORT"
echo "Puerto para Tableros efímero: $BSM_DASHBOARD_PORT"
echo "Nombre contenedor SM efímero: $BSM_NAME"
