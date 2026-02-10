#!/bin/bash
# Cambiar las variables de entorno sólo si se sabe que hacen
# Ejecutar con source para que las variables tengan efecto durante toda la sesión. 
# source ./envvars_efimeras.sh

# Define the range (MIN_PORT and MAX_PORT)
MIN_PORT=10000
MAX_PORT=49151
# Function to check if a port is free using `ss` (available on Ubuntu)
# This avoids depending on `lsof`. We check listening TCP/UDP ports and
# consider a port used if it appears as a local endpoint in `ss -lntu` output.
is_port_free() {
  local port=$1
  # Prefer ss (iproute2). If not available, fall back to lsof if present.
  if command -v ss >/dev/null 2>&1; then
    # List local endpoints and match a trailing :port (covers IPv4 and IPv6 like 127.0.0.1:80 and [::]:22)
    if ss -lntu 2>/dev/null | awk '{print $5}' | grep -E -q ":[[:alnum:]:]*:${port}$|:${port}$"; then
      return 1
    else
      return 0
    fi
  elif command -v lsof >/dev/null 2>&1; then
    # Use lsof to check if any process is listening on the port
    if lsof -iTCP -sTCP:LISTEN -Pn 2>/dev/null | awk '{print $9}' | grep -E -q ":${port}$"; then
      return 1
    else
      return 0
    fi
  else
    echo "Warning: neither 'ss' nor 'lsof' available; assuming port $port is free." >&2
    return 0
  fi
}

# Main script: pick two distinct free ports
max_attempts=200
attempts=0
selected_ports=()
while [ ${#selected_ports[@]} -lt 2 ] && [ $attempts -lt $max_attempts ]; do
  port=$((MIN_PORT + RANDOM % (MAX_PORT - MIN_PORT + 1)))
  # Skip if already selected
  already=false
  for p in "${selected_ports[@]}"; do
    if [ "$p" -eq "$port" ]; then
      already=true
      break
    fi
  done
  if $already; then
    attempts=$((attempts + 1))
    continue
  fi
  if is_port_free $port; then
    selected_ports+=("$port")
    echo "Selected free port: $port"
  else
    echo "Port $port is in use, skipping." 
  fi
  attempts=$((attempts + 1))
done

if [ ${#selected_ports[@]} -lt 2 ]; then
  echo "Failed to find 2 free ports after $max_attempts attempts."
  # Uso exit y no return porque en ese punto no hay contenedor levantado.
  exit 1
fi

# Assign the two found ports to the expected env vars
export BSM_DASHBOARD_PORT=${selected_ports[0]}
export BSM_SSH_PORT=${selected_ports[1]}

# If BSM_NAME is already set, do nothing
if [ -z "$BSM_NAME" ]; then
  # Fallback: random name if BSM_NAME was not provided
  # Generate a random string of 10 characters
  export BSM_NAME=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 10 | head -n 1)
fi

echo "Puerto para Tableros efímero: $BSM_DASHBOARD_PORT"
echo "Puerto para SSH efímero: $BSM_SSH_PORT"
echo "Nombre contenedor SM efímero: $BSM_NAME"
echo
