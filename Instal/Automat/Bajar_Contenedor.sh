#!/usr/bin/env bash
##!/bin/bash

# Default values
DEFAULT_BSM_NAME="efimero"

# Use environment variables if set, otherwise use default values
BSM_NAME=${BSM_NAME:-$DEFAULT_BSM_NAME}

# Override with command line arguments if provided
if [[ "$1" != "--help" ]]; then
    export BSM_NAME=${1:-$BSM_NAME}
fi

export BSM_PORT=$(docker inspect --format='{{(index (index .NetworkSettings.Ports "8787/tcp") 0).HostPort}}' sm-svc-$BSM_NAME)

# Chequeo si se econtró el puerto. 
if [ -z "$BSM_PORT" ]; then
  echo "No se encontró puerto para el servicio sm-svc-${BSM_NAME}"
  echo "Está corriendo el contenedor sm-cont-${BSM_NAME}?"
  exit 1
fi

# Function to display help
show_help() {
    echo "Comando para bajar contenedor SmartModel efímero."
    echo "Uso: $0 [BSM_NAME]"
    echo
    echo "Parámetros:"
    echo "  BSM_NAME    El nombre del contenedor sin prefijo (default: $BSM_NAME)"
    echo
    echo "Opciones:"
    echo "  --help  Mostrar este help y salir."
    echo 
    echo "Ejemplo:"
    echo "./Bajar_Contenedor.sh Mi_SM"
    echo
    echo "El comando baja la imagen de SM en el contenedor sm-cont-$BSM_NAME"
}

# Check for --help parameter
if [[ "$1" == "--help" ]]; then
    show_help
    exit 0
fi

# Resto de las variables asume default. 
source ./envvars_defaults.sh

echo "Bajando contenedor SM efimero sm-cont-${BSM_NAME}"
echo "y el servicio provisto en el puerto $BSM_PORT"
docker compose --file ./config-contenedor-bsm.yml down
echo "Chau SM"
