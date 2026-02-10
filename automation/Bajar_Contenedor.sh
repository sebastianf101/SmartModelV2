#!/usr/bin/env bash
##!/bin/bash

# Default values.
source ./envvars_defaults.sh

# Override with command line arguments if provided
if [[ "$1" != "--help" ]]; then
    export BSM_NAME=${1:-$BSM_NAME}
fi

export BSM_SSH_PORT=$(docker inspect --format='{{(index (index .NetworkSettings.Ports "22/tcp") 0).HostPort}}' sm-svc-$BSM_NAME)

# Chequeo si se encontró el puerto.
if [ -z "$BSM_SSH_PORT" ]; then
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

echo "Bajando contenedor SM efimero sm-cont-${BSM_NAME}"
echo "SSH en puerto $BSM_SSH_PORT"
docker compose --file ./config-contenedor-bsm.yml down --remove-orphans
echo "Chau SM"
