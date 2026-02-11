#!/usr/bin/env bash
##!/bin/bash

# Default values
ENV_FILE="${ENV_FILE:-../.env}"
if [ -f "$ENV_FILE" ]; then
        set -a
        source "$ENV_FILE"
        set +a
fi
# port variables will be set to free ephemeral ports
source ./envvars_efimeras.sh

show_help() {
    echo "Uso: $0 [--name NOMBRE] [--dashboard PUERTO_TAB] [--ssh PUERTO_SSH] [--yml CONFIG] [--help]"
    echo
    echo "Opciones:"
    echo "  --name NOMBRE      Nombre del contenedor (por defecto: \$BSM_NAME)"
    echo "  --dashboard PUERTO Puerto para tableros interactivos (por defecto: \$BSM_DASHBOARD_PORT)"
    echo "  --ssh PUERTO       Puerto para acceso SSH al contenedor (por defecto: \$BSM_SSH_PORT)"
    echo "  --yml CONFIG       Perfil de docker-compose a usar (bsm|api) o ruta a un archivo .yml (por defecto: \$BSM_YML)"
    echo "  --help             Muestra esta ayuda"
    echo
    echo "Ejemplos:"
    echo "  ./Levantar_Contenedor.sh --name Mi_SM --ssh 2222"
    echo "  ./Levantar_Contenedor.sh --name Mi_SM --dashboard 3001 --yml api"
    echo
}

# Parse parameters
while [[ "$1" != "" ]]; do
    case $1 in
        --name ) shift
                 BSM_NAME=$1
                 ;;
        --ssh ) shift
                BSM_SSH_PORT=$1
                ;;
        --dashboard ) shift
                BSM_DASHBOARD_PORT=$1
                ;;
        --yml ) shift
                BSM_YML=$1
                ;;
        --help ) show_help
                 exit 0
                 ;;
        * ) echo "Unknown parameter: $1"
                 show_help
                 exit 1
                 ;;
    esac
    shift
done

export BSM_NAME=$BSM_NAME
export BSM_DASHBOARD_PORT=$BSM_DASHBOARD_PORT
export BSM_SSH_PORT=$BSM_SSH_PORT
export BSM_YML=$BSM_YML

# Check if the selected ports are free
if ! is_port_free "$BSM_DASHBOARD_PORT"; then
  echo "Puerto $BSM_DASHBOARD_PORT ocupado. Elegir otro!"
  exit 1
fi
if ! is_port_free "$BSM_SSH_PORT"; then
  echo "Puerto $BSM_SSH_PORT ocupado. Elegir otro!"
  exit 1
fi

echo "Nombre contenedor SM: $BSM_NAME"
echo "Configuración YML: $BSM_YML"
echo "Puerto para Tableros: $BSM_DASHBOARD_PORT"
echo "Puerto para SSH: $BSM_SSH_PORT"
echo

# Actual call to docker compose up
source ./compose_up.sh $BSM_YML


