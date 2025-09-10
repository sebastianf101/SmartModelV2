#!/usr/bin/env bash
##!/bin/bash

# Default values
BSM_NAME="efimero"
BSM_PORT="3001"
BSM_DASHBOARD_PORT="3002"
BSM_YML="bsm"

# Function to display help
show_help() {
    echo "Usage: $0 [--name BSM_NAME] [--port BSM_PORT] [--dashboard BSM_DASHBOARD_PORT] [--yml BSM_YML] [--help]"
    echo
    echo "Options:"
    echo "  --name BSM_NAME    Nombre del contenedor (default: \$BSM_NAME ó 'efimero')"
    echo "  --port BSM_PORT    Puerto asociado (default: \$BSM_PORT ó '3001')"
    echo "  --dashboard BSM_DASHBOARD_PORT Puerto reservado para tableros interactivos (default: \$BSM_DASHBOARD_PORT ó '3002')"
    echo "  --yml BSM_YML      Configuración de compose a usar (default: \$BSM_YML ó 'bsm')"
    echo "  --help             Muestra esta explicación"
    echo 
    echo "Ejemplo de Contenedor Estándar:"
    echo "./Levantar_Contenedor.sh --name Mi_SM --port 3000"    
    echo "Ejemplo de Contenedor con API de Tableros:"
    echo "./Levantar_Contenedor.sh --name Mi_SM --port 3000 --yml api"
}

# Parse parameters
while [[ "$1" != "" ]]; do
    case $1 in
        --name ) shift
                 BSM_NAME=$1
                 ;;
        --port ) shift
                 BSM_PORT=$1
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
        * )      echo "Unknown parameter: $1"
                 show_help
                 exit 1
                 ;;
    esac
    shift
done

if lsof -i:$BSM_PORT > /dev/null; then
  echo "Puerto $BSM_PORT ocupado.  Elegir otro!"
  exit 1
elif lsof -i:$BSM_DASHBOARD_PORT > /dev/null; then
  echo "Puerto $BSM_DASHBOARD_PORT ocupado.  Elegir otro!"
  exit 1
fi

export BSM_NAME=$BSM_NAME
export BSM_PORT=$BSM_PORT
export BSM_DASHBOARD_PORT=$BSM_DASHBOARD_PORT

echo "Puerto SM: $BSM_PORT"
echo "Puerto para Tableros: $BSM_DASHBOARD_PORT"
echo "Nombre contenedor SM: $BSM_NAME"
echo "Configuración YML: $BSM_YML"

# Resto de las variables asume default. 
source ./envvars_defaults.sh

source ./compose_up.sh $BSM_YML


