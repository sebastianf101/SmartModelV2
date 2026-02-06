#!/usr/bin/env bash

# Help 
print_help() {
    echo "Uso: $0 Cuaderno Parametros Datos"
    echo "  - Si se provee Datos se los copia a Data/."
    echo "  - Si se provee Parametros se los copia a Param/."    
    echo "  - Sin argumentos ejecuta Tejer Cuadernos.R"
    echo "  - Sino se ejecuta Cuaderno"
}

# Check for --help option
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    print_help
    exit 0
fi

# Variables de entorno
source ./envvars_setting.sh

# Chequeo Parámetros y Datos. 
if [[ -z "$2" ]]; then
    echo "Atención: Argumento 2 no suministrado. Usando Parámetros viejos."
else
    if [[ -f "$2" ]]; then
        echo "Copiando archivo de Parámetros $2 a Param/"
        docker cp "$2" "sm:$BSM_DIR/Params"
    else
        echo "Atención: Argumento $2 no existe. Usando Parámetros viejos."
    fi
fi

if [[ -z "$3" ]]; then
    echo "Atención: Argumento 3 no suministrado. Usando Datos viejos."
else
    if [[ -f "$3" ]]; then
        echo "Copiando archivo de Datos $3 a Datos/"
        docker cp "$3" "sm:$BSM_DIR/Datos"
    else
        echo "Atención: Argumento $3 no existe. Usando Datos viejos."
    fi
fi

# Chequeo Script
if [[ -z "$1" ]]; then
    echo "Atención: Argumento 1 no suministrado. Usando Tejer Cuadernos.R."
    # Chequeo que exista Tejer Cuadernos.R
    if docker exec --user $BSM_USER --workdir $BSM_DIR \
       sm bash -c '[ -f "./Scripts/Tejer Cuadernos.R" ]'; then
        echo "Inicio Tejer Cuadernos.R"
        docker exec --user $BSM_USER --workdir $BSM_DIR \
        sm Rscript "Scripts/Tejer Cuadernos.R" > results.out 2>errors.out
        echo "Fin Tejer Cuadernos.R"
    else
        echo "Error: Tejer_Cuadernos.R no encontrado!"
        exit 1
    fi
else
    if docker exec --user $BSM_USER --workdir $BSM_DIR \
       sm bash -c "[ -f $1 ]"; then
        echo "Inicio tejido de $1"
        # No sé porqué pero quarto usa la standar output como errores!
        docker exec --user $BSM_USER --workdir $BSM_DIR \
        sm quarto render $1 > errors.out 2>results.out
        echo "Fin de tejido $1"
    else
        echo "Error: $1 no encontrado!"
        exit 1
    fi
fi

# Chequeo de errores en el tejido. 
if [[ ! -s errors.out ]]; then
    echo "Inicio copia de Reportes"
    docker cp sm:$BSM_DIR/Reportes ./Output
    echo "Reportes copiados a ./Output/"
else
    echo "Atención: Hubo errores. Revisar errors.out"
    exit 1
fi

