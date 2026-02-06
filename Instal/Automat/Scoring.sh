#!/usr/bin/env bash

# Uso set +e para tener reintentos. 
# Uso set +e: Disable the -e option, allowing the script to continue even if a command fails.
set +e

print_help() {
    echo "Scoring de Nueva Muestra"
    echo "dados Parámetros, Modelo y Muestra Scoring"
    echo "Uso: $0 --input-dir <dir-entrada> [--output-dir <dir-salida>] [--help]"
    echo "  - En dir-entrada se esperan los archivos" 
    echo "    'Control de SmartModelStudio.xlsx' (ó json), Modelo.zip, Muestra_Desarrollo.txt y Muestra_Scoring.txt"
    echo "  - En dir-salida se dejan los logs de salida y errores y el Reporte"    
    echo "  - Si hubo errores este script retorna > 0"
    echo ""
    echo "Proceso normal"
    echo "Levanta un contenedor efímero"
    echo "Copia los archivos de dir-entrada a los dir /Param, /Trabajo y /Datos del contenedor"
    echo "Ejecuta el Cuaderno de Scoring."
    echo "Copia los resultados a dir-salida"
    echo "Baja el contenedor"
}

####### Chequeo de Parámetros ------------------------------------

# Check for --help option
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    print_help
    exit 0
fi

# Parse named parameters
while [[ $# -gt 0 ]]; do
    case $1 in
        --input-dir)
            INPUT_DIR="$2"
            shift 2
            ;;
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
		--help)
            print_help
            exit 1
            ;;
		--h)
            print_help
            exit 1
            ;;    				
        *)
            echo "Invalid option: $1"
            print_help
            exit 1
            ;;
    esac
done

# Check if input-dir is given
if [ -z "$INPUT_DIR" ]; then
    echo "Error: --input-dir es obligatorio."
    print_help
    exit 1
fi

# Check if input-dir is a directory
if [ ! -d "$INPUT_DIR" ]; then
    echo "Error: $INPUT_DIR no se encuentra o no es un directorio!"
    exit 1
fi

# Default output-dir to input-dir if not provided
if [ -z "$OUTPUT_DIR" ]; then
    echo "Atención: No se suministró --output-dir. Se asume que es igual a --input-dir."
    OUTPUT_DIR="$INPUT_DIR"
fi

# Check if output-dir is a directory
if [ ! -d "$OUTPUT_DIR" ]; then
    echo "Error: $OUTPUT_DIR no se encuentra o no es un directorio!"
    exit 1
fi

# Check if the first directory contains a file named "Control.xlsx" or "Control.json"
if [ -f "$INPUT_DIR/Control de SmartModelStudio.xlsx" ]; then
    export PARAM_FILE_EXT=xlsx
elif [ -f "$INPUT_DIR/Control de SmartModelStudio.json" ]; then
    export PARAM_FILE_EXT=json
else
    echo "Error: $INPUT_DIR no contiene archivo de Parámetros 'Control de SmartModelStudio' .xlsx ni .json."
    exit 1
fi

if [ ! -f "$INPUT_DIR/Modelo.zip" ]; then
    echo "Error: $INPUT_DIR no contiene archivo de Modelo 'Modelo.zip'"
    exit 1
fi

# Check if the first directory contains a file named "Muestra_Desarrollo.txt"
if [ ! -f "$INPUT_DIR/Muestra_Desarrollo.txt" ]; then
    echo "Error: $INPUT_DIR no contiene archivo de datos 'Muestra_Desarrollo.txt'"
    exit 1
fi

# Check if the first directory contains a file named "Muestra_Scoring.txt"
if [ ! -f "$INPUT_DIR/Muestra_Scoring.txt" ]; then
    echo "Error: $INPUT_DIR no contiene archivo de datos 'Muestra_Scoring.txt'"
    exit 1
fi

echo "Using input directory: $INPUT_DIR"
echo "Using output directory: $OUTPUT_DIR"

## Cambio el directorio the trabajo a la dirección de este script. 
# Los paths se convierten en relativos al script. 
cd $(dirname "$0")

####### Params y Vars ---------------------------------------

# Parece que a veces fallan los cuadernos porque no consigue alocar suficientes recursos. 
# En ese caso editar las variables *_MIN en envvars_defaults.sh.
source ./envvars_defaults.sh
source ./envvars_sesion.sh

####### Levantar Contenedor Efímero  ------------------------------------

# Encapsulo toda la lógica de comandos docker para permitir tres reintentos. 
# Parece que a veces fallan los cuadernos porque no consigue alocar suficientes recursos. 
pipeline_scoring() {
    # Variables de entorno aleatorias
    # BSM_NAME y BSM_PORT
    source ./envvars_efimeras.sh
    
    source ./compose_up.sh
    if [ $? -ne 0 ]; then return 1; fi      
    
    echo "Copiando datos de entrada en el contenedor"
    docker cp "$INPUT_DIR/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Params"
    exit_status_1=$?              
    docker cp "$INPUT_DIR/Muestra_Desarrollo.txt" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Datos"
    exit_status_2=$?              
    docker cp "$INPUT_DIR/Muestra_Scoring.txt" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Datos"
    exit_status_3=$?              
    docker cp "$INPUT_DIR/Modelo.zip" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo"
    exit_status_4=$?              
    docker exec --user root --workdir $BSM_DIR \
      "$BSM_CONTAINER_SERVICE" chown "$BSM_USER" \
      "$BSM_DIR/Params/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_DIR/Datos/Muestra_Desarrollo.txt" \
      "$BSM_DIR/Datos/Muestra_Scoring.txt" \
      "$BSM_DIR/Trabajo/Modelo.zip"      
    exit_status_5=$?              
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ] || [ $exit_status_3 -ne 0 ] \
      || [ $exit_status_4 -ne 0 ] || [ $exit_status_5 -ne 0 ]; then 
      echo "Error: Fallo en copia de datos de entrada!"
      return 1; 
    fi
    
    echo "Inicio Cuaderno de Validación Nueva Muestra"
    docker exec --user $BSM_USER --workdir $BSM_DIR \
      $BSM_CONTAINER_SERVICE quarto render "Cuadernos/Scoring.qmd" --log "$BSM_DIR/Trabajo/results_scoring.log" #--log-format json-stream
    exit_status_1=$?                      
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/results_scoring.log "$OUTPUT_DIR"/  
    exit_status_2=$?                    
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then 
      echo "Error: Cuaderno Scoring.qmd falló."
      return 1; 
    fi
    echo "Fin Cuaderno de Validación Nueva Muestra"
    
    echo "Inicio de copia de resultados en $OUTPUT_DIR"
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Reportes "$OUTPUT_DIR"/ 
    exit_status_1=$?
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/Scores.txt "$OUTPUT_DIR"/ 
    exit_status_2=$?
    docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE Rscript -e "print('Chau R')"
    exit_status_3=$?    
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ] || [ $exit_status_3 -ne 0 ]; then 
      echo "Error: Fallo en copia de resultados!"
      return 1; 
    fi
    echo "Fin de copia de resultados en "$OUTPUT_DIR""
    return 0
}

source ./Ejecutar_pipeline.sh pipeline_scoring

echo "*****************************************************************************"
echo "    Fin de proceso de Scoring en Muestra Nueva sin errores detectados.    "
echo "*****************************************************************************"

