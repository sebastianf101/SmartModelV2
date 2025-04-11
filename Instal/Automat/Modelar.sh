#!/usr/bin/env bash

# Uso set +e para tener reintentos. 
# Uso set +e: Disable the -e option, allowing the script to continue even if a command fails.
set +e

# Help 
print_help() {
    echo "Obtiene Modelo"
    echo "Dado Parámetros y Muestra de Desarrollo"
    echo "Uso: $0 --input-dir <dir-entrada> [--output-dir <dir-salida>] [--help]"
    echo "  - En dir-entrada se esperan los archivos" 
    echo "    'Control de SmartModelStudio.xlsx' (ó json) y Muestra_Desarrollo.txt"
    echo "  - En dir-salida se dejan los logs de salida y errores, Modelo.zip, Reportes y Dataframe con scores."    
    echo "  - Si hubo errores este script retorna > 0."
    echo ""
    echo "Proceso normal"
    echo "Levanta un contenedor efímero"
    echo "Copia los archivos de dir-entrada a los dir /Param y /Datos del contenedor"
    echo "Ejecuta los cuadernos de Limpieza, Transformación y Modelado"
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

# Check if the first directory contains a file named "Muestra_Desarrollo.txt"
if [ ! -f "$INPUT_DIR/Muestra_Desarrollo.txt" ]; then
    echo "Error: $INPUT_DIR no contiene archivo de datos 'Muestra_Desarrollo.txt'"
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
pipeline_modelar() {
    # Variables de entorno aleatorias
    # BSM_NAME y BSM_PORT
    source ./envvars_efimeras.sh

    source ./compose_up.sh
    if [ $? -ne 0 ]; then return 1; fi      
    
    echo
    echo "Copiando datos de entrada en el contenedor"
    docker cp "$INPUT_DIR/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Params" 
    exit_status_1=$?  
    docker cp "$INPUT_DIR/Muestra_Desarrollo.txt" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Datos"
    exit_status_2=$?  
    docker exec --user root --workdir $BSM_DIR \
      "$BSM_CONTAINER_SERVICE" chown "$BSM_USER" \
      "$BSM_DIR/Params/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_DIR/Datos/Muestra_Desarrollo.txt"
    exit_status_3=$?        
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ] || [ $exit_status_3 -ne 0 ]; then 
      echo "Error: Fallo en copia de datos de entrada!"
      return 1; 
    fi
    
    echo
    echo "Inicio Cuaderno de Limpieza y Transformación"
    docker exec --user $BSM_USER --workdir $BSM_DIR \
      $BSM_CONTAINER_SERVICE quarto render "Cuadernos/Clean-Transf.qmd" \
       --log "$BSM_DIR/Trabajo/results_clean_transf.log" #--log-format json-stream
    exit_status_1=$?
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/results_clean_transf.log "$OUTPUT_DIR"/
    exit_status_2=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then 
      echo "Error: Cuaderno Clean-Transf.qmd falló."
      return 1; 
    fi
    echo "Fin Cuaderno de Limpieza y Transformación"

    echo "Inicio Cuaderno de Modelado"
    docker exec --user $BSM_USER --workdir $BSM_DIR \
      $BSM_CONTAINER_SERVICE quarto render "Cuadernos/Modelling.qmd" --log "$BSM_DIR/Trabajo/results_modelling.log" #--log-format json-stream    
    exit_status_1=$?
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/results_modelling.log "$OUTPUT_DIR"/
    exit_status_2=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then 
      echo "Error: Cuaderno Modelling.qmd falló."
      return 1; 
    fi
    echo "Fin Cuaderno de Modelado"

    echo "Inicio de copia de resultados en $OUTPUT_DIR"
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Reportes "$OUTPUT_DIR"/
    exit_status_1=$?    
    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/Modelo.zip "$OUTPUT_DIR"/
    exit_status_2=$?
    docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE Rscript -e "print('Chau R')"
    exit_status_3=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ] || [ $exit_status_3 -ne 0 ]; then 
      echo "Error: Fallo en copia de resultados!"
      return 1; 
    fi
    echo "Fin de copia de resultados en $OUTPUT_DIR"
    return 0
}

source ./Ejecutar_pipeline.sh pipeline_modelar

echo "*****************************************************************************"
echo "Fin de proceso de Limpieza, Transformacion y Modelado sin errores detectados."
echo "*****************************************************************************"
