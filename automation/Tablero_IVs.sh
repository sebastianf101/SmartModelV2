#!/usr/bin/env bash

# Uso set +e para tener reintentos.
# Uso set +e: Disable the -e option, allowing the script to continue even if a command fails.
set +e

print_help() {
    echo "Tablero de reportes de IV"
    echo "dados Parámetros y reportes de Estabilidad de IVS"
    echo "Uso: $0 --input-dir <dir-entrada> [--output-dir <dir-salida>] [--help]"
    echo "  - En dir-entrada se esperan los archivos"
    echo "    'Control de SmartModelStudio.xlsx' (ó json), Modelo.zip y opcionalmente Estab_ivs_*.Rdat"
    echo "  - En dir-salida se dejan los logs de salida y errores"
    echo "  - Si hubo errores este script retorna > 0"
    echo ""
    echo "Proceso normal"
    echo "Levanta un contenedor efímero"
    echo "Copia los archivos de dir-entrada a los dir /Param, /Trabajo y /Datos del contenedor"
    echo "Ejecuta el cuaderno Explorador_Estab_ivs.qmd"
    echo "Abre un browser en http://localhost:$BSM_DASHBOARD_PORT"
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

echo "Using input directory: $INPUT_DIR"
echo "Using output directory: $OUTPUT_DIR"

## Cambio el directorio the trabajo a la dirección de este script.
# Los paths se convierten en relativos al script.
cd $(dirname "$0")

####### Params y Vars ---------------------------------------

# Parece que a veces fallan los cuadernos porque no consigue alocar suficientes recursos.
# En ese caso editar las variables *_MIN en .env.
ENV_FILE="${ENV_FILE:-../.env}"
if [ -f "$ENV_FILE" ]; then
  set -a
  source "$ENV_FILE"
  set +a
fi
source ./envvars_sesion.sh

###### Funciones auxiliares ---------------------------------------------

# Function to check if a URL is responsive
is_port_responsive() {
  local port=$1
  nc -z 127.0.0.1 $port
  return $?
}

####### Levantar Contenedor Efímero  ------------------------------------

# Encapsulo toda la lógica de comandos docker para permitir tres reintentos.
# Parece que a veces fallan los cuadernos porque no consigue alocar suficientes recursos.
pipeline_desa() {
    # Variables de entorno aleatorias
    # BSM_NAME y BSM_SSH_PORT
    source ./envvars_efimeras.sh

    source ./compose_up.sh
    if [ $? -ne 0 ]; then return 1; fi

    echo "Copiando datos de entrada en el contenedor"
    docker cp "$INPUT_DIR/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Params"
    exit_status_1=$?
    docker cp "$INPUT_DIR/Modelo.zip" \
      "$BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo"
    exit_status_2=$?
    docker exec "$BSM_CONTAINER_SERVICE" chown "$BSM_USER:$BSM_USER" \
      "$BSM_DIR/Params/Control de SmartModelStudio.$PARAM_FILE_EXT" \
      "$BSM_DIR/Trabajo/Modelo.zip"
    exit_status_3=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ] || [ $exit_status_3 -ne 0 ]; then
      echo "Error: Fallo en copia de datos de entrada!"
      return 1;
    fi

    # Copia cualquier archivo Estab_ivs_*.Rdat a Trabajo/
    temp_file=$(mktemp)
    ls "$INPUT_DIR"/Estab_ivs_*.Rdat 2> /dev/null > "$temp_file"
    if [ ! -s "$temp_file" ]; then
        echo "Warning: No files matching the pattern '$INPUT_DIR/Estab_ivs_*.Rdat' were found"
    else
        while IFS= read -r file; do
          filename=$(basename "$file")
          echo "Copiando $filename en /Trabajo"
          docker cp "$INPUT_DIR/$filename" "$BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo"
          exit_status_1=$?
          docker exec "$BSM_CONTAINER_SERVICE" chown "$BSM_USER:$BSM_USER" \
            "$BSM_DIR/Trabajo/$filename"
          exit_status_2=$?
              if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then
                echo "Error: Copia de $filename falló!"
                return 1;
              fi
        done < "$temp_file"
    fi
    rm "$temp_file"

    echo "Inicio Tablero de IVs"

    docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
      touch "$BSM_DIR/Trabajo/results_dashboard.log"
    exit_status_1=$?
    # Lanzo el tablero en background.  Sale con wait.
    docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
      quarto serve "Tableros/Explorador_Estab_ivs.qmd" \
        --log "$BSM_DIR/Trabajo/results_dashboard.log" \
        --host "0.0.0.0" --port 3838 &
    exit_status_2=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then
      echo "Error: Tablero Explorador_Estab_ivs.qmd falló."
      return 1;
    fi
    # Abre el tablero y el botón de salir
    url="http://localhost:$BSM_DASHBOARD_PORT"
    while ! is_port_responsive $BSM_DASHBOARD_PORT; do
      echo "Esperando al tablero en puerto $BSM_DASHBOARD_PORT"
      sleep 1
    done
    echo "Abriendo tablero en $url"

    is_wsl() {
        grep -qEi "(Microsoft|WSL)" /proc/version &> /dev/null
        return $?
    }

    if is_wsl; then
        echo "WSL detectado.  Usando powershell"
        echo "Abriendo tablero en $url"
        powershell.exe /c start "$url"
    else
        echo "WSL no detectado.  Usando xdg-open"
        xdg-open "$url"
    fi
    wait
    echo "Cerrando tablero en $url"

    docker cp $BSM_CONTAINER_SERVICE:$BSM_DIR/Trabajo/results_dashboard.log "$OUTPUT_DIR"/
    exit_status_1=$?
    docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE Rscript -e "print('Chau R')"
    exit_status_2=$?
    if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then
      echo "Error: Fin del Tablero Explorador_Estab_ivs.qmd con fallas!"
      return 1;
    fi
    echo "Fin Tablero de IVs"
    return 0
}

source ./Ejecutar_pipeline.sh pipeline_desa

echo "*****************************************************************************"
echo "Fin de proceso de Tablero de IVs sin errores detectados."
echo "*****************************************************************************"
