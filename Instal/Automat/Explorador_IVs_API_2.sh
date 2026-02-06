#!/usr/bin/env bash

# Uso set +e para tener reintentos. 
# Uso set +e: Disable the -e option, allowing the script to continue even if a command fails.
set +e

print_help() {
    echo "Tablero de reportes de IV"
    echo "dados Parámetros y reportes de Estabilidad de IVS"
    echo "Uso: $0 --input-dir <dir-entrada> [--output-dir <dir-salida>] [--user-id <user_id>] [--help]"
    echo "  - En dir-entrada se esperan los archivos" 
    echo "    'Control de SmartModelStudio.xlsx' (ó json), Modelo.zip y opcionalmente Estab_ivs_*.Rdat"
    echo "  - En dir-salida se dejan los logs de salida y errores"    
    echo "  - Si no se proporciona output-dir se asume igual a input-dir"
    echo "  - user_id identifica al contenedor subyacente.  Debe ser único por host."
    echo "  - Si no se proporciona se genera un id al azar."
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

while [[ $# -gt 0 ]]; do
    case $1 in
        --bsm-container-service)
            BSM_CONTAINER_SERVICE="$2"
            shift 2
            ;;
        *)
            echo "Invalid option: $1"
            print_help
            exit 1
            ;;
    esac
done

# Check if bsm-container-service is given
if [ -z "$BSM_CONTAINER_SERVICE" ]; then
    echo "Error: --bsm-container-service es obligatorio."
    print_help
    exit 1
fi


####### Params y Vars ---------------------------------------

# Parece que a veces fallan los cuadernos porque no consigue alocar suficientes recursos. 
# En ese caso editar las variables *_MIN en envvars_defaults.sh.
source ./envvars_defaults.sh

echo "Using BSM_CONTAINER_SERVICE: $BSM_CONTAINER_SERVICE"
echo "Using BSM_USER: $BSM_USER"
echo "Using BSM_DIR: $BSM_DIR"

## Cambio el directorio the trabajo a la dirección de este script. 
# Los paths se convierten en relativos al script. 
# echo "The current working directory is: $(pwd)"
# cd $(dirname "$0")

# Si se proporciona USER_ID se usa para construir BSM_NAME
echo "Inicio Tablero de IVs"

docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE \
  bash -c "Rscript $BSM_DIR/Tableros/Explorador_IVs_API_run.R"
exit_status_1=$?
docker exec --user $BSM_USER --workdir $BSM_DIR $BSM_CONTAINER_SERVICE Rscript -e "print('Chau R')"
exit_status_2=$?    
if [ $exit_status_1 -ne 0 ] || [ $exit_status_2 -ne 0 ]; then 
  echo "Error: Fin del Tablero Explorador_IVs_API con fallas!"
  exit 1; 
fi    
echo "Fin Tablero de IVs"

echo "*****************************************************************************"
echo "Fin de proceso de Tablero de IVs sin errores detectados."
echo "*****************************************************************************"
