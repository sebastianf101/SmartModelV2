# Valores por defecto de Variables de entorno permanentes.
# Cambiarlos sólo si se sabe que hacen!
#
export BSM_USER="user"
export BSM_VERSION="10.3"
export BSM_DIR="/home/$BSM_USER/Documents/besmart/$BSM_VERSION"
export BSM_IMAGE="bsm-studio"
export BSM_RETRIES=1
export BSM_PWD="smartmodel"
export BSM_CPU_CORES_MIN="2"
export BSM_MEMORY_MIN="4G"
export BSM_YML="bsm"
export BSM_NAME="Mi_SM"
export BSM_PORT="3000" # Obsoleto (RStudio removido)
export BSM_DASHBOARD_PORT="3001"
export BSM_SSH_PORT="2222"
# Logs directory (inside container and for env var)
export BSM_LOG_DIR="${BSM_DIR}/Logs"
# Logs directory on host (mounted to ${BSM_LOG_DIR} inside container)
export BSM_LOGS_HOST_DIR="/tmp/sm-logs-${BSM_NAME}"

