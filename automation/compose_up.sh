#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./compose_up.sh bsm            # base (Rocker default)
#   ./compose_up.sh api            # base + API override (runs Rscript)
#   ./compose_up.sh ./custom.yml   # use a custom compose file
MODE="${1:-bsm}"

die() { return 1 2>/dev/null || exit 1; }

case "$MODE" in
  api|--api)
    COMPOSE_ARGS=(-f ./config-contenedor-bsm.yml -f ./config-contenedor-api.override.yml)
    ;;
  bsm|--bsm)
    COMPOSE_ARGS=(-f ./config-contenedor-bsm.yml)
    ;;
  *.yml|*.yaml)
    COMPOSE_ARGS=(-f "$MODE")
    ;;
  -h|--help)
    echo "Usage: $0 [bsm|api|compose-file.yml]"; exit 0 ;;
  *)
    echo "Unknown argument: $MODE"; die ;;
esac

# The container has prefix sm-cont-
export BSM_CONTAINER="sm-cont-${BSM_NAME}"
export BSM_CONTAINER_SERVICE="sm-svc-${BSM_NAME}"

# Pre-create log directory with correct ownership to avoid permission issues
mkdir -p "${BSM_LOGS_HOST_DIR}"
if [ "$(id -u)" -eq 0 ]; then
  chown 1000:1000 "${BSM_LOGS_HOST_DIR}"
else
  # If not root, try to ensure it's writable by the current user
  chmod 775 "${BSM_LOGS_HOST_DIR}" 2>/dev/null || true
fi

echo "Using compose files: ${COMPOSE_ARGS[*]}"
echo "Variables assigned in the session"
printenv | grep "BSM" | grep -v "BSM_PWD" || true

echo "Starting ephemeral SM container ${BSM_CONTAINER} (SSH on localhost:${BSM_SSH_PORT})"
docker compose "${COMPOSE_ARGS[@]}" up --remove-orphans --detach --wait --wait-timeout 30 || die

docker exec --user "${BSM_USER}" --workdir "${BSM_DIR}" "$BSM_CONTAINER_SERVICE" \
  Rscript -e "if (!exists('error_custom', mode = 'function')) { cat('error_custom not found\n'); quit(status=1) }" || {
  echo "Pooling_RServer failed! Libraries or R not available!"; die; }

docker exec --user "${BSM_USER}" --workdir "${BSM_DIR}" "$BSM_CONTAINER_SERVICE" \
  Rscript -e "if (fs::path_real(bsm_path) != getwd()) { error_custom('R setup failed!'); quit(status=1) }" || {
  echo "Pooling_RServer failed! SM directories not available in R!"; die; }

echo "Hello SmartModel ${BSM_CONTAINER} at localhost:${BSM_SSH_PORT} (SSH)"
