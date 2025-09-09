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

echo "Using compose files: ${COMPOSE_ARGS[*]}"
echo "Variables assigned in the session"
printenv | grep "BSM" | grep -v "BSM_PWD" || true

echo "Starting ephemeral SM container ${BSM_CONTAINER} on localhost:${BSM_PORT}"
docker compose "${COMPOSE_ARGS[@]}" up --remove-orphans --detach --wait --wait-timeout 30 || die

# Poll RStudio Server
until docker exec "$BSM_CONTAINER_SERVICE" wget -qO- http://localhost:8787 >/dev/null 2>&1; do
  echo "Waiting for RStudio Server..."
  sleep 1
done

docker exec --user "${BSM_USER}" --workdir "${BSM_DIR}" "$BSM_CONTAINER_SERVICE" \
  Rscript -e "stopifnot(exists('error_custom', mode = 'function'))" || {
  echo "Pooling_RServer failed! Libraries or R not available!"; die; }

docker exec --user "${BSM_USER}" --workdir "${BSM_DIR}" "$BSM_CONTAINER_SERVICE" \
  Rscript -e "if (fs::path_real(bsm_path) != getwd()) error_custom('R setup failed!')" || {
  echo "Pooling_RServer failed! SM directories not available in R!"; die; }

echo "Hello SmartModel ${BSM_CONTAINER} at localhost:${BSM_PORT}"
