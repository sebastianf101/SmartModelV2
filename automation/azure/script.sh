#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Uso: $0 <tag>" >&2
  exit 1
fi

TAG="$1"
IMAGE="ghcr.io/fedev23/bsm_with_az:${TAG}"

echo "Construyendo imagen ${IMAGE}..."
docker build -t "${IMAGE}" .

echo "Publicando ${IMAGE}..."
docker push "${IMAGE}"

echo "Actualizando Azure Container Apps job sm-run-models2..."
az containerapp job update -n sm-run-models2 -g SMApp \
  --container-name sm-run-models2 \
  --image "${IMAGE}"
