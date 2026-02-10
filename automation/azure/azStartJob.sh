#!/usr/bin/env bash
set -euo pipefail
JOB_NAME="${JOB_NAME:-sm-run-models2}"
RESOURCE_GROUP="${RESOURCE_GROUP:-SMApp}"
CONTAINER_NAME="${CONTAINER_NAME:-$JOB_NAME}"
TAIL="${TAIL:-50}"
FORMAT="${FORMAT:-text}"
if [[ $# -ne 1 ]]; then
  echo "Usage: $(basename "$0") <exec-yaml>" >&2
  echo "Env overrides: JOB_NAME, RESOURCE_GROUP, CONTAINER_NAME, TAIL, FORMAT" >&2
  exit 1
fi
EXEC_YAML="$1"
if [[ ! -f "$EXEC_YAML" ]]; then
  echo "Error: YAML not found: $EXEC_YAML" >&2
  exit 1
fi
EXECUTION_ID="$(az containerapp job start \
  -n "$JOB_NAME" \
  -g "$RESOURCE_GROUP" \
  --yaml "$EXEC_YAML" \
  --query "name" \
  -o tsv)"
if [[ -z "$EXECUTION_ID" ]]; then
  echo "Error: could not get execution id from az start output" >&2
  exit 1
fi
az containerapp job logs show \
  -n "$JOB_NAME" \
  -g "$RESOURCE_GROUP" \
  --container "$CONTAINER_NAME" \
  --execution "$EXECUTION_ID" \
  --follow \
  --tail "$TAIL" \
  --format "$FORMAT"
