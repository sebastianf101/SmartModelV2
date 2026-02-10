#!/usr/bin/env bash
set -euo pipefail

JOB_NAME="${JOB_NAME:-sm-run-models2}"
RESOURCE_GROUP="${RESOURCE_GROUP:-SMApp}"

if [[ $# -ne 1 ]]; then
  echo "Usage: $(basename "$0") <job-execution-id>" >&2
  echo "Env overrides: JOB_NAME, RESOURCE_GROUP" >&2
  exit 1
fi

EXECUTION_ID="$1"

az containerapp job stop \
  --name "$JOB_NAME" \
  --resource-group "$RESOURCE_GROUP" \
  --job-execution-name "$EXECUTION_ID"
