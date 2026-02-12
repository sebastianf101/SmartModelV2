#!/usr/bin/env bash
# Pull SmartModel image from GitHub Container Registry.
# Required env vars: IMAGE_VERSION, GHCR_PULL_PWD (or will prompt).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${SCRIPT_DIR}/.."

# Prompt for missing env vars
check_and_set_env_var() {
    local var_name=$1
    if [ -z "${!var_name:-}" ]; then
        read -rp "Enter value for ${var_name}: " value
        export "${var_name}=${value}"
    fi
}

check_and_set_env_var "IMAGE_VERSION"
check_and_set_env_var "GHCR_PULL_PWD"

echo "Pulling ghcr.io/sferro-besmart/smartmodelv2:${IMAGE_VERSION}"
echo "${GHCR_PULL_PWD}" | docker login -u sferro-besmart --password-stdin ghcr.io
docker pull "ghcr.io/sferro-besmart/smartmodelv2:${IMAGE_VERSION}"
docker tag "ghcr.io/sferro-besmart/smartmodelv2:${IMAGE_VERSION}" bsm-studio
docker logout ghcr.io
echo "Done."
