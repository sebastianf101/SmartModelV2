#! /bin/bash
# Bash script para construir imagen desde cero y salvarla
# Setear antes las env vars con los paswords!
# Para detenerse al mínimo error.
set -e
set -o pipefail

# Dynamically determine the project root (parent of the script's directory)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"
echo "Directorio de trabajo: $(pwd)"

# Function to check and prompt for env var
check_and_set_env_var() {
    local var_name=$1
    local var_value

    eval var_value=\$"$var_name"

    if [ -z "$var_value" ]; then
        read -p "Enter value for $var_name: " var_value
        export "$var_name"="$var_value"
    fi
}

# Check and prompt for env vars.
check_and_set_env_var "IMAGE_VERSION"
check_and_set_env_var "DOCKER_PWD"
check_and_set_env_var "GHCR_PUSH_PWD"

echo "Versión de Imagen: $IMAGE_VERSION"
echo "Inicio construcción de imagen bsm-studio-paq"
docker build -t bsm-studio-paq -f docker/Dockerfile.base . #--no-cache
docker tag bsm-studio-paq sebastianf101/bsm-studio-paq
echo "Fin de construcción de imagen bsm-studio-paq"
echo "Inicio construcción de imagen bsm-studio"
docker build -t bsm-studio -f docker/Dockerfile.config --progress=plain . #--no-cache
echo "Fin de construcción de imagen bsm-studio"
echo "Inicio subida a docker Hub de imagen sebastianf101/bsm-studio:$IMAGE_VERSION"
echo $DOCKER_PWD | docker login -u "sebastianf101" --password-stdin docker.io
docker tag bsm-studio sebastianf101/bsm-studio:$IMAGE_VERSION
docker push sebastianf101/bsm-studio:$IMAGE_VERSION
docker logout docker.io
echo "Fin subida a docker Hub"
echo "Inicio subida a GitHub Container Registry de imagen ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION"
echo $GHCR_PUSH_PWD | docker login -u sferro-besmart --password-stdin ghcr.io
docker tag bsm-studio ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION
docker push ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION
docker logout ghcr.io
echo "Fin subida a GitHub Container Registry"



