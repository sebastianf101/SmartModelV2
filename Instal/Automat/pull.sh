#! /bin/bash
# Bash script para construir imagen desde cero y salvarla
# Setear antes las env vars con los paswords!
# Para detenerse al mínimo error.
set -e
set -o pipefail

# Ojo asegurarse de que estoy parado en el siguiente cd sea el directorio raiz del proyecto!!!
cd /c/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelV2/
#

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
check_and_set_env_var "GHCR_PULL_PWD"

echo "Versión de Imagen: $IMAGE_VERSION"
#echo $DOCKER_PWD | docker login  -u "sebastianf101" --password-stdin docker.io 
#docker tag bsm-studio sebastianf101/bsm-studio:$IMAGE_VERSION
#docker logout docker.io
echo "Inicio bajada de GitHub Container Registry de imagen ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION"
echo $GHCR_PULL_PWD  | docker login -u sferro-besmart --password-stdin ghcr.io 
docker pull ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION
docker tag ghcr.io/sferro-besmart/smartmodelv2:$IMAGE_VERSION bsm-studio 
docker logout ghcr.io
echo "Fin bajada de GitHub Container Registry"



