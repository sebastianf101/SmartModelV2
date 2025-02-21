#! /bin/bash
# Asume que corre desde el directorio Instal
source ./Automat/envvars_defaults.sh
echo "Inicio bajada de imagen $BSM_IMAGE"
echo $GHCR_PULL_PWD | docker login -u sferro-besmart --password-stdin ghcr.io 
docker pull $BSM_IMAGE
docker logout ghcr.io
echo "Fin bajada de imagen $BSM_IMAGE"



