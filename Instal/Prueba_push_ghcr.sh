#! /bin/bash
# Bash script para construir imagen desde cero y salvarla
echo "Inicio subida a GitHub Container Registry"
docker tag bsm-studio-ssh ghcr.io/sferro-besmart/smartmodelv2:v10.3.0
docker push ghcr.io/sferro-besmart/smartmodelv2:v10.3.0
echo "Fin subida a GitHub Container Registry"
#docker save sebastianf101/bsm-studio:v10.3 | gzip -9 > Instal/bsm-studio-v10.3.tar.gz




