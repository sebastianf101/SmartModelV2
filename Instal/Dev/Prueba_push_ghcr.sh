#! /bin/bash
echo "Inicio subida a GitHub Container Registry"
echo $GHCR_PUSH_PWD | docker login -u sferro-besmart --password-stdin ghcr.io 
docker tag bsm-studio-ssh ghcr.io/sferro-besmart/smartmodelv2:v10.3.0
docker push ghcr.io/sferro-besmart/smartmodelv2:v10.3.0
docker logout ghcr.io
echo "Fin subida a GitHub Container Registry"



