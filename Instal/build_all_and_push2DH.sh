#! /bin/bash
# Bash script para construir imagen desde cero y salvarla
# Pararse en el directorio raiz del proyecto
cd /mnt/c/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelStudioV1/
echo "Login a docker.io"
docker login  -u "sebastianf101" -p "Cuanto tiempo nos queda para vivir" docker.io
echo "Inicio construcción de imagen bsm-studio-paq:v2.7"
docker build -t bsm-studio-paq -f Instal/Dockerfile.bsm-studio-paq.v2 --no-cache .
docker tag bsm-studio-paq sebastianf101/bsm-studio-paq:v2.7
docker push sebastianf101/bsm-studio-paq:v2.7
echo "Inicio construcción de imagen bsm-studio:v10.3"
docker build -t bsm-studio-ssh -f Instal/Dockerfile.bsm-studio.v2 --progress=plain --no-cache .
docker tag bsm-studio-ssh sebastianf101/bsm-studio:v10.3
docker push sebastianf101/bsm-studio:v10.3
docker save sebastianf101/bsm-studio:v10.3 | gzip -9 > Instal/bsm-studio-v10.3.tar.gz
echo "Fin de construcción de imagen"


