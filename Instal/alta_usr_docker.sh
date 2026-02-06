#!/bin/bash
# Con el contenedor rstudio_sm levantado se pueden incorporar usuarios nuevos
docker exec sm addgroup --gid 1100 admin 
docker exec sm addgroup --gid 1200 consultores
docker exec sm addgroup --gid 1300 consultores_ext 
docker exec sm addgroup --gid 2000 compartido
docker cp users.conf.newusers sm:/etc/security/
docker exec sm chmod 0600 //etc/security/users.conf.newusers
docker exec sm newusers //etc/security/users.conf.newusers
