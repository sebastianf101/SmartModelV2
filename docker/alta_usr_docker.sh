#!/bin/bash
# Con el contenedor sm levantado se puede recrear el usuario base
docker exec sm addgroup --gid 1000 consultores_ext
docker cp users.conf.newusers sm:/etc/security/
docker exec sm chmod 0600 //etc/security/users.conf.newusers
docker exec sm newusers //etc/security/users.conf.newusers
