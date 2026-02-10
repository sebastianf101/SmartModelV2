---
title: "Instalación de Smart Model"
subtitle: "versión 10.3"
---

### 1. Requisitos

Instalar Docker Desktop. Docker corre en Linux. En Windows se instala en WSL2.

Archivo necesarios:

1.  Imagen docker: bsm-studio-vXX.tar.gz

2.  Configuracion del contenedor: config-contenedor-bsm.yml

3.  Scripts sh: envvars_setting.sh, Modelar.sh, Validar_Desa.sh, Validar_Nueva.sh

Copiar los archivos de \\datasrv\\Proyectos\\Smart Model\\ a un directorio de Trabajo, abrir una terminal Linux allí y verificar que los .sh tengan permisos de ejecución.

### 3. Levantar y bajar el contenedor persistente

Con los comandos

``` bash
./Levantar_Contenedor.sh
./Bajar_Contenedor.sh
```

Levantamos o bajamos el contenedor.

Podemos verificar que está subido con el acceso al tablero en el puerto configurado (por defecto 3001) o por SSH (último recurso).

No es necesario loguearse. Podemos controlar todo el trabajo con estos dos scripts más el siguiente script.

Tampoco el necesario bajar el contenedor por cada trabajo.

### 4. Levantar y Bajar Contenedor efímero

Primero generamos un puerto y nombre de contenedor al azar y las almacenamos en variables de entorno de la *sesión* con el comando.

``` bash
source ./envvars_efimeras.sh
```

Luego levantamos o bajamos el contenedor con los comandos

``` bash
./Levantar_Efimero.sh ./Bajar_Efimero.sh
```

Cada vez que se baja el contenedor se pierden los datos a diferencia del modo persistente.

Notar que se pueden tener varias réplicas del contenedor corriendo simultaneamente, cada una con un nombre y puerto diferentes.

Estos comandos también aceptan los parámetros \[BSM_NAME\] \[BSM_PORT\] para sobreescribir las variables de entorno. Para bajar el servicio sólo basta el nombre sin prefijos del contenedor.

```
./Levantar_Efimero [BSM_NAME] [BSM_PORT]
./Bajar_Efimero [BSM_NAME]
```

Se pueden tener múltiples contenedores efímeros corriendo simultáneamente en background.

De manera local se acceden con un browser en la dirección localhost:BSM_DASHBOARD_PORT.

### 5. Tejer Cuadernos

Con el comando

``` bash
./Tejer_Cuadernos.sh Cuaderno Parametros Datos
```

copiamos los archivos de Parámetros y Datos dentro del contenedor y tejemos/ejecutamos los Cuadernos.

Al usar el flag -h o –help

```
Uso: ./Tejer_Cuadernos.sh Cuaderno Parametros Datos
  - Si se provee Datos se los copia a Data/.
  - Si se provee Parametros se los copia a Param/.
  - Sin argumentos ejecuta Tejer Cuadernos.R
  - Sino se ejecuta Cuaderno
```

Los Cuadernos disponibles son:

Cuadernos/Clean-Transf_v10.qmd

Cuadernos/Modelling_v10.qmd

Cuadernos/Validation_v10.qmd

Cuadernos/Validation_OoS_v10.qmd

Los paths son relativos al directorio de trabajo dentro del Contenedor.

Si se proveen los archivos de Parámetros o Datos primero se los copia a las carpetas respectivas.

Luego se ejecuta el cuaderno. Sino se usa el script interno Tejer Cuadernos.R que teje estos 4 cuadernos.

Esta última opción puede tardar muchos minutos.

Este script devuelve dos archivos de salida: results.out y errors.out además de mensajes de pantalla.

Si el proceso termina bien, se copian los reportes obtenidos desde el Contenedor al subdirectorio Output dentro del directorio de Trabajo actual.

En Rstudio se pueden procesar los cuadernos y revisar los resultados de manera más interactiva.

### 6. Ejemplos

``` bash
./Tejer_Cuadernos.sh 'Cuadernos/Clean-Transf_v10.qmd' 'Control de SmartModelStudio.xlsx' 'set_Cliente_Conocido.txt'
```

Revisar mensajes, results.out, errors.out y salida en Output/.
