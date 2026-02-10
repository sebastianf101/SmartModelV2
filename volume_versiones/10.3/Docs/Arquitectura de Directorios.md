# Arquitectura de Directorios - SmartModel

## Estructura de Directorios

SmartModel usa una arquitectura con copia local para facilitar el desarrollo:

### 1. Espacio de Trabajo del Usuario (Read-Write)
**Ubicación**: `~/Documents/besmart/{version}/`

```
~/Documents/besmart/10.3/
├── Librerias/              # COPIA LOCAL EDITABLE
│   ├── Funciones auxiliares.R
│   ├── Funciones logging.R
│   ├── Setup.R
│   ├── Utils Tejido.R
│   └── Utils Pres and Reports.R
├── Cuadernos/              # Copia local editable
├── Scripts/                # Copia local editable
├── Params/                 # Parámetros del usuario
├── Datos/                  # Datos del usuario
├── Logs/                   # LOGS DE SESIÓN (creado automáticamente)
│   ├── smartmodel_20260122.log
│   ├── smartmodel_20260122_143022.log.zip
│   └── progress.json
├── Trabajo/                # Archivos de trabajo (RDS, CSV, etc.)
├── Reportes/               # Notebooks renderizados (HTML)
└── Auxil/                  # Archivos auxiliares/temporales
    ├── Interm/
    └── Cache/
```

**Variable de entorno**: `bsm_path`

Características:
- ✅ Read-write (directorio del usuario)
- ✅ Aislado por usuario
- ✅ Librerias pueden modificarse localmente para desarrollo
- ✅ Logs y trabajos se guardan aquí
- ✅ Puede modificar notebooks y código localmente

### 2. Versiones Base (Read-Only)
**Ubicación**: `/var/data/besmart/versiones/{version}/`

```
/var/data/besmart/versiones/10.3/
├── Cuadernos/
├── Scripts/
├── Params/
├── Datos/
├── Docs/
├── Tableros/
└── Librerias/
```

Características:
- ✅ Read-only (montado desde volumen Docker)
- ✅ Fuente para copias iniciales
- ✅ Versionado (10.3, 10.4, etc.)

## Configuración en RProfile.site

El archivo `/etc/R/Rprofile.site` configura la ruta local:

```r
# Variables de entorno
Sys.getenv("BSM_VERSION") -> version_bsm

# Ruta a espacio de trabajo del usuario (READ-WRITE)
fs::path("~/Documents/besmart", version_bsm) -> bsm_path

# Crear directorios de trabajo si no existen
bsm_path |> fs::dir_create("Logs")      # ← IMPORTANTE para logging
bsm_path |> fs::dir_create("Auxil")
bsm_path |> fs::dir_create("Reportes")
bsm_path |> fs::dir_create("Trabajo")

# Establecer directorio de trabajo
fs::path(bsm_path) |> setwd()

# Cargar librerías LOCALES desde bsm_path
sys.source(fs::path(bsm_path, "Librerias/Utils Tejido.R"), envir = globalenv())
sys.source(fs::path(bsm_path, "Librerias/Funciones auxiliares.R"), envir = globalenv())
sys.source(fs::path(bsm_path, "Librerias/Utils Pres and Reports.R"), envir = globalenv())
```

## Flujo de Carga

### 1. Al iniciar R/Positron
```
RProfile.site ejecuta:
  ↓
Define bsm_path (~/Documents/besmart/10.3)
  ↓
Crea directorios en bsm_path (Logs, Trabajo, etc.)
  ↓
Establece bsm_path como directorio de trabajo
```

### 2. Al renderizar un notebook
```
Notebook (ej: Modelling.qmd)
  ↓
CargaParam chunk ejecuta:
  fs::path(bsm_path, "Scripts/Carga Parametros.R") |> source()
  ↓
Carga Parametros.R ejecuta:
  sys.source(fs::path(bsm_path, "Librerias/Setup.R"))
  ↓
Setup.R ejecuta:
  source(fs::path(bsm_path, "Librerias/Funciones logging.R"))
  log_init()  # Crea logs en bsm_path/Logs/
```

## Sistema de Logging

### Ubicación de Archivos

| Componente | Ubicación | Tipo |
|------------|-----------|------|
| `Funciones logging.R` | `bsm_path/Librerias/` | READ-WRITE |
| `smartmodel_*.log` | `bsm_path/Logs/` | READ-WRITE |
| `progress.json` | `bsm_path/Logs/` | READ-WRITE |

### Configuración Docker

```yaml
environment:
  BSM_VERSION: "10.3"
  BSM_DIR: "~/Documents/besmart/10.3"  # = bsm_path en contenedor
  BSM_LOG_DIR: "${BSM_DIR}/Logs"
  PROGRESS_JSON_PATH: "${BSM_LOG_DIR}/progress.json"

volumes:
  # Librerías y contenido versionado (read-only)
  - /var/data/besmart/versiones:/var/data/besmart/versiones:ro

  # Logs del usuario (read-write, expuesto al host)
  - ${BSM_LOGS_HOST_DIR:-/tmp/sm-logs}:${BSM_DIR}/Logs:rw
```

## Inicialización de Usuarios

El script `Init_users.sh` prepara el espacio de trabajo para nuevos usuarios:

```bash
# Para cada usuario con UID 1000-1999
for u in $_USERS; do
    # Copiar versión completa al workspace
    /bin/cp -rf /var/data/besmart/versiones/10.3/. ~/Documents/besmart/10.3/

    # Crear directorios de trabajo
    mkdir -p ~/Documents/besmart/10.3/Logs
    mkdir -p ~/Documents/besmart/10.3/Trabajo
    mkdir -p ~/Documents/besmart/10.3/Reportes
    mkdir -p ~/Documents/besmart/10.3/Auxil

    # Ajustar permisos
    chown -R $u:$u ~/Documents/besmart/
done
```

## Referencias a Variables en Código

### ✅ CORRECTO
```r
# Cargar librerías desde bsm_path (ahora editables localmente)
source(fs::path(bsm_path, "Librerias/Funciones logging.R"))

# Guardar archivos de trabajo en bsm_path
log_file <- fs::path(bsm_path, "Logs/smartmodel.log")
saveRDS(data, fs::path(bsm_path, "Trabajo/datos.rds"))
```

## Ventajas de esta Arquitectura

1. **Desarrollo facilitado**
   - Código editable en `bsm_path/Librerias/`
   - Cada usuario puede experimentar sin afectar a otros
   - Fácil debugging y modificación local

2. **Backup centralizado**
  - `/var/data/besmart/versiones/{version}` mantiene plantilla original
  - Fácil restaurar desde versión base
  - Versionado claro

3. **Aislamiento de usuarios**
   - Cada usuario tiene su propia copia completa
   - Logs y trabajos no se mezclan entre usuarios
   - Modificaciones no afectan a otros

4. **Flexibilidad**
   - Usuarios pueden modificar librerías localmente
  - Actualizaciones globales via `/var/data/besmart/versiones/{version}` → re-copy
   - Fácil rollback a versión anterior

5. **Monitoreo externo**
   - Logs expuestos via volumen Docker
   - Fácil integración con dashboards

## Troubleshooting

### Error: "No se puede escribir en Librerias/"
**Causa**: Permisos incorrectos en `bsm_path/Librerias/`
**Solución**:
```bash
chmod -R u+w ~/Documents/besmart/10.3/Librerias
```

### Error: "No existe directorio Logs/"
**Causa**: `Init_users.sh` no se ejecutó o falló
**Solución**:
```bash
mkdir -p ~/Documents/besmart/10.3/Logs
```

### Logs no aparecen
**Causa**: `BSM_LOG_DIR` apunta a ubicación incorrecta
**Verificar**:
```r
Sys.getenv("BSM_LOG_DIR")
# Debe ser: ~/Documents/besmart/10.3/Logs
```

### Librerías no se cargan
**Causa**: Librerias no copiadas o `bsm_path` no definido
**Verificar**:
```r
fs::dir_exists(fs::path(bsm_path, "Librerias"))
# Debe ser: TRUE
```
**Solución**: Re-ejecutar `Init_users.sh` o copiar manualmente

### Actualizar Librerias desde plantilla base
**Para actualizar una librería específica**:
```bash
cp /var/data/besmart/versiones/10.3/Librerias/Setup.R ~/Documents/besmart/10.3/Librerias/
```

**Para actualizar todas las librerías**:
```bash
cp -rf /var/data/besmart/versiones/10.3/Librerias/* ~/Documents/besmart/10.3/Librerias/
```

## Migración de Código Existente

Si tienes código que usa rutas a `/var/data/besmart/versiones`, cámbialo a `bsm_path`:

**Después**:
```r
source(fs::path(bsm_path, "Librerias/Funciones logging.R"))
```

Todos los archivos de trabajo ya deben estar usando `bsm_path`:
```r
# CORRECTO - archivos de trabajo
saveRDS(modelo, fs::path(bsm_path, "Trabajo/modelo.rds"))
write.csv(datos, fs::path(bsm_path, "Datos/datos.csv"))
```
