# Sistema de Logging de SmartModel

## Descripción General

SmartModel implementa un sistema de logging completo y estructurado que sigue las mejores prácticas de R, con soporte para:

- **Niveles estándar de log**: TRACE, DEBUG, INFO, WARN, ERROR, FATAL
- **Rotación automática**: Por tamaño (10MB) y por semana
- **Contexto de sesión**: Session ID único, timestamp, notebook actual
- **Separación batch/interactivo**: En modo batch no se muestra en consola (para no contaminar cuadernos renderizados)
- **Integración con progress.json**: Progress tracking con session ID

## Estructura de Archivos

```
# User workspace (bsm_path)
~/Documents/besmart/10.3/
├── Logs/
│   ├── smartmodel_20260122.log          # Log del día actual
│   ├── smartmodel_20260122_143022.log.zip # Log rotado y comprimido
│   └── progress.json                     # Estado de progreso actual
├── Trabajo/                              # Working files
├── Reportes/                             # Rendered reports
└── Auxil/                                # Auxiliary files

# Shared libraries (version_path - read-only)
/var/data/besmart/versiones/10.3/
└── Librerias/
    └── Funciones logging.R               # Sistema de logging
```

## Inicialización

El sistema se inicializa automáticamente en `Setup.R`:

```r
# Se carga en Setup.R (from shared version_path)
source(fs::path(version_path, "Librerias/Funciones logging.R"))

# Inicialización automática con detección de modo
.bsm_session_id <- log_init(
  is_batch = !interactive(),     # Auto-detecta modo
  min_level_console = "INFO",    # Mínimo en consola
  min_level_file = "DEBUG"       # Mínimo en archivo
)
```

## Configuración

### Variables de Entorno

- `BSM_LOG_DIR`: Directorio de logs (default: `~/Documents/besmart/10.3/Logs`)
- `PROGRESS_JSON_PATH`: Ruta del archivo progress.json (default: `${BSM_DIR}/Logs/progress.json`)

### Docker Configuration

En `config-contenedor-bsm.yml`:

```yaml
environment:
  BSM_LOG_DIR: "${BSM_DIR}/Logs"
  PROGRESS_JSON_PATH: "${BSM_DIR}/Logs/progress.json"
```

## Uso

### Funciones de Logging

```r
# Niveles de log disponibles
log_trace("Mensaje muy detallado")                    # Nivel 0 - Tracing detallado
log_debug("Variable x = {x}")                         # Nivel 1 - Debugging
log_info("Proceso iniciado")                          # Nivel 2 - Información general
log_warn("Advertencia: {mensaje}")                    # Nivel 3 - Advertencias
log_error("Error en proceso", code = "123")           # Nivel 4 - Errores
log_fatal("Error crítico irrecuperable", code = "999") # Nivel 5 - Fatales

# Interpolación automática con cli
log_info("Procesando {nrow(df)} registros en {archivo}")

# Con códigos de error
log_error("Validación falló", code = "202")
```

### Contexto de Notebook

```r
# Al iniciar un notebook (en Tejer Cuadernos.R)
log_set_notebook("Modelling.qmd")
"Cuadernos/Modelling.qmd" |> quarto::quarto_render()
log_clear_notebook()

# Dentro del notebook, los logs incluirán automáticamente el nombre
log_info("Iniciando modelado")  # Se loguea con contexto "Modelling.qmd"
```

### Funciones Heredadas Actualizadas

Las funciones existentes ahora loguean automáticamente **solo al archivo**:

```r
# error_custom: loguea a ERROR antes de abortar
# NO produce output en consola durante renderizado
error_custom("Proceso falló!",
             "i" = "Detalles adicionales",
             ">"=cli::col_red("Cod 123"))

# msg_custom: loguea a INFO
# NO produce output en consola durante renderizado
msg_custom("Mensaje importante")
```

**Importante**: Estas funciones fueron modificadas para que **no generen output en notebooks renderizados**.
Todo el logging se hace únicamente al archivo de log, manteniendo los notebooks limpios.

### Captura de Errores

```r
# Wrapper para capturar y loguear errores
log_catch(
  {
    resultado <- funcion_riesgosa()
  },
  context = "Procesamiento de datos",
  code = "301"
)
```

## Formato de Log

### Archivo de Log

Cada línea sigue el formato:

```
[TIMESTAMP] [LEVEL] [SESSION_ID] [NOTEBOOK] MESSAGE (Cod: CODE)
```

Ejemplo:

```
[2026-01-22 14:30:22.456] [INFO] [20260122_143022_abc123] [Modelling.qmd] Iniciando modelado
[2026-01-22 14:31:05.123] [WARN] [20260122_143022_abc123] [Modelling.qmd] Variable X tiene 5 missings
[2026-01-22 14:35:11.789] [ERROR] [20260122_143022_abc123] [Modelling.qmd] Convergencia falló (Cod: 202)
```

### Progress.json

```json
{
  "session_id": "20260122_143022_abc123",
  "progress": 0.45,
  "current_step": 3,
  "time_elapsed": 125.3,
  "time_remaining": 152.7,
  "timestamp": "2026-01-22 14:32:15.456",
  "notebook": "Modelling.qmd"
}
```

## Rotación de Logs

### Automática

- **Por tamaño**: Cuando un archivo supera 10MB se rota automáticamente
- **Por fecha**: Un archivo nuevo por día (`smartmodel_YYYYMMDD.log`)
- **Compresión**: Los archivos rotados se comprimen automáticamente (.zip)

### Manual

```r
# Limpiar logs antiguos (>7 días)
source("Scripts/Limpiar Logs.R")

# O programáticamente
log_cleanup_old(log_dir = "~/Documents/besmart/10.3/Logs", days = 7)
```

## Comportamiento por Modo

### Modo Interactivo (`interactive() == TRUE`)

- **Consola**: Muestra mensajes >= INFO con formato `cli`
- **Archivo**: Guarda mensajes >= DEBUG en el archivo de log
- **Progress.json**: Se actualiza normalmente

### Modo Batch (`!interactive()`)

- **Consola**: NO muestra mensajes (evita contaminar cuadernos renderizados)
- **Archivo**: Guarda TODOS los mensajes >= DEBUG
- **Progress.json**: Se actualiza para monitoreo externo

## Session ID

Cada ejecución recibe un Session ID único con formato:

```
YYYYMMDD_HHMMSS_HASH
Ejemplo: 20260122_143022_abc123
```

Este ID permite:
- Rastrear una ejecución completa en los logs
- Correlacionar progress.json con logs
- Identificar múltiples ejecuciones paralelas

Obtener el Session ID actual:

```r
session_id <- log_get_session_id()
```

## Monitoreo Externo

### Progress Tracking

El archivo `progress.json` puede monitorearse externamente (ej: dashboards Docker):

```bash
# Leer progreso actual
cat ~/Documents/besmart/10.3/Logs/progress.json

# Monitorear en tiempo real
watch -n 1 'cat ~/Documents/besmart/10.3/Logs/progress.json | jq .'
```

### Análisis de Logs

```bash
# Ver logs del día
tail -f ~/Documents/besmart/10.3/Logs/smartmodel_20260122.log

# Filtrar por nivel
grep "\\[ERROR\\]" smartmodel_20260122.log

# Filtrar por session ID
grep "20260122_143022_abc123" smartmodel_20260122.log

# Contar errores por código
grep -oP "Cod: \\K[0-9]+" smartmodel_20260122.log | sort | uniq -c
```

## Mejores Prácticas

### 1. Usar niveles apropiados

```r
log_trace("Iteración {i} de {n}")           # Solo para debugging profundo
log_debug("Variables: x={x}, y={y}")        # Debugging de valores
log_info("Proceso completado en {t}s")      # Progreso normal
log_warn("Valor atípico detectado: {v}")    # Situaciones inusuales
log_error("Falló validación", code="123")   # Errores recuperables
log_fatal("Datos corruptos", code="999")    # Errores irrecuperables
```

### 2. Usar interpolación de cli

```r
# BUENO: Interpolación automática
log_info("Procesando {length(files)} archivos")

# EVITAR: Concatenación manual
log_info(paste("Procesando", length(files), "archivos"))
```

### 3. Incluir códigos de error

```r
# BUENO: Con código para búsqueda fácil
log_error("Validación falló", code = "202")

# ACEPTABLE: Sin código para warnings menores
log_warn("Variable {var} tiene NAs")
```

### 4. Establecer contexto de notebook

```r
# Siempre en scripts que ejecutan notebooks
log_set_notebook("Modelling.qmd")
# ... renderizar notebook ...
log_clear_notebook()
```

### 5. Limpiar logs periódicamente

```bash
# Agregar a crontab para limpieza semanal
0 2 * * 0 Rscript ~/Documents/besmart/Scripts/Limpiar\ Logs.R
```

## Troubleshooting

### Los logs no aparecen en archivo

1. Verificar que existe el directorio: `~/Documents/besmart/10.3/Logs/`
2. Verificar permisos de escritura
3. Verificar variable `BSM_LOG_DIR`

### Los mensajes aparecen en notebooks renderizados

- Verificar que está en modo batch: `!interactive()` debe ser `TRUE`
- El sistema detecta esto automáticamente al ejecutar con `quarto::quarto_render()`

### Progress.json no se actualiza

1. Verificar `PROGRESS_JSON_PATH` está configurado
2. Verificar permisos de escritura en directorio
3. Revisar logs para mensajes de error al escribir progress.json

### Session ID es NULL

- Llamar manualmente `log_init()` si no se ejecutó Setup.R

## Migración desde Sistema Anterior

### Código Anterior

```r
message("Proceso iniciado")
cli::cli_alert_info("Procesando...")
cli::cli_alert_warning("Advertencia!")
warning("Problema detectado")
```

### Código Nuevo

```r
log_info("Proceso iniciado")
log_info("Procesando...")
log_warn("Advertencia!")
log_warn("Problema detectado")
```

### Compatibilidad

Las funciones `error_custom()` y `msg_custom()` siguen funcionando igual desde el punto de vista del código que las llama, pero ahora:
- **Solo loguean al archivo** (no generan output en consola durante renderizado)
- **Mantienen los notebooks limpios** sin mensajes en el output HTML
- Los detalles completos están disponibles en los archivos de log para análisis posterior

## Dependencias

Paquetes requeridos:
- `cli` - Formato de mensajes
- `fs` - Manejo de archivos
- `jsonlite` - Progress.json
- `stringr` - Manipulación de strings
- `digest` - Generación de session ID
- `zip` (opcional) - Compresión de logs rotados

## Referencias

- [R Logging Best Practices](https://cran.r-project.org/web/packages/logger/vignettes/Intro.html)
- [Structured Logging](https://www.structuredlogging.org/)
- [Twelve-Factor App: Logs](https://12factor.net/logs)
