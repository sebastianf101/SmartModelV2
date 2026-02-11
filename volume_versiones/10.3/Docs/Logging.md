# Logging SmartModel (10.3)

## Resumen

El logging se inicializa automáticamente en `Setup.R` con `log_init()` y escribe en `${BSM_LOG_DIR}`.

## Variables de Entorno

- `BSM_LOG_DIR`: `${BSM_DIR}/Logs`.
- `PROGRESS_JSON_PATH`: `${BSM_LOG_DIR}/progress.json`.

## Ubicación de Archivos

```
${BSM_DIR}/Logs/
├── smartmodel_YYYYMMDD.log
└── progress.json
```

## Uso Básico

```r
source(fs::path(bsm_path, "Librerias/Funciones logging.R"))
.bsm_session_id <- log_init()

log_info("Proceso iniciado")
log_warn("Advertencia")
log_error("Falló validación", code = "202")
```

## Contexto de Notebook

```r
log_set_notebook("Modelling.qmd")
# ... ejecución ...
log_clear_notebook()
```

## Modo Batch vs Interactivo

- **Batch**: escribe solo a archivo.
- **Interactivo**: escribe a archivo y consola.
