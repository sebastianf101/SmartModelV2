# Guía Rápida de Logging - SmartModel

## Inicialización (Automática en Setup.R)

```r
source(fs::path(bsm_path, "Librerias/Funciones logging.R"))
.bsm_session_id <- log_init()
```

## Funciones Básicas

```r
log_trace("Detalle muy específico")
log_debug("x = {x}, y = {y}")
log_info("Proceso completado")
log_warn("Advertencia: {msg}")
log_error("Error en validación", code = "202")
log_fatal("Error crítico", code = "999")
```

## Contexto de Notebook

```r
log_set_notebook("Modelling.qmd")
# ... código del notebook ...
log_clear_notebook()
```

## Ejemplos de Uso

### Logging Simple
```r
log_info("Iniciando procesamiento de {nrow(df)} registros")
log_debug("Parámetros: nbins={nbins}, minpts={minpts}")
```

### Con Códigos de Error
```r
if (!validacion_ok) {
  log_error("Validación falló para variable {var}", code = "301")
  stop("Validación falló")
}
```

### Captura de Errores
```r
log_catch({
  resultado <- operacion_riesgosa()
}, context = "Procesamiento de datos", code = "401")
```

### En Loops
```r
for (i in seq_along(vars)) {
  log_debug("Procesando variable {vars[i]} ({i}/{length(vars)})")
  # ... procesamiento ...
}
```

## Niveles de Log

| Nivel | Uso | Consola Interactiva | Archivo |
|-------|-----|---------------------|---------|
| TRACE | Debugging detallado | ❌ | ✅ |
| DEBUG | Valores de variables | ❌ | ✅ |
| INFO | Progreso normal | ✅ | ✅ |
| WARN | Advertencias | ✅ | ✅ |
| ERROR | Errores | ✅ | ✅ |
| FATAL | Errores críticos | ✅ | ✅ |

## Formato de Log

```
[2026-01-22 14:30:22.456] [INFO] [20260122_143022_abc123] [Modelling.qmd] Mensaje aquí (Cod: 202)
```

## Archivos de Log

- **Ubicación**: `~/Documents/besmart/10.3/Logs/`
- **Formato**: `smartmodel_YYYYMMDD.log`
- **Rotación**: Automática a 10MB o diariamente
- **Retención**: 7 días (configurable)

## Comandos Útiles

### Ver logs en tiempo real
```bash
tail -f ~/Documents/besmart/10.3/Logs/smartmodel_$(date +%Y%m%d).log
```

### Filtrar errores
```bash
grep "\[ERROR\]" smartmodel_20260122.log
```

### Ver progress.json
```bash
cat ~/Documents/besmart/10.3/Logs/progress.json | jq .
```

### Limpiar logs antiguos
```r
source("Scripts/Limpiar Logs.R")
```

## Configuración

### Variables de Entorno
```bash
export BSM_LOG_DIR="~/Documents/besmart/10.3/Logs"
export PROGRESS_JSON_PATH="~/Documents/besmart/10.3/Logs/progress.json"
```

### Cambiar niveles mínimos
```r
log_init(
  min_level_console = "WARN",  # Solo warnings+ en consola
  min_level_file = "TRACE"     # Todo en archivo
)
```

## Modo Batch vs Interactivo

- **Interactivo** (`interactive() == TRUE`): Muestra en consola + archivo
- **Batch** (renderizado): **SOLO archivo** (no contamina notebooks)

**Importante**: `msg_custom()` y `error_custom()` también siguen esta regla - no generan output en notebooks renderizados.

## Session ID

Cada ejecución tiene un ID único:

```r
session_id <- log_get_session_id()
# Ejemplo: "20260122_143022_abc123"
```

Permite rastrear toda una ejecución en los logs.

## Mejores Prácticas

✅ **HACER**
- Usar interpolación: `log_info("Procesando {n} items")`
- Incluir códigos en errores: `log_error("Falló", code = "202")`
- Establecer contexto de notebook
- Limpiar logs periódicamente

❌ **EVITAR**
- Concatenar strings: `log_info(paste("x =", x))`
- Loguear en exceso en loops internos
- Olvidar códigos en errores importantes
- Logs gigantes sin rotación

## Troubleshooting

| Problema | Solución |
|----------|----------|
| Logs no aparecen | Verificar permisos en `~/Documents/besmart/10.3/Logs/` |
| Mensajes en notebooks | Normal - solo en consola interactiva, no en renderizados |
| Progress.json no actualiza | Verificar `PROGRESS_JSON_PATH` y permisos |
| Session ID NULL | Ejecutar `log_init()` manualmente |

## Soporte

Ver documentación completa: `Docs/Sistema de Logging.md`
