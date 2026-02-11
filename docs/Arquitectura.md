# Arquitectura SmartModel (10.3)

## Resumen

SmartModel se ejecuta en un único workspace controlado por `BSM_VERSION` y `BSM_DIR`.
Todo el contenido versionado se copia a `BSM_DIR` en la imagen de producción.
En desarrollo, `BSM_DIR` puede montarse desde el host para edición directa.

## Variables de Entorno

- `BSM_VERSION`: versión activa (ej. `10.3`).
- `BSM_DIR`: workspace absoluto (ej. `/home/user/Documents/besmart/10.3`).
- `BSM_LOG_DIR`: `${BSM_DIR}/Logs`.
- `PROGRESS_JSON_PATH`: `${BSM_LOG_DIR}/progress.json`.

## Estructura del Workspace

```
${BSM_DIR}/
├── Librerias/
├── Cuadernos/
├── Scripts/
├── Params/
├── Datos/
├── Tableros/
├── Logs/
├── Trabajo/
├── Reportes/
└── Auxil/
```

## Producción (Dockerfile.config)

- Copia `volume_versiones/${BSM_VERSION}/` directamente a `BSM_DIR`.
- Crea `Logs/`, `Trabajo/`, `Reportes/`, `Auxil/`.
- Ajusta permisos para el usuario `user`.

## Desarrollo (bind-mount)

- El host puede montar `volume_versiones/${BSM_VERSION}` en `BSM_DIR`.
- El runtime sigue usando `BSM_DIR` como única referencia.

## Inicio en R

`RProfile.site` define `bsm_path` con `BSM_VERSION`, crea directorios y carga librerías desde `bsm_path`.
