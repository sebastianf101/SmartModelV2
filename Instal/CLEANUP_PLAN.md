# Plan de limpieza (sin borrar archivos)

Este plan propone candidatos a limpieza para reducir ruido y facilitar el mantenimiento. No se eliminará nada hasta confirmar.

## Candidatos a archivar o eliminar (completado)

### Contenedores/compose antiguos (movidos a `Instal/Archive/2026-02-09/legacy-configs/`)
- `Instal/Automat/config-contenedor-old.yml`
- `Instal/Automat/config-contenedor-bsm-parche-server.yml`
- `Instal/Automat/config-contenedor-bsm-parche-server_CON_EXPOSE.yml`

### Scripts duplicados o de prueba (movidos a `Instal/Archive/2026-02-09/legacy-scripts/`)
- `Instal/Automat/Explorador_IVs_API_2.sh`
- `Instal/prueba.sh`
- `Instal/git related.R`
- `Instal/Dev/`

### Artefactos y ejemplos locales
- `Instal/Examples/Entrada/` (ejemplos vigentes)
- `Instal/Archive/2026-02-09/artifacts/bsm-studio.tar.gz`
- `Instal/Archive/2026-02-09/legacy-docs/Guia de Usuario.scratch.txt`

## Criterios de decisión
- **En uso productivo**: se conserva, se documenta el uso real.
- **Obsoleto**: se archiva en `Instal/Archive/` con fecha.
- **Duplicado**: se mantiene solo el flujo recomendado.

## Resultado
Se movieron los candidatos a carpeta de archivo para reducir ruido sin perder historial.
