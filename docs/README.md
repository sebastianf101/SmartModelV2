# SmartModelV2

## Resumen

Este repositorio incluye el código y la configuración de SmartModel V2, además de una configuración de Dev Container para desarrollo.

## Dev Container

Esta configuración del Dev Container busca reflejar el contenedor de producción y permitir edición directa del código versionado. RStudio Server no está incluido.

### Configuración

- **Usuario**: el contenedor corre como `user` (UID: 1000, GID: 1000).
- **Sync**: solo la carpeta `volume_versiones/10.3` del host se monta en `/home/user/Documents/besmart/10.3`.
- **Git**: la carpeta `.git` no se monta; las operaciones de Git deben hacerse en el host.

### Troubleshooting

#### Permission denied al guardar

- **Síntoma**: no puedes guardar archivos dentro del contenedor ("Permission denied").
- **Causa**: desalineación UID/GID entre host y contenedor.
- **Solución**: asegurar UID/GID 1000 en el host para que los bind mounts sean escribibles.

#### Variables de entorno

Compose carga variables desde `../.env` (ver `.devcontainer/docker-compose.dev.yml`). Para cambiar puertos o defaults, editar ese archivo y reconstruir el contenedor.

### Logs locales 🔍

- Para inspeccionar logs localmente sin abrir un workspace multi-root, puede crearse un enlace simbólico en la raíz del repositorio:

  `ln -s /tmp/sm-logs-Mi_SM sm-logs-Mi_SM`

- El enlace está añadido a `.gitignore` para evitar que se versionen logs y **no debe** ser committeado. Para eliminarlo: `rm sm-logs-Mi_SM`

## Publishing releases 🔧

This repository publishes container images to GitHub Container Registry.

Defaults:
- Image: `ghcr.io/SmartModelling/smartmodelv2`
- Dockerfile: `docker/Dockerfile.config`
- Tag format: `vMAJOR.MINOR.PATCH` (also pushed as `latest`)

Required environment variables (local runs):
- `GHCR_PAT` — Personal Access Token with `packages:write` (and `repo` scope if you want to create releases)
- `GHCR_USER` — (optional) your GitHub username for `docker login`

In CI (GitHub Actions) the script prefers `GITHUB_TOKEN` for GitHub API actions.

Quick examples:
- Dry run: `./docker/release-publish.sh --version 1.2.3 --dry-run`
- Build + push + release: `GHCR_PAT=xxx ./docker/release-publish.sh --version 1.2.3`
- Bump patch and publish: `GHCR_PAT=xxx ./docker/release-publish.sh --bump patch`

Verification:
- `docker images | grep smartmodelv2`
- `git tag -l 'v*'`
- Check Releases and Packages in `https://github.com/SmartModelling/SmartModelV2`

