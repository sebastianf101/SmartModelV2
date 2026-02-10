# SmartModelV2 — Repository Architecture

This document defines the layout after the 2026-02 reorganization.

## Root

| Path | Purpose |
|------|---------|
| `docker/` | Dockerfiles, entrypoint, build script, user-provisioning assets |
| `automation/` | Compose files, lifecycle scripts, env-var defaults |
| `automation/ssh/` | SSH keypair for consultant access |
| `automation/azure/` | Azure Container Instance job scripts |
| `docs/` | Quarto documentation sources (Guía del Consultor, Instalación, Interfases) |
| `examples/` | Example input data for automation scripts |
| `volume_versiones/` | Versioned runtime assets (copied into images / mounted for dev) |
| `.devcontainer/` | Devcontainer config (SSH-only, no RStudio Server) |
| `.github/workflows/` | CI/CD (Docker publish) |

## `docker/`

| File | Description |
|------|-------------|
| `Dockerfile.studio-base` | Base image: R runtime + system deps + Azure CLI + R packages |
| `Dockerfile.studio` | Final image: single-user SSH, built on studio-base |
| `Dockerfile.azure` | Azure wrapper: copies job scripts into studio image |
| `entrypoint.sh` | Unified SSH entrypoint (generates host keys, starts sshd) |
| `build.sh` | Build all images, push to Docker Hub + GHCR |
| `RProfile.site` | R site profile (sets `bsm_path`) |
| `Init_users.sh` | User workspace initialization at build time |
| `alta_usr_docker.sh` | Helper to add users (legacy, kept for patching) |
| `id_rocker.pub` | SSH public key baked into image |
| `users.conf.newusers` | `newusers(8)` format file for user provisioning |

## `automation/`

| File | Description |
|------|-------------|
| `config-contenedor-bsm.yml` | Base compose: SSH + dashboard ports |
| `config-contenedor-api.override.yml` | API-mode command override |
| `Levantar_Contenedor.sh` / `Bajar_Contenedor.sh` | Start / stop ephemeral containers |
| `compose_up.sh` | Quick compose-up wrapper |
| `Modelar.sh` | Main modelling pipeline |
| `Validar_Desa.sh` / `Validar_Nueva.sh` | Validation flows |
| `Scoring.sh` | Scoring pipeline |
| `pull.sh` | Pull image from GHCR |
| `envvars_defaults.sh` / `envvars_sesion.sh` / `envvars_efimeras.sh` | Environment variable layers |
| `Explorador_IVs_API.sh` / `Explorador_IVs_API_2.sh` | IV explorer launchers |
| `Tablero_IVs.sh` | IV dashboard launcher |
| `ssh/` | SSH keypair (`id_consultor`, `id_consultor.pub`) |
| `azure/` | Azure job entrypoints (Modelar, Validar, Scoring, entrypoint, etc.) |

## `volume_versiones/10.3/`

| Path | Description |
|------|-------------|
| `Librerias/` | R libraries (Setup.R, logging, auxiliary functions) |
| `Logs/`, `Reportes/`, `Trabajo/`, `Auxil/` | Runtime directories (user workspace) |

## Conventions

- **Single user**: `user` with UID/GID `1000:1000`, group `consultores_ext`.
- **SSH-only**: No RStudio Server, no s6. Port 22 only.
- **Build context**: Always the project root. Dockerfiles reference `./docker/…` paths.
- **Single runtime workspace**:
  - `bsm_path` (`~/Documents/besmart/10.3/`) — read-write user workspace.

