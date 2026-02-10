# SmartModelV2 folder architecture

This document defines the intended layout to keep the repository maintainable.

## Root
- `Instal/`: Build, run, and automation assets for SmartModel containers.
- `volume_versiones/`: Versioned runtime assets (copied into images or mounted for dev).
- `.devcontainer/`: Devcontainer configuration (SSH-only, no RStudio Server).

## `Instal/`
- `Dockerfile.bsm-studio-paq`: Base image (R runtime + system deps + R packages).
- `Dockerfile.bsm-studio`: Final image (SSH-only, single user `user`).
- `Dockerfile.azure`: Azure runtime wrapper image (uses `bsm-studio`).
- `entrypoint.sh`: Unified SSH entrypoint.
- `Automat/`: Scripts that drive the container workflows and compose files.
- `Automat/Scripts-Azure/`: Azure job entrypoints and scripts.
- `Examples/Entrada/`: Example input data for automation scripts.
- `Artifacts/`: Build outputs (tarballs). Ignored by git.

## `Instal/Automat/`
- `config-contenedor-bsm.yml`: Base compose for SSH + dashboard.
- `config-contenedor-api.override.yml`: API mode command override.
- `Levantar_Contenedor.sh` / `Bajar_Contenedor.sh`: Lifecycle control for ephemeral containers.
- `Modelar.sh`, `Validar_Desa.sh`, `Validar_Nueva.sh`, `Scoring.sh`: Main automation flows.

## Conventions
- **Single user**: only `user` with UID/GID `1000:1000`.
- **SSH is last resort**: default access is via automation scripts or dashboard ports.
- **No RStudio Server**: port `8787` is not used.
- **Keep legacy out of tree**: deprecated files should be removed or kept in external archives.
