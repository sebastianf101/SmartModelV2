# SmartModel V2 Dev Container Setup

This Dev Container configuration is designed to provide a development environment that mirrors the production container while allowing direct editing of the versioned source code. RStudio Server is not included.

## Configuration Overview

- **User**: The container runs as `user` (UID: 1000, GID: 1000).
- **Sync**: Only the folder `volume_versiones/10.3` from the host is mounted into the container at `/home/user/Documents/besmart/10.3`.
- **Git**: The `.git` folder is **not** mounted. Git operations should be performed on the host machine.

## Troubleshooting

### 1. Permission Denied Errors

**Symptom**: You cannot save files within the container, getting "Permission denied" errors.

**Cause**:
UID/GID mismatch between host and container.

**Solution**:
Ensure the host user is UID/GID 1000 so bind mounts are writable without extra chmod.

### 2. Environment Variables

Compose loads variables from `.devcontainer/.env` because the dev compose file is listed first (making `.devcontainer` the project directory). If you need to change ports or defaults, edit that file and rebuild the container.
