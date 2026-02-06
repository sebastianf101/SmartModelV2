# SmartModel V2 Dev Container Setup

This Dev Container configuration is designed to provide a development environment that mirrors the production container while allowing direct editing of the versioned source code.

## Configuration Overview

- **User**: The container runs as `user` (UID: 1301, GID: 1300), **NOT** the default `root` or `rstudio` (UID 1000).
- **Sync**: Only the folder `volume_versiones/10.3` from the host is mounted into the container at `/home/user/Documents/besmart/10.3`.
- **Git**: The `.git` folder is **not** mounted. Git operations should be performed on the host machine.

## Troubleshooting

### 1. Permission Denied Errors

**Symptom**: You cannot save files within the container, getting "Permission denied" errors.

**Cause**:
The container user `user` has UID 1301. Your host user likely has UID 1000.
When the container tries to write to the bind-mounted directory `volume_versiones/10.3`, the Linux host kernel blocks the write if the file is owned by UID 1000 and "others" do not have write permissions.

**Solution**:
On the **host** machine, grant write permissions to "others" for the synced folder:

```bash
chmod -R o+w volume_versiones/10.3
```

If you create new files inside the container (which will be owned by UID 1301), you might need to run this command again on the host to allow your host user to modify them later.

### 2. Environment Variables

Variables are loaded from `.devcontainer/.env`. If you need to change ports or default settings, edit that file and rebuild the container.
