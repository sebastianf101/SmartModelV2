#!/usr/bin/env bash
set -euo pipefail

# Ensure SSH host keys and runtime dir exist
ssh-keygen -A >/dev/null 2>&1 || true
mkdir -p /var/run/sshd
chmod 0755 /var/run/sshd

# If running as root, ensure correct ownership for user home and log directory
if id user >/dev/null 2>&1; then
  chown -R user:consultores_ext /home/user 2>/dev/null || true
  
  # Fix log directory permissions if BSM_LOG_DIR is set and exists
  if [ -n "${BSM_LOG_DIR:-}" ] && [ -d "${BSM_LOG_DIR}" ]; then
    chown -R user:consultores_ext "${BSM_LOG_DIR}"
    chmod -R u+rwX,g+rX "${BSM_LOG_DIR}"
  fi
  
  ssh_dir="/home/user/.ssh"
  env_file="$ssh_dir/environment"
  mkdir -p "$ssh_dir"
  printenv | grep -v '^_' | grep -v '^SHLVL=' | grep -v '^PWD=' | grep -v '^HOME=' > "$env_file" || touch "$env_file"
  chown user:consultores_ext "$env_file"
  chmod 600 "$env_file"
fi

if [ "$#" -gt 0 ]; then
  /usr/sbin/sshd -D -e &
  exec "$@"
fi

exec /usr/sbin/sshd -D -e
