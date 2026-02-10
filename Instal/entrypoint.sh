#!/usr/bin/env bash
set -euo pipefail

# Ensure SSH host keys and runtime dir exist
ssh-keygen -A >/dev/null 2>&1 || true
mkdir -p /var/run/sshd
chmod 0755 /var/run/sshd

# If running as root, ensure correct ownership for user home
if id user >/dev/null 2>&1; then
  chown -R user:consultores_ext /home/user 2>/dev/null || true
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
