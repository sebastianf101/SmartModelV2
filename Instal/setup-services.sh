#!/usr/bin/env bash
set -euo pipefail

# Create service directories
mkdir -p services.d/sshd services.d/api services.d/healthcheck cont-init.d

# SSHD service
cat > services.d/sshd/run <<'EOF'
#!/usr/bin/execlineb -P
/usr/sbin/sshd -D
EOF
chmod +x services.d/sshd/run

# API service
cat > services.d/api/run <<'EOF'
#!/usr/bin/execlineb -P
su ${BSM_USER} -c 'R --no-save -f Tableros/Explorador_IVs_API_run.R'
EOF
chmod +x services.d/api/run

# Healthcheck service (optional)
cat > services.d/healthcheck/run <<'EOF'
#!/usr/bin/execlineb -P
curl -sf http://localhost:3838/__docs__/ > /dev/null
EOF
chmod +x services.d/healthcheck/run

# Init script to prepare sshd runtime dir
cat > cont-init.d/00-sshd-dir <<'EOF'
#!/usr/bin/with-contenv bash
set -e
mkdir -p /var/run/sshd
chmod 0755 /var/run/sshd
EOF
chmod +x cont-init.d/00-sshd-dir

echo "✅ Service scripts created in $(pwd)"
