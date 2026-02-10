#!/usr/bin/env bash
set -euo pipefail

# SSH last-resort access
ssh-keygen -A >/dev/null 2>&1 || true
mkdir -p /var/run/sshd
chmod 0755 /var/run/sshd
/usr/sbin/sshd -D -e &

# Asegurar HOME/USER para que ~ expanda bien aunque el proceso sea root
BSM_USER="${BSM_USER:-user}"
export BSM_USER
export HOME="${HOME:-/home/$BSM_USER}"
export USER="${USER:-$BSM_USER}"
export LOGNAME="${LOGNAME:-$BSM_USER}"

# Crear home si no existe (por si la imagen base no lo trae)
mkdir -p "$HOME" || true

cmd="${1:-}"
shift || true

cmd="$(echo "$cmd" | tr '[:upper:]' '[:lower:]' | sed 's/\.sh$//' | tr '-' '_')"

case "$cmd" in
  modelar)
    exec /usr/local/bin/modelar.sh "$@"
    ;;
  validar_desa|validar_desa_sh)
    exec /usr/local/bin/validar_desa.sh "$@"
    ;;
  validar_nueva|validar_nueva_sh)
    exec /usr/local/bin/validar_nueva.sh "$@"
    ;;
  scoring|scoring_sh)
    exec /usr/local/bin/scoring.sh "$@"
    ;;
  *)
    echo "Uso: <modelar|validar_desa|validar_nueva|scoring> [args...]"
    exit 2
    ;;
esac
