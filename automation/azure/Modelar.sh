#!/usr/bin/env bash
set +e  # reintentos

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=valid_and_upload_to_blob.sh
. "$SCRIPT_DIR/valid_and_upload_to_blob.sh"

print_help() {
  cat <<'EOF'
Modelado (Azure Container Apps Job)

Uso:
  modelar.sh --input-dir <prefix-in-blob> [--output-dir <prefix-out-blob>]

En --input-dir se esperan:
  - Control de SmartModelStudio.xlsx o .json
  - Muestra_Desarrollo.txt

Se sube a --output-dir:
  - results_clean_transf.log
  - results_modelling.log
  - Modelo.zip (si se genera)
  - Reportes/ (si existe)
  - modelar_runtime.log

Requiere env vars:
  AZURE_STORAGE_CONNECTION_STRING
  AZURE_STORAGE_CONTAINER_RESULTS

Opcionales:
  BSM_USER (default: user)
  BSM_VERSION (default: 10.3)
  BSM_DIR (default: /home/$BSM_USER/Documents/besmart/$BSM_VERSION)
  BSM_RETRIES (default: 3)
  QUARTO_TIMEOUT_CLEAN (default: 30m)
  QUARTO_TIMEOUT_MODEL (default: 60m)
EOF
}

# --- help ---
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  print_help
  exit 0
fi

# --- args ---
INPUT_PATH=""
OUTPUT_PATH=""
RUNTIME_LOG_NAME="modelar_runtime.log"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --input-dir)  INPUT_PATH="$2"; shift 2 ;;
    --output-dir) OUTPUT_PATH="$2"; shift 2 ;;
    -h|--help)    print_help; exit 0 ;;
    *) echo "Opción inválida: $1"; print_help; exit 1 ;;
  esac
done

if [[ -z "${INPUT_PATH:-}" ]]; then
  echo "Falta --input-dir"
  exit 1
fi

if [[ -z "${OUTPUT_PATH:-}" ]]; then
  echo "Atención: No se suministró --output-dir. Se asume igual a --input-dir."
  OUTPUT_PATH="$INPUT_PATH"
fi

# --- env obligatorias ---
: "${AZURE_STORAGE_CONNECTION_STRING:?Falta AZURE_STORAGE_CONNECTION_STRING}"
: "${AZURE_STORAGE_CONTAINER_RESULTS:?Falta AZURE_STORAGE_CONTAINER_RESULTS}"

BLOB_CONN="$AZURE_STORAGE_CONNECTION_STRING"
BLOB_CONTAINER="$AZURE_STORAGE_CONTAINER_RESULTS"

BLOB_INPUT_PREFIX="$(blob_normalize_prefix "$INPUT_PATH")"
BLOB_OUTPUT_PREFIX="$(blob_normalize_prefix "$OUTPUT_PATH")"

echo "INPUT (blob):  container='$BLOB_CONTAINER' prefix='$BLOB_INPUT_PREFIX/'"
echo "OUTPUT (blob): container='$BLOB_CONTAINER' prefix='$BLOB_OUTPUT_PREFIX/'"

# --- logging a archivo ---
LOG_FILE="/tmp/modelar.$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG_FILE") 2>&1
set -x
trap 'echo "ERROR en línea $LINENO. Comando: $BASH_COMMAND"; blob_upload_log "$BLOB_CONN" "$BLOB_CONTAINER" "$LOG_FILE" "$BLOB_OUTPUT_PREFIX" "$RUNTIME_LOG_NAME" "quiet"; exit 1' ERR

pipeline_modelar() {
  echo "JOB START $(date -Is)"
  echo "BSM_RETRIES='${BSM_RETRIES:-<unset>}' CLEAN_TIMEOUT='${QUARTO_TIMEOUT_CLEAN:-<unset>}' MODEL_TIMEOUT='${QUARTO_TIMEOUT_MODEL:-<unset>}'"

  blob_require_az || return 1
  blob_require_container "$BLOB_CONN" "$BLOB_CONTAINER" || { echo "No existe el contenedor Blob: $BLOB_CONTAINER"; return 1; }

  # Input debe existir. Output NO hace falta que exista.
  blob_require_prefix "$BLOB_CONN" "$BLOB_CONTAINER" "${BLOB_INPUT_PREFIX%/}/" || {
    echo "No existe el prefijo en Blob (input): $BLOB_CONTAINER/${BLOB_INPUT_PREFIX%/}/"
    return 1
  }

  WORK_ROOT="$(mktemp -d)"
  IN_DIR="$WORK_ROOT/in"
  OUT_DIR="$WORK_ROOT/out"
  mkdir -p "$IN_DIR" "$OUT_DIR"

  # blobs esperados
  local control_xlsx="${BLOB_INPUT_PREFIX%/}/Control de SmartModelStudio.xlsx"
  local control_json="${BLOB_INPUT_PREFIX%/}/Control de SmartModelStudio.json"
  local muestra_txt="${BLOB_INPUT_PREFIX%/}/Muestra_Desarrollo.txt"

  if blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$control_xlsx"; then
    blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$control_xlsx" "$IN_DIR/Control de SmartModelStudio.xlsx" || return 1
    PARAM_FILE_EXT=xlsx
  elif blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$control_json"; then
    blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$control_json" "$IN_DIR/Control de SmartModelStudio.json" || return 1
    PARAM_FILE_EXT=json
  else
    echo "Error: no existe Control de SmartModelStudio.xlsx ni .json en input prefix"
    return 1
  fi

  blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_txt" || { echo "Error: falta Muestra_Desarrollo.txt en $muestra_txt"; return 1; }
  blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_txt" "$IN_DIR/Muestra_Desarrollo.txt" || return 1

  # Workspace
  BSM_USER="${BSM_USER:-user}"
  BSM_VERSION="${BSM_VERSION:-10.3}"
  BSM_DIR="${BSM_DIR:-/home/$BSM_USER/Documents/besmart/$BSM_VERSION}"
  export BSM_USER BSM_VERSION BSM_DIR

  mkdir -p "$BSM_DIR/Params" "$BSM_DIR/Datos" "$BSM_DIR/Trabajo" "$BSM_DIR/Reportes"

  cp "$IN_DIR/Control de SmartModelStudio.$PARAM_FILE_EXT" "$BSM_DIR/Params/" || return 1
  cp "$IN_DIR/Muestra_Desarrollo.txt" "$BSM_DIR/Datos/" || return 1

  echo "DEBUG: ls -lah '$BSM_DIR/Scripts' '$BSM_DIR/Librerias'"
  ls -lah "$BSM_DIR" || true
  ls -lah "$BSM_DIR/Scripts" || true
  ls -lah "$BSM_DIR/Librerias" || true

  cd "$BSM_DIR" || return 1

  CLEAN_LOG="$BSM_DIR/Trabajo/results_clean_transf.log"
  MODEL_LOG="$BSM_DIR/Trabajo/results_modelling.log"
  QUARTO_TIMEOUT_CLEAN="${QUARTO_TIMEOUT_CLEAN:-30m}"
  QUARTO_TIMEOUT_MODEL="${QUARTO_TIMEOUT_MODEL:-60m}"

  # 1) Clean-Transf
  timeout "$QUARTO_TIMEOUT_CLEAN" quarto render "Cuadernos/Clean-Transf.qmd" --log "$CLEAN_LOG"
  st1=$?
  echo "clean-transf exit code=$st1"

  # 2) Modelling (solo si 1 ok)
  if [[ $st1 -eq 0 ]]; then
    timeout "$QUARTO_TIMEOUT_MODEL" quarto render "Cuadernos/Modelling.qmd" --log "$MODEL_LOG"
    st2=$?
  else
    st2=1
  fi
  echo "modelling exit code=$st2"

  # Emit logs en stdout del job (si existen)
  if [[ -f "$CLEAN_LOG" ]]; then
    echo "===== CLEAN LOG (tail 200) ====="
    tail -n 200 "$CLEAN_LOG"
    echo "===== END CLEAN LOG ====="
  fi
  if [[ -f "$MODEL_LOG" ]]; then
    echo "===== MODEL LOG (tail 200) ====="
    tail -n 200 "$MODEL_LOG"
    echo "===== END MODEL LOG ====="
  fi

  # Copiar resultados locales al OUT_DIR
  cp "$CLEAN_LOG" "$OUT_DIR/" 2>/dev/null || true
  cp "$MODEL_LOG" "$OUT_DIR/" 2>/dev/null || true
  cp "$BSM_DIR/Trabajo/Modelo.zip" "$OUT_DIR/" 2>/dev/null || true
  cp "$BSM_DIR/Trabajo/sent.sql" "$OUT_DIR/" 2>/dev/null || true
  cp -r "$BSM_DIR/Reportes" "$OUT_DIR/" 2>/dev/null || true
  cp "$LOG_FILE" "$OUT_DIR/modelar_runtime.log" 2>/dev/null || true

  # Subir resultados siempre (aunque falle)
  blob_zip_html_results "$OUT_DIR" "resultados.zip" "$OUT_DIR/sent.sql" || true
  blob_upload_batch "$BLOB_CONN" "$BLOB_CONTAINER" "$OUT_DIR" "$BLOB_OUTPUT_PREFIX" "quiet" || true
  blob_upload_log "$BLOB_CONN" "$BLOB_CONTAINER" "$LOG_FILE" "$BLOB_OUTPUT_PREFIX" "$RUNTIME_LOG_NAME" "quiet" || true

  # Exit code final
  if [[ $st1 -ne 0 ]]; then
    echo "JOB END $(date -Is) rc=$st1 (falló Clean-Transf)"
    return "$st1"
  fi

  echo "JOB END $(date -Is) rc=$st2"
  return "$st2"
}

MAX_RETRIES="${BSM_RETRIES:-3}"
i=1
while [[ $i -le $MAX_RETRIES ]]; do
  echo "Intento $i/$MAX_RETRIES"
  pipeline_modelar
  rc=$?
  if [[ $rc -eq 0 ]]; then
    echo "OK"
    exit 0
  fi
  echo "Falló (rc=$rc). Reintentando..."
  sleep 2
  i=$((i+1))
done

echo "Fallo definitivo luego de $MAX_RETRIES intentos"
exit 1
