#!/usr/bin/env bash
set +e  # reintentos manuales

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=valid_and_upload_to_blob.sh
. "$SCRIPT_DIR/valid_and_upload_to_blob.sh"

print_help() {
  cat <<'EOF'
Scoring de Nueva Muestra (Azure Container Apps Job)

Uso:
  scoring.sh --input-dir <prefix-in-blob> [--output-dir <prefix-out-blob>] [--model-dir <prefix-model-blob>]

Dónde en --input-dir se esperan:
  - Control de SmartModelStudio.xlsx o .json
  - Muestra_Desarrollo.txt
  - Muestra_Scoring.txt

El Modelo.zip se busca en:
  - --model-dir si se provee
  - si no, en el mismo prefix de --input-dir

Requiere env vars:
  AZURE_STORAGE_CONNECTION_STRING
  AZURE_STORAGE_CONTAINER_RESULTS

Opcionales:
  BSM_USER (default: user)
  BSM_VERSION (default: 10.3)
  BSM_DIR (default: /home/$BSM_USER/Documents/besmart/$BSM_VERSION)
  BSM_RETRIES (default: 3)
  QUARTO_TIMEOUT (default: 30m)
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
MODEL_PATH=""
RUNTIME_LOG_NAME="scoring_runtime.log"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --input-dir)  INPUT_PATH="$2"; shift 2 ;;
    --output-dir) OUTPUT_PATH="$2"; shift 2 ;;
    --model-dir)  MODEL_PATH="$2"; shift 2 ;;
    -h|--help)    print_help; exit 0 ;;
    *) echo "Invalid option: $1"; print_help; exit 1 ;;
  esac
done

if [[ -z "${INPUT_PATH:-}" ]]; then
  echo "Error: --input-dir es obligatorio."
  print_help
  exit 1
fi

if [[ -z "${OUTPUT_PATH:-}" ]]; then
  echo "Atención: No se suministró --output-dir. Se asume igual a --input-dir."
  OUTPUT_PATH="$INPUT_PATH"
fi

if [[ -z "${MODEL_PATH:-}" ]]; then
  MODEL_PATH="$INPUT_PATH"
fi

# --- env obligatorias ---
: "${AZURE_STORAGE_CONNECTION_STRING:?Falta AZURE_STORAGE_CONNECTION_STRING}"
: "${AZURE_STORAGE_CONTAINER_RESULTS:?Falta AZURE_STORAGE_CONTAINER_RESULTS}"

BLOB_CONN="$AZURE_STORAGE_CONNECTION_STRING"
BLOB_CONTAINER="$AZURE_STORAGE_CONTAINER_RESULTS"

BLOB_INPUT_PREFIX="$(blob_normalize_prefix "$INPUT_PATH")"
BLOB_OUTPUT_PREFIX="$(blob_normalize_prefix "$OUTPUT_PATH")"
BLOB_MODEL_PREFIX="$(blob_normalize_prefix "$MODEL_PATH")"

echo "INPUT (blob):  container='$BLOB_CONTAINER' prefix='$BLOB_INPUT_PREFIX/'"
echo "MODEL (blob):  container='$BLOB_CONTAINER' prefix='$BLOB_MODEL_PREFIX/'"
echo "OUTPUT (blob): container='$BLOB_CONTAINER' prefix='$BLOB_OUTPUT_PREFIX/'"

# --- logging a archivo ---
LOG_FILE="/tmp/scoring.$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG_FILE") 2>&1
set -x
trap 'echo "ERROR en línea $LINENO. Comando: $BASH_COMMAND"; blob_upload_log "$BLOB_CONN" "$BLOB_CONTAINER" "$LOG_FILE" "$BLOB_OUTPUT_PREFIX" "$RUNTIME_LOG_NAME" "quiet"; exit 1' ERR

pipeline_scoring() {
  echo "JOB START $(date -Is)"
  echo "BSM_RETRIES='${BSM_RETRIES:-<unset>}' QUARTO_TIMEOUT='${QUARTO_TIMEOUT:-<unset>}'"

  blob_require_az || return 1
  blob_require_container "$BLOB_CONN" "$BLOB_CONTAINER" || { echo "No existe el contenedor Blob: $BLOB_CONTAINER"; return 1; }

  blob_require_prefix "$BLOB_CONN" "$BLOB_CONTAINER" "${BLOB_INPUT_PREFIX%/}/" || {
    echo "No existe el prefijo en Blob (input): $BLOB_CONTAINER/${BLOB_INPUT_PREFIX%/}/"
    return 1
  }

  blob_require_prefix "$BLOB_CONN" "$BLOB_CONTAINER" "${BLOB_MODEL_PREFIX%/}/" || {
    echo "No existe el prefijo en Blob (model): $BLOB_CONTAINER/${BLOB_MODEL_PREFIX%/}/"
    return 1
  }

  WORK_ROOT="$(mktemp -d)"
  IN_DIR="$WORK_ROOT/in"
  OUT_DIR="$WORK_ROOT/out"
  mkdir -p "$IN_DIR" "$OUT_DIR"

  # --- blobs (input) ---
  local control_xlsx="${BLOB_INPUT_PREFIX%/}/Control de SmartModelStudio.xlsx"
  local control_json="${BLOB_INPUT_PREFIX%/}/Control de SmartModelStudio.json"
  local muestra_desa="${BLOB_INPUT_PREFIX%/}/Muestra_Desarrollo.txt"
  local muestra_score="${BLOB_INPUT_PREFIX%/}/Muestra_Scoring.txt"

  # --- blobs (model) ---
  local modelo_zip="${BLOB_MODEL_PREFIX%/}/Modelo.zip"

  # Param file
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

  # Required files (input)
  blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_desa"  || { echo "Error: falta Muestra_Desarrollo.txt en $muestra_desa"; return 1; }
  blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_score" || { echo "Error: falta Muestra_Scoring.txt en $muestra_score"; return 1; }

  # Required file (model)
  blob_exists "$BLOB_CONN" "$BLOB_CONTAINER" "$modelo_zip" || { echo "Error: falta Modelo.zip en $modelo_zip"; return 1; }

  blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_desa"  "$IN_DIR/Muestra_Desarrollo.txt" || return 1
  blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$muestra_score" "$IN_DIR/Muestra_Scoring.txt" || return 1
  blob_download "$BLOB_CONN" "$BLOB_CONTAINER" "$modelo_zip"    "$IN_DIR/Modelo.zip" || return 1

  # Workspace
  BSM_USER="${BSM_USER:-user}"
  BSM_VERSION="${BSM_VERSION:-10.3}"
  BSM_DIR="${BSM_DIR:-/home/$BSM_USER/Documents/besmart/$BSM_VERSION}"
  export BSM_USER BSM_VERSION BSM_DIR

  echo "Usando BSM_DIR='$BSM_DIR' BSM_VERSION='$BSM_VERSION' BSM_USER='$BSM_USER'"
  env | grep -E '^BSM_' || true

  mkdir -p "$BSM_DIR/Params" "$BSM_DIR/Datos" "$BSM_DIR/Trabajo" "$BSM_DIR/Reportes"

  cp "$IN_DIR/Control de SmartModelStudio.$PARAM_FILE_EXT" "$BSM_DIR/Params/" || return 1
  cp "$IN_DIR/Muestra_Desarrollo.txt" "$BSM_DIR/Datos/" || return 1
  cp "$IN_DIR/Muestra_Scoring.txt" "$BSM_DIR/Datos/" || return 1
  cp "$IN_DIR/Modelo.zip" "$BSM_DIR/Trabajo/" || return 1

  echo "DEBUG: ls -lah '$BSM_DIR/Scripts' '$BSM_DIR/Librerias'"
  ls -lah "$BSM_DIR" || true
  ls -lah "$BSM_DIR/Scripts" || true
  ls -lah "$BSM_DIR/Librerias" || true

  # correr cuaderno (evita jobs eternos)
  cd "$BSM_DIR" || return 1
  QUARTO_LOG="$BSM_DIR/Trabajo/results_scoring.log"
  QUARTO_TIMEOUT="${QUARTO_TIMEOUT:-30m}"

  timeout "$QUARTO_TIMEOUT" quarto render "Cuadernos/Scoring.qmd" --log "$QUARTO_LOG"
  st=$?
  echo "quarto exit code=$st"

  # mostrar log para que salga en stdout del job
  if [[ -f "$QUARTO_LOG" ]]; then
    echo "===== QUARTO LOG (tail 200) ====="
    tail -n 200 "$QUARTO_LOG"
    echo "===== END QUARTO LOG ====="
  else
    echo "No existe el log de Quarto: $QUARTO_LOG"
  fi

  # outputs opcionales
  cp "$QUARTO_LOG" "$OUT_DIR/" 2>/dev/null || true
  cp -r "$BSM_DIR/Reportes" "$OUT_DIR/" 2>/dev/null || true
  cp "$BSM_DIR/Trabajo/Scores.txt" "$OUT_DIR/" 2>/dev/null || true
  cp "$LOG_FILE" "$OUT_DIR/scoring_runtime.log" 2>/dev/null || true

  blob_zip_html_results "$OUT_DIR" "resultados.zip" "Scores.txt" || true
  blob_upload_batch "$BLOB_CONN" "$BLOB_CONTAINER" "$OUT_DIR" "$BLOB_OUTPUT_PREFIX" "quiet" || true
  blob_upload_log "$BLOB_CONN" "$BLOB_CONTAINER" "$LOG_FILE" "$BLOB_OUTPUT_PREFIX" "$RUNTIME_LOG_NAME" "quiet" || true

  echo "JOB END $(date -Is) rc=$st"
  return "$st"
}

MAX_RETRIES="${BSM_RETRIES:-3}"
i=1
while [[ $i -le $MAX_RETRIES ]]; do
  echo "Intento $i/$MAX_RETRIES"
  pipeline_scoring
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
