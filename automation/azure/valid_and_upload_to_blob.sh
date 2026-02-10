#!/usr/bin/env bash
# Common Azure Blob helpers shared by BSM scripts.
# Keep this file side-effect free: only function definitions.

blob_require_az() {
  local msg="${1:-Falta Azure CLI (az)}"
  command -v az >/dev/null 2>&1 || { echo "$msg"; return 1; }
}

blob_require_container() {
  local conn="$1"
  local container="$2"
  az storage container exists \
    --connection-string "$conn" \
    --name "$container" \
    --only-show-errors -o tsv | grep -q True
}

blob_require_prefix() {
  local conn="$1"
  local container="$2"
  local prefix="${3:-}"
  local debug="${4:-}"

  if [[ -z "${prefix:-}" ]]; then
    [[ -n "$debug" ]] && echo "blob_require_prefix: empty prefix -> OK"
    return 0
  fi

  [[ -n "$debug" ]] && echo "blob_require_prefix: container='$container' prefix='$prefix'"

  local first
  first="$(az storage blob list \
    --connection-string "$conn" \
    --container-name "$container" \
    --prefix "$prefix" \
    --only-show-errors \
    --query "[0].name" -o tsv 2>/dev/null || true)"

  [[ -n "$debug" ]] && echo "blob_require_prefix: first='$first'"

  [[ -n "${first:-}" ]]
}

blob_exists() {
  local conn="$1"
  local container="$2"
  local name="$3"
  az storage blob exists \
    --connection-string "$conn" \
    --container-name "$container" \
    --name "$name" \
    --only-show-errors -o tsv | grep -q True
}

blob_download() {
  local conn="$1"
  local container="$2"
  local name="$3"
  local dest="$4"
  az storage blob download \
    --connection-string "$conn" \
    --container-name "$container" \
    --name "$name" \
    --file "$dest" \
    --no-progress \
    --only-show-errors >/dev/null
}

blob_upload_batch() {
  local conn="$1"
  local container="$2"
  local src_dir="$3"
  local dest_prefix="${4:-}"
  local quiet="${5:-}"

  if [[ -n "${dest_prefix:-}" ]]; then
    if [[ "$quiet" == "quiet" ]]; then
      az storage blob upload-batch \
        --connection-string "$conn" \
        --source "$src_dir" \
        --destination "$container" \
        --destination-path "$dest_prefix" \
        --overwrite true \
        --no-progress \
        --only-show-errors >/dev/null
    else
      az storage blob upload-batch \
        --connection-string "$conn" \
        --source "$src_dir" \
        --destination "$container" \
        --destination-path "$dest_prefix" \
        --overwrite true \
        --no-progress \
        --only-show-errors
    fi
  else
    if [[ "$quiet" == "quiet" ]]; then
      az storage blob upload-batch \
        --connection-string "$conn" \
        --source "$src_dir" \
        --destination "$container" \
        --overwrite true \
        --no-progress \
        --only-show-errors >/dev/null
    else
      az storage blob upload-batch \
        --connection-string "$conn" \
        --source "$src_dir" \
        --destination "$container" \
        --overwrite true \
        --no-progress \
        --only-show-errors
    fi
  fi
}

blob_upload_log() {
  local conn="$1"
  local container="$2"
  local log_file="$3"
  local dest_prefix="${4:-}"
  local log_name="${5:-}"
  local quiet="${6:-}"

  local blob_name="$log_name"
  if [[ -n "${dest_prefix:-}" ]]; then
    blob_name="${dest_prefix%/}/$log_name"
  fi

  if [[ "$quiet" == "quiet" ]]; then
    az storage blob upload \
      --connection-string "$conn" \
      --container-name "$container" \
      --file "$log_file" \
      --name "$blob_name" \
      --overwrite true \
      --only-show-errors >/dev/null 2>&1
  else
    az storage blob upload \
      --connection-string "$conn" \
      --container-name "$container" \
      --file "$log_file" \
      --name "$blob_name" \
      --overwrite true \
      --only-show-errors
  fi
}

blob_zip_html_results() {
  local out_dir="$1"
  local zip_name="${2:-resultados.zip}"

  if [[ -z "${out_dir:-}" ]]; then
    echo "blob_zip_html_results: out_dir requerido"
    return 1
  fi

  if [[ ! -d "$out_dir" ]]; then
    echo "blob_zip_html_results: no existe el directorio '$out_dir'"
    return 1
  fi

  if ! command -v python3 >/dev/null 2>&1; then
    echo "blob_zip_html_results: falta python3"
    return 1
  fi

  python3 - "$out_dir" "$zip_name" "${@:3}" <<'PY'
import os
import sys
import zipfile

out_dir = sys.argv[1]
zip_name = sys.argv[2]
extras = sys.argv[3:]
zip_path = os.path.join(out_dir, zip_name)

html_files = []
for root, _, files in os.walk(out_dir):
    for name in files:
        if name.lower().endswith(".html"):
            html_files.append(os.path.join(root, name))

html_files.sort()

files_to_zip = []
seen = set()

def add_file(full, rel, bucket):
    if rel == zip_name:
        return
    if rel in seen:
        return
    files_to_zip.append((full, rel, bucket))
    seen.add(rel)

for full in html_files:
    rel = os.path.relpath(full, out_dir)
    add_file(full, rel, "html")

for extra in extras:
    if not extra:
        continue
    full = extra
    if not os.path.isabs(full):
        full = os.path.join(out_dir, extra)
    if not os.path.isfile(full):
        continue
    rel = os.path.relpath(full, out_dir)
    add_file(full, rel, "extra")

if not files_to_zip:
    print("blob_zip_html_results: no HTML or extra files found")
    sys.exit(0)

with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED) as zf:
    for full, rel, _ in files_to_zip:
        zf.write(full, rel)

html_count = sum(1 for _, _, bucket in files_to_zip if bucket == "html")
extra_count = sum(1 for _, _, bucket in files_to_zip if bucket == "extra")
print(
    f"blob_zip_html_results: wrote {zip_path} with {len(files_to_zip)} files "
    f"({html_count} html, {extra_count} extra)"
)
PY
}

blob_normalize_prefix() {
  local p="$1"
  p="${p#/}"
  p="${p#/data/}"
  if [[ "$p" != data/* ]]; then
    p="data/${p}"
  fi
  p="${p%/}"
  echo "$p"
}

blob_trim_prefix() {
  local p="$1"
  p="${p#/}"
  p="${p%/}"
  echo "$p"
}
