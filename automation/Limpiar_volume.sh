#!/usr/bin/env bash
set -euo pipefail

version_bsm="10.3"

# determine PROJECT_ROOT (use existing env, then git top-level, then script parent)
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${PROJECT_ROOT:-$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || (cd "$script_dir/.." && pwd))}"

bsm_dir="${BSM_LOCAL_DIR:-${PROJECT_ROOT}/volume_versiones/${version_bsm}}"

# ensure target directory exists
if [ ! -d "${bsm_dir}" ]; then
  echo "ERROR: target directory '${bsm_dir}' does not exist." >&2
  exit 1
fi

# expected folders to remove; abort if none are present to avoid accidental deletions
expected=(Auxil Dev Reportes Trabajo)
found=false
for sub in "${expected[@]}"; do
  if [ -e "${bsm_dir}/${sub}" ]; then
    found=true
    break
  fi
done

if [ "$found" = false ]; then
  echo "ERROR: none of the expected folders (${expected[*]}) were found under '${bsm_dir}'. Aborting." >&2
  echo "Contents of '${bsm_dir}':"
  ls -la "${bsm_dir}"
  exit 1
fi

cd "${bsm_dir}"

rm -rf Auxil Dev Reportes Trabajo
mkdir -p Auxil Dev Reportes Trabajo
