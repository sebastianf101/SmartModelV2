#!/usr/bin/env bash
set -euo pipefail

version_bsm="10.3"
bsm_dir="${BSM_DIR:-$HOME/Documents/besmart/${version_bsm}}"

cd "${bsm_dir}"

rm -rf Auxil Logs Reportes Trabajo
mkdir -p Auxil Logs Reportes Trabajo
