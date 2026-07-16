#!/usr/bin/env bash

set -euo pipefail

project_root="$(git rev-parse --show-toplevel)"
artifact_dir="${1:?source artifact directory is required}"
check_dir="${2:?check directory is required}"
shift 2
check_arguments=("$@")
if (( ${#check_arguments[@]} == 0 )); then
  check_arguments=("--as-cran")
fi

artifact_dir="$(cd "${artifact_dir}" && pwd -P)"
mkdir -p "${check_dir}"
check_dir="$(cd "${check_dir}" && pwd -P)"

(
  cd "${artifact_dir}"
  sha256sum --check SHA256SUMS
)

shopt -s nullglob
archives=("${artifact_dir}"/*.tar.gz)
if (( ${#archives[@]} != 1 )); then
  echo "Expected one source archive, found ${#archives[@]}." >&2
  exit 1
fi

Rscript \
  "${project_root}/.github/scripts/check-source-package.R" \
  "${archives[0]}" \
  "${check_dir}" \
  "${check_arguments[@]}"

(
  cd "${artifact_dir}"
  sha256sum --check SHA256SUMS
)

echo "Verified and fully checked source archive: ${archives[0]}"
