#!/usr/bin/env bash

set -euo pipefail

project_root="$(git rev-parse --show-toplevel)"
artifact_dir="${1:-${project_root}/release}"
check_dir="${2:-${project_root}/check}"

mkdir -p "${artifact_dir}" "${check_dir}"
artifact_dir="$(cd "${artifact_dir}" && pwd -P)"
check_dir="$(cd "${check_dir}" && pwd -P)"

shopt -s nullglob
existing_artifacts=("${artifact_dir}"/*)
if (( ${#existing_artifacts[@]} > 0 )); then
  echo "Artifact directory is not empty: ${artifact_dir}" >&2
  exit 1
fi

package_name="$(
  Rscript -e 'cat(read.dcf("DESCRIPTION", fields = "Package")[[1L]])'
)"
build_dir="$(mktemp -d)"
trap 'rm -rf "${build_dir}"' EXIT

(
  cd "${build_dir}"
  R CMD build --compact-vignettes=gs+qpdf "${project_root}"
)

archives=("${build_dir}/${package_name}"_*.tar.gz)
if (( ${#archives[@]} != 1 )); then
  echo "Expected one source archive, found ${#archives[@]}." >&2
  exit 1
fi

archive="${artifact_dir}/$(basename "${archives[0]}")"
mv "${archives[0]}" "${archive}"
(
  cd "${artifact_dir}"
  sha256sum "$(basename "${archive}")" > SHA256SUMS
)

Rscript \
  "${project_root}/.github/scripts/check-source-package.R" \
  "${archive}" \
  "${check_dir}"

(
  cd "${artifact_dir}"
  sha256sum --check SHA256SUMS
)

echo "Fully checked source archive: ${archive}"
