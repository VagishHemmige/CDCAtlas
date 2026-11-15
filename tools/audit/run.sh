#!/usr/bin/env bash
# Runs the whole-package review's synthetic audit checks in an isolated R
# process. Use a separate, inspected checkout; nothing is installed or
# downloaded here, and no live request can run.
#
#   bash tools/audit/run.sh            # every file in tools/audit/tests/
#   bash tools/audit/run.sh <pattern>  # files whose names match <pattern>
#
# AUDIT_RSCRIPT may name an Rscript executable.
set -euo pipefail

audit_dir="$(cd "$(dirname "$0")" && pwd)"
repo_dir="$(cd "$audit_dir/../.." && pwd)"
work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

# No startup files, for this process and any child R process.
: > "$work_dir/empty"
export R_ENVIRON="$work_dir/empty" R_ENVIRON_USER="$work_dir/empty"
export R_PROFILE="$work_dir/empty" R_PROFILE_USER="$work_dir/empty"

# No live requests, no Census key, and a throwaway cache.
export CDCATLAS_LIVE_TESTS=false
unset CENSUS_API_KEY
export R_USER_CACHE_DIR="$work_dir/cache"
mkdir -p "$R_USER_CACHE_DIR"

"${AUDIT_RSCRIPT:-Rscript}" --vanilla "$audit_dir/run.R" \
  "$repo_dir" "$audit_dir/tests" "${1:-}"
