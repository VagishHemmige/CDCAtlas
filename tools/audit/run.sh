#!/usr/bin/env bash
# Runs the whole-package review's synthetic audit checks in an isolated R
# process. Use a separate, inspected checkout; nothing is installed or
# downloaded here. Outgoing HTTP(S) requests from R are pointed at a proxy
# that does not exist, so a check that forgets to mock a request fails instead
# of reaching CDC or the Census.
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

# No live tests, no Census key, no network, and a throwaway cache.
export CDCATLAS_LIVE_TESTS=false
unset CENSUS_API_KEY
dead_proxy="http://127.0.0.1:9"
export HTTP_PROXY="$dead_proxy" HTTPS_PROXY="$dead_proxy" ALL_PROXY="$dead_proxy"
export http_proxy="$dead_proxy" https_proxy="$dead_proxy" all_proxy="$dead_proxy"
unset NO_PROXY no_proxy
export R_USER_CACHE_DIR="$work_dir/cache"
mkdir -p "$R_USER_CACHE_DIR/R/CDCAtlas"

"${AUDIT_RSCRIPT:-Rscript}" --vanilla "$audit_dir/run.R" \
  "$repo_dir" "$audit_dir/tests" "${1:-}"
