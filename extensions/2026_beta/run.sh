#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: extensions/2026_beta/run.sh /path/to/duma-2026-csv-2026-09-30.zip" >&2
  exit 2
fi
repo_dir="$(cd "$(dirname "$0")/../.." && pwd)"
archive="$1"
python3 - "$archive" <<'PY'
import hashlib, sys
expected = "e1fbff4c4994e8ea8d74b3a73685c7be4c2d418d1573b446e643be42cf0ee87c"
digest = hashlib.sha256()
with open(sys.argv[1], "rb") as source:
    for block in iter(lambda: source.read(1024 * 1024), b""):
        digest.update(block)
actual = digest.hexdigest()
if actual != expected:
    raise SystemExit(f"Source archive SHA-256 mismatch: {actual}")
PY
stage="$(mktemp -d)"
mkdir -p "$stage/data/raw_votes" "$stage/data/crosswalks" "$stage/scripts"
ln -s "$repo_dir"/data/raw_votes/* "$stage/data/raw_votes/"
unzip -q "$archive" 'protocols.csv' 'party_results.csv' 'precincts.csv' -d "$stage/snapshot"
ln -s "$stage/snapshot" "$stage/data/raw_votes/2026_snapshot_2026-09-30"
cp "$repo_dir/extensions/2026_beta/pipeline/01b_read_2026.R" "$stage/scripts/"
cp "$repo_dir/extensions/2026_beta/pipeline/tik_recode_2026.csv" "$stage/data/crosswalks/"
cd "$stage"
RED_CLEAN_SCRIPT="$repo_dir/extensions/2026_beta/pipeline/01_clean_merge_elections_2026.R" \
  Rscript -e 'library(tidyverse); library(stringi); source(Sys.getenv("RED_CLEAN_SCRIPT"), encoding="UTF-8")'
RED_BASELINE_RDS="$repo_dir/data/red.Rds" Rscript "$repo_dir/extensions/2026_beta/pipeline/export.R"
python3 "$repo_dir/extensions/2026_beta/build_release.py" \
  --inputs "$stage/data" --out "$repo_dir/extensions/2026_beta/release"
python3 "$repo_dir/extensions/2026_beta/build_swing.py" \
  --inputs "$stage/data" --out "$repo_dir/extensions/2026_beta/release"
echo "Isolated working directory: $stage"
