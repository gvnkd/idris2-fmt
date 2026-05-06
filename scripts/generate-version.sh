#!/usr/bin/env bash
set -e

# Generate IdrisFmt.Version module from git tag
# Falls back to "unknown" if not in a git repo

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUT_FILE="$PROJECT_ROOT/src/IdrisFmt/Version.idr"

if command -v git >/dev/null 2>&1 && [ -d "$PROJECT_ROOT/.git" ]; then
  VERSION="$(git -C "$PROJECT_ROOT" describe --tags --always 2>/dev/null || echo 'unknown')"
else
  VERSION="unknown"
fi

cat > "$OUT_FILE" << EOF
module IdrisFmt.Version

||| Formatter version, auto-generated from git tag at build time.
export
versionString : String
versionString = "$VERSION"
EOF

echo "Generated IdrisFmt.Version with version: $VERSION"
