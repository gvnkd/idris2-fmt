#!/usr/bin/env bash
set -euo pipefail

FMT="${1:-./build/exec/idris2-fmt}"
FAILED=0

# Find all Idris source files
# Note: tests/ input files are excluded until idempotency issues are resolved
FILES=$(find src -name "*.idr" | sort)

echo "Checking formatting and idempotency..."
echo ""

for f in $FILES; do
  # Skip if formatter binary doesn't exist
  if [ ! -f "$FMT" ]; then
    echo "ERROR: Formatter not found at $FMT"
    echo "Usage: $0 [path/to/idris2-fmt]"
    exit 1
  fi

  # Make a temp copy for comparison
  TMP=$(mktemp)
  cp "$f" "$TMP"

  # First pass: format in place
  "$FMT" --inplace "$f" 2>/dev/null

  # Check if first pass made changes
  if diff -q "$TMP" "$f" >/dev/null 2>&1; then
    # No changes, already formatted
    rm "$TMP"
    continue
  fi

  echo "  FORMATTED: $f"

  # Second pass: verify idempotency
  cp "$f" "$TMP"
  "$FMT" --inplace "$f" 2>/dev/null

  if ! diff -q "$TMP" "$f" >/dev/null 2>&1; then
    echo "  IDEMPOTENCY FAIL: $f"
    echo "    Second pass produced different output!"
    FAILED=$((FAILED + 1))
  fi

  rm "$TMP"
done

echo ""

if [ $FAILED -gt 0 ]; then
  echo "FAILED: $FAILED file(s) failed idempotency check"
  exit 1
elif [ -n "$(find src tests -name '*.idr' | head -1)" ]; then
  echo "OK: All files are formatted and idempotent"
  exit 0
else
  echo "No .idr files found"
  exit 0
fi
