#!/bin/sh
# Bulletproof test runner for idris2-fmt
# Usage: runtests <formatter-binary>

FMT="${1:-./build/exec/idris2-fmt}"
REF="tests/Reference.idr"
BROKEN="tests/broken.idr"
TMPDIR="/tmp/idris2-fmt-tests-$$"
mkdir -p "$TMPDIR"
REF_TMP="$TMPDIR/Reference.idr"
BROKEN_TMP="$TMPDIR/Reference.idr"
FAILED=0

compile_idr() {
  local file="$1"
  local dir=$(dirname "$file")
  local name=$(basename "$file")
  local label="$2"
  echo "  Compiling $label..."
  (cd "$dir" && idris2 --check "$name" > /dev/null 2>&1)
  if [ $? -eq 0 ]; then
    echo "    PASS"
  else
    echo "    FAIL"
    FAILED=1
  fi
}

# Step 1: Copy Reference to tmp and compile
echo "Step 1: Compile Reference.idr"
cp "$REF" "$REF_TMP"
compile_idr "$REF_TMP" "Reference.idr"

# Step 2: Format Reference.idr to tmp
echo "Step 2: Format Reference.idr"
"$FMT" --stdin < "$REF" > "$REF_TMP" 2>/dev/null
if [ $? -eq 0 ]; then
  echo "  PASS"
else
  echo "  FAIL: formatter exited with error"
  FAILED=1
fi

# Step 3: Check identity
echo "Step 3: Check identity (formatted == Reference.idr)"
if diff -q "$REF" "$REF_TMP" > /dev/null 2>&1; then
  echo "  PASS"
else
  echo "  FAIL"
  diff "$REF" "$REF_TMP"
  FAILED=1
fi

# Step 3.1: Compile formatted Reference
echo "Step 3.1: Compile formatted Reference.idr"
compile_idr "$REF_TMP" "formatted Reference.idr"

# Step 4: Format broken.idr to tmp (use different tmpdir to avoid conflict)
BROKEN_DIR="/tmp/idris2-fmt-tests-broken-$$"
mkdir -p "$BROKEN_DIR"
BROKEN_TMP="$BROKEN_DIR/Reference.idr"
echo "Step 4: Format broken.idr"
"$FMT" --stdin < "$BROKEN" > "$BROKEN_TMP" 2>/dev/null
if [ $? -eq 0 ]; then
  echo "  PASS"
else
  echo "  FAIL: formatter exited with error"
  FAILED=1
fi

# Step 5: Check identity with Reference
echo "Step 5: Check identity (broken-formatted == Reference.idr)"
if diff -q "$BROKEN_TMP" "$REF" > /dev/null 2>&1; then
  echo "  PASS"
else
  echo "  FAIL"
  diff "$REF" "$BROKEN_TMP"
  FAILED=1
fi

# Step 5.1: Compile formatted broken
echo "Step 5.1: Compile formatted broken.idr"
compile_idr "$BROKEN_TMP" "formatted broken.idr"

# Cleanup
rm -rf "$TMPDIR" "$BROKEN_DIR"

if [ "$FAILED" -eq 0 ]; then
  echo ""
  echo "All tests passed."
  exit 0
else
  echo ""
  echo "Some tests failed."
  exit 1
fi
