#!/bin/sh
# Test runner for idris2-fmt
# Usage: runtests <formatter-binary>
#
# Workflow per test case:
#   1. Select case from CASES list
#   2. Compile Reference/<case>.idr
#   3. Format Reference/<case>.idr → formatted/<case>.idr
#   4. Compile formatted/<case>.idr
#   5. Check identity: formatted == Reference (idempotency)
#   6. Compile Broken/<case>.idr
#   7. Format Broken/<case>.idr → formatted-broken/<case>.idr
#   8. Compile formatted-broken/<case>.idr
#   9. Check identity: formatted-broken == Reference (convergence)

FMT="${1:-./build/exec/idris2-fmt}"

# Supported test cases. Add new names here as files are added.
CASES="RecordAccess LetAlternatives ImportPublic AsPattern CaseExpr Comments ConstraintArrow DataType DependentPair DoLetAnnot Expr Fixity Forall Functions Gadt IfThenElse Implicit Interface Mutual Operators Record TupleSection WhereClause"

FAILED=0
PASSED=0

compile_idr() {
  local file="$1"
  local label="$2"
  local dir=$(dirname "$file")
  local name=$(basename "$file")
  (cd "$dir" && idris2 --check "$name" > /dev/null 2>&1)
  if [ $? -eq 0 ]; then
    echo "    PASS compile: $label"
    return 0
  else
    echo "    FAIL compile: $label"
    FAILED=1
    return 1
  fi
}

format_file() {
  local input="$1"
  local output="$2"
  local label="$3"
  "$FMT" --stdin < "$input" > "$output" 2>/dev/null
  if [ $? -eq 0 ]; then
    echo "    PASS format:  $label"
    return 0
  else
    echo "    FAIL format:  $label"
    FAILED=1
    return 1
  fi
}

check_identity() {
  local left="$1"
  local right="$2"
  local label="$3"
  if diff -q "$left" "$right" > /dev/null 2>&1; then
    echo "    PASS identity: $label"
    return 0
  else
    echo "    FAIL identity: $label"
    diff -y -W 90 "$left" "$right"
    FAILED=1
    return 1
  fi
}

# Prepare temp dirs
TMPDIR="/tmp/idris2-fmt-tests-$$"
FORMATTED_DIR="$TMPDIR/formatted"
BROKEN_FMT_DIR="$TMPDIR/formatted-broken"
mkdir -p "$FORMATTED_DIR" "$BROKEN_FMT_DIR"

for CASE in $CASES; do
  REF="tests/Reference/${CASE}.idr"
  BRK="tests/Broken/${CASE}.idr"
  FMT_OUT="$FORMATTED_DIR/Reference.idr"
  BRK_OUT="$BROKEN_FMT_DIR/Reference.idr"

  echo "=== $CASE ==="

  # 2. Compile reference
  cp "$REF" "$TMPDIR/Reference.idr"
  compile_idr "$TMPDIR/Reference.idr" "reference" || { echo ""; continue; }

  # 3. Format reference
  format_file "$REF" "$FMT_OUT" "reference" || { echo ""; continue; }

  # 4. Compile formatted
  compile_idr "$FMT_OUT" "formatted reference" || { echo ""; continue; }

  # 5. Check idempotency
  check_identity "$REF" "$FMT_OUT" "idempotency"

  # 6. Compile broken
  cp "$BRK" "$TMPDIR/Reference.idr"
  compile_idr "$TMPDIR/Reference.idr" "broken" || { echo ""; continue; }

  # 7. Format broken
  format_file "$BRK" "$BRK_OUT" "broken" || { echo ""; continue; }

  # 8. Compile formatted broken
  compile_idr "$BRK_OUT" "formatted broken" || { echo ""; continue; }

  # 9. Check convergence
  check_identity "$REF" "$BRK_OUT" "convergence"

  echo ""
done

# Cleanup
rm -rf "$TMPDIR"

if [ "$FAILED" -eq 0 ]; then
  echo "All tests passed."
  exit 0
else
  echo "Some tests failed."
  exit 1
fi
