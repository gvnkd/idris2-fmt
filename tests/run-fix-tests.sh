#!/bin/sh
# Test runner for --fix-indentation mode
# Usage: run-fix-tests.sh <formatter-binary>

FMT="${1:-./build/exec/idris2-fmt}"
TESTDIR="tests/FixIndentation"

FAILED=0
PASSED=0

for input in "$TESTDIR"/*.idr; do
  case="$(basename "$input" .idr)"
  # Skip expected files
  case "$case" in
    *.expected) continue ;;
  esac
  expected="$TESTDIR/$case.expected.idr"
  
  echo "=== $case ==="
  
  if [ ! -f "$expected" ]; then
    echo "  SKIP: no expected file"
    continue
  fi
  
  # Run fix-indentation and compare
  result=$("$FMT" --fix-indentation "$input" 2>/dev/null)
  if [ $? -ne 0 ]; then
    echo "  FAIL: formatter exited with error"
    FAILED=1
    continue
  fi
  
  expected_content=$(cat "$expected")
  
  if [ "$result" = "$expected_content" ]; then
    echo "  PASS"
    PASSED=$((PASSED + 1))
  else
    echo "  FAIL"
    echo "--- expected ---"
    cat "$expected"
    echo "--- got ---"
    echo "$result"
    FAILED=1
  fi
done

echo ""
if [ "$FAILED" -eq 0 ]; then
  echo "All $PASSED tests passed."
  exit 0
else
  echo "Some tests failed."
  exit 1
fi
