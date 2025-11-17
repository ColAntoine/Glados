#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
BIN="${ROOT}/../glados"
fail=0

for f in "${ROOT}"/cases/*.scm; do
  base="$(basename "$f")"
  echo -n "TEST $base ... "
  out="$("$BIN" < "$f" 2>&1 || true)"
  rc=$?
  if [[ "$base" == 08_error_unbound_variable.scm ]]; then
    if [ $rc -ne 84 ]; then
      echo "FAIL (rc=$rc, expected 84)"
      fail=1
      continue
    fi
    echo "OK (error)"
  else
    if [ $rc -ne 0 ]; then
      echo "FAIL (rc=$rc, expected 0)"
      fail=1
      continue
    fi
    echo "OK"
  fi
done

if [ $fail -ne 0 ]; then
  echo "SOME TESTS FAILED"
  exit 1
fi
echo "ALL OK"
exit 0
