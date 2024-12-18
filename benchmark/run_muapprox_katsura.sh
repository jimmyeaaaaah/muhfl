#!/bin/bash
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
"$(dirname -- "$SCRIPT_DIR")"/_build/default/bin/muapprox_main.exe \
  --try-weak \
  --remove-disjunction \
  --agg \
  --timeout=300 \
  --solver katsura --verbose "${@:2}" \
  "$1" > /tmp/stdout_1.txt 2> /tmp/stderr_1.txt
# --replacer="$(basename "$1" .in)" 
