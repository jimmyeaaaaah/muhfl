#!/bin/bash
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
"$(dirname -- "$SCRIPT_DIR")"/_build/default/bin/muapprox_main.exe \
  --solver mochi --verbose "${@:2}" \
  "$1" > /tmp/stdout_1.txt 2> /tmp/stderr_1.txt