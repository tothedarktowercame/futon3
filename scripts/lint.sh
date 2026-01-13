#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -gt 0 ]; then
  clj-kondo --lint "$@"
else
  clj-kondo --lint src dev test
fi
