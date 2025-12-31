#!/usr/bin/env bash
# Tiny sandbox for test JVMs

# ---- RAM / CPU limits via ulimit ----
: "${SANDBOX_VMEM:=4000000}"
: "${SANDBOX_RSS:=3000000}"
: "${SANDBOX_CPU:=300}"
ulimit -v "$SANDBOX_VMEM"   # max virtual memory (kB)
ulimit -m "$SANDBOX_RSS"    # max resident set (kB)
ulimit -t "$SANDBOX_CPU"    # max CPU seconds

# ---- Disk-ish limits (optional, comment out if annoying) ----
# ulimit -f 200000   # max file size (blocks of 512 bytes) ≈ 100 MB
# TMPDIR="$PWD/.tmp-sandbox"
# mkdir -p "$TMPDIR"
# export TMPDIR

# ---- JVM heap limit ----
export JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:-"-Xmx2g -Xms512m"}"

# ---- Actual test command goes here ----
# Examples:
# clojure -M:test "$@"
# lein test "$@"
# mvn -q test
# gradle test

: "${SANDBOX_ALIAS:=test}"
clojure -M:${SANDBOX_ALIAS} "$@"
