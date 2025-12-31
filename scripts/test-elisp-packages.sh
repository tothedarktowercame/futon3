#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="${EMACS:-emacs}"
ERT_SELECTOR="${ERT_SELECTOR:-t}"

case "$ERT_SELECTOR" in
  \(*|\'* )
    SELECTOR_EXPR="$ERT_SELECTOR" ;;
  *)
    SELECTOR_EXPR="(quote ${ERT_SELECTOR})" ;;
esac

if [[ "$#" -gt 0 ]]; then
  PACKAGES=("$@")
elif [[ -n "${PACKAGE:-}" ]]; then
  IFS=',' read -r -a PACKAGES <<< "${PACKAGE}"
else
  PACKAGES=(embedding hud sessions arxana bridge)
fi

declare -a TEST_FILES=()

for pkg in "${PACKAGES[@]}"; do
  case "$pkg" in
    embedding)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-embedding-test.el") ;;
    hud)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-hud-test.el") ;;
    sessions)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-sessions-test.el") ;;
    arxana)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-arxana-compat-test.el") ;;
    bridge)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-bridge-test.el") ;;
    aob)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/aob-chatgpt-test.el") ;;
    all)
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-embedding-test.el")
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-hud-test.el")
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-sessions-test.el")
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-arxana-compat-test.el")
      TEST_FILES+=("${ROOT_DIR}/test/elisp/futon3-bridge-test.el")
      TEST_FILES+=("${ROOT_DIR}/test/elisp/aob-chatgpt-test.el") ;;
    *)
      echo "Unknown package: ${pkg}" >&2
      exit 1 ;;
  esac
done

${EMACS_BIN} -Q --batch \
  -L "${ROOT_DIR}/test/elisp" \
  -L "${ROOT_DIR}/contrib" \
  $(printf -- "-l %q " "${TEST_FILES[@]}") \
  --eval "(ert-run-tests-batch-and-exit ${SELECTOR_EXPR})"
