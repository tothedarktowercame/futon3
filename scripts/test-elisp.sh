#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="${EMACS:-emacs}"
ERT_SELECTOR="${ERT_SELECTOR:-t}"
ELISP_TEST_FILES="${ELISP_TEST_FILES:-}"
case "$ERT_SELECTOR" in
  \(*|\'* )
    SELECTOR_EXPR="$ERT_SELECTOR" ;;
  *)
    SELECTOR_EXPR="(quote ${ERT_SELECTOR})" ;;
esac

LOAD_FILES=()
if [[ -n "${ELISP_TEST_FILES}" ]]; then
  IFS=',' read -r -a LOAD_FILES <<< "${ELISP_TEST_FILES}"
else
  LOAD_FILES=("aob-chatgpt-test.el" "flexiarg-test.el")
fi

LOAD_ARGS=()
for file in "${LOAD_FILES[@]}"; do
  LOAD_ARGS+=(-l "${ROOT_DIR}/test/elisp/${file}")
done

${EMACS_BIN} -Q --batch \
  -L "${ROOT_DIR}/test/elisp" \
  -L "${ROOT_DIR}/contrib" \
  "${LOAD_ARGS[@]}" \
  --eval "(ert-run-tests-batch-and-exit ${SELECTOR_EXPR})"
