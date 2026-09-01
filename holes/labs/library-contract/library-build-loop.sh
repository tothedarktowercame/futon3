#!/usr/bin/env bash
# library-build-loop.sh -- continuous build of the pattern library rationale layer from the ledger:
#   unblock -> work (one row, seat A) -> review (one row, seat B != A) -> publish (when the
#   registries are clear) -> repeat, until nothing is open, unreviewed, or unblockable.
# Runs unattended. Log: runs/build-loop.log (tail it; voxterm shows the process tree).
# Seats: WORK_SEAT=claude|codex (default claude), REVIEW_SEAT the other. Author != reviewer
# is enforced by using different tools for the two phases within an iteration.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$HERE/runs/build-loop.log"; mkdir -p "$HERE/runs"
WORK_SEAT="${WORK_SEAT:-claude}"; REVIEW_SEAT="${REVIEW_SEAT:-codex}"; SLEEP="${SLEEP:-20}"; MAX_ITER="${MAX_ITER:-60}"
log() { echo "[$(date -u '+%H:%M:%S')] $*" | tee -a "$LOG"; }
# Every way out of this loop bells claude-1 (the Emacs seat) with the reason and
# the tail of the log, so a stopped loop is a message in that session, not a
# discovery the next day (Joe, 2026-09-01: "if it stops, I feel like you should
# get an error message sent through to this session so that you could fix it").
NOTIFIED=0
notify() { # $1 reason
  [ "$NOTIFIED" = 1 ] && return 0; NOTIFIED=1
  { echo "From library-build-loop.sh, $(date -u '+%Y-%m-%d %H:%M:%S') UTC. STOPPED: $1"
    echo "Ledger counts: $(bb "$HERE/build_step.bb" counts 2>/dev/null)"
    echo "Last 40 log lines ($LOG):"; tail -40 "$LOG"
    echo; echo "To restart after fixing: cd $HERE && nohup ./library-build-loop.sh > /tmp/library-build-nohup.out 2>&1 &"
  } > /tmp/library-build-notify.md
  (cd "$HOME/code" && python3 futon3c/scripts/agency_send.py --from library-build-loop --to claude-1 --kind bell < /tmp/library-build-notify.md >> "$LOG" 2>&1) || log "notify: bell to claude-1 failed"
}
trap 'rc=$?; notify "exit code $rc (trap)"' EXIT
STALL_ID=""; STALL_N=0
stall_check() { # $1 next-open id; three iterations on the same open row with nothing reviewed = stalled
  if [ "$1" = "$STALL_ID" ]; then STALL_N=$((STALL_N+1)); else STALL_ID="$1"; STALL_N=1; fi
  if [ "$STALL_N" -ge 3 ] && [ "$1" != "NONE" ]; then log "stalled: $1 has been next-open for $STALL_N iterations without changing status"; notify "stalled on row $1 (3 iterations, no status change)"; exit 3; fi
}
run_seat() { # $1 seat, $2 prompt file, $3 label
  local seat="$1" prompt="$2" label="$3"
  log "$label: $seat starting"
  case "$seat" in
    claude) (cd "$HOME/code" && timeout 7200 claude -p --permission-mode bypassPermissions "$(cat "$prompt")" >> "$LOG" 2>&1) ;;
    codex)  (cd "$HOME/code" && timeout 5400 codex exec --skip-git-repo-check --sandbox danger-full-access "$(cat "$prompt")" >> "$LOG" 2>&1) ;;
    *) log "unknown seat $seat"; return 2 ;;
  esac
  local rc=$?; log "$label: $seat exit=$rc"; return $rc
}
ledger_ok() { bb "$HERE/worklist_check.bb" "$HERE/worklist.edn" > /tmp/library-build-check.out 2>&1; local rc=$?; grep -vE 'WARNING|^worklist_check:\s+(M|\?\?) ' /tmp/library-build-check.out | tail -1 | tee -a "$LOG"; return $rc; }
publish() {
  # library-contract has no registry publish; report the graph's growth instead.
  local counts
  counts=$(cd "$HOME/code/futon3/library" && echo "why=$(grep -rl '^@why ' --include='*.flexiarg' . | wc -l) how=$(grep -rl '^@how ' --include='*.flexiarg' . | wc -l) posthoc=$(grep -rl '^@why-posthoc ' --include='*.flexiarg' . | wc -l) see-also=$(grep -rl '^@see-also ' --include='*.flexiarg' . | wc -l)")
  log "graph: $counts"
}
log "=== library-build-loop start (work=$WORK_SEAT review=$REVIEW_SEAT) ==="
i=0
while [ $i -lt "$MAX_ITER" ]; do
  i=$((i+1))
  ledger_ok || { log "ledger invalid; stopping"; notify "ledger invalid before work"; exit 1; }
  bb "$HERE/build_step.bb" unblock | tee -a "$LOG"
  if [ -n "$(bb "$HERE/build_step.bb" unblock)" ]; then (cd "$HOME/code/futon3" && git add holes/labs/library-contract/worklist.edn && git commit -q -m "worklist: library-build-loop unblocked rows whose :depends-on are done"); fi
  next="$(bb "$HERE/build_step.bb" next-open)"; unrev="$(bb "$HERE/build_step.bb" unreviewed)"
  log "iteration $i: next-open=$next unreviewed=[$unrev] counts=$(bb "$HERE/build_step.bb" counts)"
  [ -z "$unrev" ] && stall_check "$next"
  if [ "$next" = "NONE" ] && [ -z "$unrev" ]; then log "nothing open or unreviewed; done"; publish; notify "DONE: nothing open or unreviewed"; break; fi
  if [ "$next" != "NONE" ]; then
    # The loop chose the row (build_step.bb priorities); the prompt must say so,
    # or the seat takes the first open row in ledger order (iteration 1 took I1
    # when RUN12 was meant). The prompt is composed per iteration.
    { echo "ROW TO DO THIS INVOCATION: $next -- the build loop chose it by priority; take this row and no other. If it carries :loop-mode :one-slice-per-invocation, do its next slice."; echo; cat "$HERE/worklist-prompt.md"; } > /tmp/library-build-work-prompt.md
    run_seat "$WORK_SEAT" /tmp/library-build-work-prompt.md "work($next)"
    ledger_ok || { log "ledger invalid after work; stopping"; notify "ledger invalid after work"; exit 1; }
  fi
  unrev="$(bb "$HERE/build_step.bb" unreviewed)"
  if [ -n "$unrev" ]; then
    { echo "ROWS AWAITING REVIEW: $unrev -- review the FIRST of these."; echo; cat "$HERE/review-prompt.md"; } > /tmp/library-build-review-prompt.md
    run_seat "$REVIEW_SEAT" /tmp/library-build-review-prompt.md "review($unrev)"
    ledger_ok || { log "ledger invalid after review; stopping"; notify "ledger invalid after review"; exit 1; }
  fi
  publish
  sleep "$SLEEP"
done
log "=== library-build-loop end after $i iterations ==="; notify "ended after $i iterations (MAX_ITER=$MAX_ITER or done)"
