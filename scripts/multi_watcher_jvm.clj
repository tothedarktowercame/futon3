#!/usr/bin/env clojure
;; JVM wrapper for the multi-repo watcher.
;;
;; This runs the existing watcher logic on a full JVM so WatchService-based
;; event delivery is available, while keeping the substrate semantics in the
;; shared multi_watcher.clj implementation.

(load-file "/home/joe/code/futon3/scripts/multi_watcher.clj")

(apply -main (concat ["--watch-mode" "watchservice"] *command-line-args*))
