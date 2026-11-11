(ns scripts.build-pattern-index
  "Compatibility entry point for the canonical pattern-index derivation."
  (:require [futon3.pattern-index.derivation :as derivation]))

(defn -main [& args]
  (apply derivation/-main args))
