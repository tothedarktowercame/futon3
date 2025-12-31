(ns futon3.gap-store
  "Persist gap artifacts so futon1 (memory) can ingest them later."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)))

(defonce ^:private gap-path (atom "resources/futon1-gaps.edn"))
(def ^:private lock (Object.))

(defn set-path!
  "Override the default gap log (useful for tests)."
  [path]
  (reset! gap-path path))

(defn- ensure-path! []
  (let [file (io/file @gap-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    (when-not (.exists file)
      (spit file "[]\n"))
    file))

(defn load-gaps []
  (let [file (ensure-path!)]
    (with-open [reader (PushbackReader. (io/reader file))]
      (or (edn/read {:eof []} reader) []))))

(defn append-gap!
  "Persist GAP artifact, returning the updated collection."
  [gap]
  (locking lock
    (let [file (ensure-path!)
          current (vec (load-gaps))
          updated (conj current gap)]
      (spit file (with-out-str (pr updated)))
      updated)))
