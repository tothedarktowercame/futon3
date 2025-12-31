(ns futon3.vitality.git
  "Utilities for loading git contribution summaries into FUTON3 HUD components."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def default-summary-path "resources/vitality/git_summary.edn")

(defn load-summary
  "Read the git summary EDN produced by `scripts.git-activity-summary`.
  Optional PATH overrides the default location."
  ([] (load-summary default-summary-path))
  ([path]
   (with-open [r (io/reader path)]
     (edn/read r))))

(defn daily-grid
  "Return the heatmap day vector (spans the configured window)."
  [summary]
  (get-in summary [:heatmap :days]))

(defn latest-active-day
  "Find the most recent day with commits (>0)."
  [summary]
  (get summary :last-active))

(defn quiet-days
  "How many days since the last commit within the window?"
  [summary]
  (get-in summary [:last-active :quiet-days] 0))

(defn streak
  "Return {:current n :longest n}."
  [summary]
  (:streak summary))

(defn sphere-share
  "Vector describing commits per sphere sorted descending."
  [summary]
  (:spheres summary))

(defn repo-share
  "Vector describing per-repo totals sorted descending."
  [summary]
  (:repos summary))

(defn dominant-sphere
  "Return the most active sphere in the current window."
  [summary]
  (some-> (sphere-share summary) first :sphere))
