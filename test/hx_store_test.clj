(ns hx-store-test
  (:require [clojure.test :refer :all]
            [futon3.hx.store :as store]))

(deftest fake-session-replay
  (let [tmp (doto (java.io.File/createTempFile "hx-store" ".edn")
              (.delete)
              (.deleteOnExit))
        original (store/current-log-path)
        artifact {:artifact/id "futon3/src/f2/router.clj"
                  :artifact/type :clojure
                  :artifact/title "Router dispatch"
                  :artifact/path "src/f2/router.clj"
                  :artifact/hash "sha256:deadbeef"
                  :artifact/registered "2026-01-06T00:00:00Z"}
        anchor {:anchor/id "dispatch"
                :anchor/artifact "futon3/src/f2/router.clj"
                :anchor/kind :defn
                :anchor/selector {:kind :regex :pattern "\\(defn dispatch"}
                :anchor/line 42
                :anchor/text "(defn dispatch [router type client envelope] ..."}
        link {:link/id "L-1"
              :link/from {:artifact/id "futon3/src/f2/router.clj" :anchor/id "dispatch"}
              :link/to {:artifact/id "futon3/src/f2/ui.clj" :anchor/id "handle"}
              :link/type :uses
              :link/status :suggested
              :link/confidence 0.72
              :link/agent :codex
              :link/rationale "dispatch calls handle"}
        entries [{:op :artifact/register :artifact artifact}
                 {:op :anchors/replace :artifact/id "futon3/src/f2/router.clj" :anchors [anchor]}
                 {:op :link/suggest :link link}
                 {:op :link/accept :link/id "L-1" :link/decided "2026-01-06T00:01:00Z" :link/decided-by :joe}]]
    (try
      (store/set-log-path! (.getAbsolutePath tmp))
      (store/reset-state!)
      (doseq [entry entries]
        (store/append-entry! entry))
      (is (.exists tmp))
      (store/reset-state!)
      (let [result (store/load-log!)]
        (is (= true (:ok result)))
        (is (= 4 (:count result)))
        (is (= artifact (get-in (store/state) [:artifacts "futon3/src/f2/router.clj"])))
        (is (= [anchor] (get-in (store/state) [:anchors "futon3/src/f2/router.clj"])))
        (is (= :accepted (get-in (store/state) [:links "L-1" :link/status]))))
      (finally
        (store/set-log-path! original)))))
