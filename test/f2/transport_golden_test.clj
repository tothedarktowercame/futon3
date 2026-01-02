(ns f2.transport-golden-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [f2.adapters.mock :as mock]
            [f2.transport :as transport]))

(defn- make-state []
  {:config (atom {:repl {:mode :off}
                  :run-id-seed "RUN-GOLDEN"
                  :clock {:fixed "2026-01-01T00:00:00Z"}})
   :clients (atom {"C-1" {:id "C-1"
                           :name "anon"
                           :caps #{"check"}
                           :connected? true
                           :remote-addr "127.0.0.1"}})
   :history (atom [])
   :adapters (atom (mock/adapters))
   :router (atom nil)})

(defn- load-ndjson [path]
  (->> (slurp path)
       str/split-lines
       (remove str/blank?)))

(defn- normalize-reply [reply]
  (-> reply
      json/encode
      (json/parse-string true)
      (dissoc :log)))

(deftest golden-check-transcript-replays
  (let [state (make-state)
        request-lines (load-ndjson "test/fixtures/transport/golden-check-request.ndjson")
        expected-lines (load-ndjson "test/fixtures/transport/golden-check-reply.ndjson")
        envelopes (map #(json/parse-string % true) request-lines)
        expected (map #(json/parse-string % true) expected-lines)
        actual (map (fn [envelope]
                      (-> (transport/apply-envelope! state "C-1" envelope)
                          :reply
                          normalize-reply))
                    envelopes)]
    (is (= (map #(dissoc % :log) expected)
           actual))))
