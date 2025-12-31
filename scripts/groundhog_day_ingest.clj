#!/usr/bin/env clojure
(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

(def transcript
  (-> (io/file "../futon5/resources/demos/groundhog_day_raw.json")
      slurp
      (json/parse-string true)
      (get-in [:choices 0 :message :content])))

(def segments
  (->> (str/split transcript #"\n---\n")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn segment [title]
  (some #(when (str/starts-with? % title) %) segments))

(def turn1 (segment "### Turn 1"))
(def turn2 (segment "### Turn 2"))
(def turn3 (segment "### Turn 3"))
(def clock-out (last segments))

(defn events []
  [{:type :hello
    :payload {:msg-id "ghd-hello"
              :t "2025-04-07T06:00:00Z"
              :actor "hud"
              :verb "hello"
              :object "prototype3"
              :notes turn1}}
   {:type :event
    :payload {:msg-id "ghd-event"
              :t "2025-04-07T06:30:00Z"
              :actor "hud"
              :verb "replay"
              :object "ndjson"
              :notes turn2}}])

(defn workday []
  {:type :workday
   :payload {:msg-id "ghd-workday"
             :t "2025-04-07T06:35:00Z"
             :activity "Prototype 3 backlog sweep"
             :notes (str turn2 "\n\nHUD:\n" (or (segment "**HUD Detail") ""))
             :check {:pattern/id "pattern-3.2"
                     :context "Groundhog Day loop"
                     :evidence [turn2]}}})

(defn check []
  {:type :check
   :payload {:msg-id "ghd-check"
             :pattern/id "pattern-3.2"
             :context "Groundhog Day backlog"
             :evidence [turn2]}})

(defn bye []
  {:type :bye
   :payload {:msg-id "ghd-bye"
             :notes turn3}})

(def frames
  (concat (events)
          [(workday)]
          [(check)]
          [(bye)]))

(with-open [w (io/writer "dev/groundhog_day.ndjson")]
  (doseq [frame frames]
    (.write w (str (json/generate-string frame) "\n"))))

(spit "dev/groundhog_day_clockout.txt" clock-out)

(println "Wrote" (count frames) "frames to dev/groundhog_day.ndjson")
(println "Saved clock-out notes to dev/groundhog_day_clockout.txt")
