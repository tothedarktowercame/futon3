(ns scripts.review-links
  "QA demo: batch review of suggested links with structural checks."
  (:require [clojure.java.io :as io]
            [f0.clock :as clock]
            [futon3.hx.link-types :as link-types]
            [futon3.hx.store :as store]))

(def default-report-path "futon3/logs/hx-review-report.edn")

(defn- anchor-exists? [state ref]
  (let [artifact-id (:artifact/id ref)
        anchor-id (:anchor/id ref)]
    (if (and artifact-id anchor-id)
      (some #(= anchor-id (:anchor/id %)) (get-in state [:anchors artifact-id]))
      true)))

(defn- artifact-exists? [state ref]
  (contains? (get-in state [:artifacts]) (:artifact/id ref)))

(defn- structural-issues [state link]
  (let [from (:link/from link)
        to (:link/to link)
        issues (cond-> []
                 (not (artifact-exists? state from))
                 (conj {:issue :missing-from-artifact :artifact/id (:artifact/id from)})
                 (not (artifact-exists? state to))
                 (conj {:issue :missing-to-artifact :artifact/id (:artifact/id to)})
                 (not (anchor-exists? state from))
                 (conj {:issue :missing-from-anchor :artifact/id (:artifact/id from)
                        :anchor/id (:anchor/id from)})
                 (not (anchor-exists? state to))
                 (conj {:issue :missing-to-anchor :artifact/id (:artifact/id to)
                        :anchor/id (:anchor/id to)})
                 (not (link-types/allowed-type? (:link/type link)))
                 (conj {:issue :unknown-link-type :link/type (:link/type link)}))]
    issues))

(defn- review-link [state link]
  (let [issues (structural-issues state link)]
    {:link-id (:link/id link)
     :status (if (empty? issues) :accept :reject)
     :issues issues
     :link link}))

(defn -main [& _]
  (store/load-log!)
  (let [state (store/state)
        candidates (filter #(= :suggested (:link/status %)) (vals (:links state)))
        reviews (mapv #(review-link state %) candidates)
        report {:generated-at (clock/->iso-string)
                :count (count reviews)
                :accepted (count (filter #(= :accept (:status %)) reviews))
                :rejected (count (filter #(= :reject (:status %)) reviews))
                :reviews reviews}]
    (spit (io/file default-report-path) (pr-str report))
    (println (pr-str {:ok true
                      :report default-report-path
                      :count (:count report)
                      :accepted (:accepted report)
                      :rejected (:rejected report)}))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
