(ns futon3.stack.status
  "Aggregate FUTON stack telemetry for Tatami HUD consumers."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3.vitality.git :as git])
  (:import (java.io PushbackReader)
           (java.time DayOfWeek Duration Instant ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.time.temporal TemporalAdjusters)
           (java.util Locale)))

(def ^:private zone (ZoneId/systemDefault))
(def ^:private iso-formatter DateTimeFormatter/ISO_OFFSET_DATE_TIME)
(def ^:private display-formatter
  (DateTimeFormatter/ofPattern "EEE HH:mm" Locale/US))

(def ^:private repo-root
  (-> (io/file ".") .getAbsoluteFile .getCanonicalFile))
(def ^:private parent-root (.getParentFile repo-root))
(def ^:private futon0-root (when parent-root (io/file parent-root "futon0")))
(def ^:private home-root (System/getProperty "user.home"))

(defn- boundary-candidates []
  (let [bases (->> (range 0 8)
                   (map #(format "futon%d" %))
                   (map (fn [name]
                          (io/file home-root "code" name "boundary.edn"))))
        local (io/file repo-root "boundary.edn")
        resources (io/file repo-root "resources" "boundary.edn")]
    (concat [resources local] bases)))

(defn- existing-file
  [& paths]
  (some (fn [path]
          (let [file (cond
                       (nil? path) nil
                       (instance? java.io.File path) path
                       (string? path) (io/file path)
                       :else (io/file path))]
            (when (and file (.exists file))
              (.getCanonicalFile file))))
        paths))

(defn- read-json-file [file]
  (try
    (with-open [r (io/reader file)]
      (json/parse-stream r keyword))
    (catch Exception _ nil)))

(defn- read-edn-file [file]
  (try
    (with-open [r (io/reader file)]
      (edn/read (PushbackReader. r)))
    (catch Exception _ nil)))

(defn- summarize-filesystem [entries]
  (when (seq entries)
    (->> entries
         (map (fn [entry]
                (let [imports (:imports entry)]
                  (cond-> {:label (:label entry)
                           :exists (boolean (:exists entry))
                           :recent-files (:recent_files entry 0)
                           :latest (:latest_mtime entry)
                           :top-children (->> (:top_children entry)
                                              (map (fn [child]
                                                     {:name (:name child)
                                                      :recent (:recent_files child 0)}))
                                              (remove #(zero? (:recent %)))
                                              (take 4)
                                              vec)}
                    imports (assoc :imports {:total (or (:total imports) 0)
                                             :recent (->> (:recent imports)
                                                          (map #(select-keys % [:title :recorded_date :ingested_at :mp3 :copied_to]))
                                                          vec)})))))
         vec)))

(defn- summarize-tatami [entry]
  (when entry
    {:exists (boolean (:exists entry))
     :last-event (:last_event entry)
     :hours-since (:hours_since_last entry)
     :gap-warning (boolean (:gap_warning entry))
     :lookback-hours (:lookback_hours entry)}))

(defn- summarize-storage [entry]
  (when entry
    {:backed-up (boolean (:backed_up entry))
     :needs-backup (boolean (or (:needs_backup entry)
                                (false? (:backed_up entry))))
     :note (:note entry)
     :paths (->> (:paths entry)
                 (map (fn [path]
                        {:label (:label path)
                         :path (:path path)
                         :exists (boolean (:exists path))}))
                 vec)}))

(defn- summarize-futon-activity [entries]
  (when (seq entries)
    (->> entries
         (map (fn [entry]
                {:id (:id entry)
                 :path (:path entry)
                 :exists (boolean (:exists entry))
                 :last-mtime (:last_mtime entry)
                 :hours-since (:hours_since entry)
                 :bucket (:bucket entry)}))
         (sort-by :id)
         vec)))

(defn- vitality-status []
  (when-let [file (existing-file (io/file repo-root "resources" "vitality" "latest_scan.json")
                                 (when futon0-root
                                   (io/file futon0-root "data" "vitality" "latest_scan.json")))]
    (when-let [scan (read-json-file file)]
      {:generated-at (:generated_at scan)
       :lookback-hours (:lookback_hours scan)
       :filesystem (summarize-filesystem (:filesystem scan))
       :tatami (summarize-tatami (:tatami scan))
       :futon-activity (summarize-futon-activity (:futon_activity scan))
       :futon-activity-window-hours (:futon_activity_window_hours scan)
       :storage (summarize-storage (:storage_status scan))})))

(defn- git-status []
  (let [file (io/file repo-root "resources" "vitality" "git_summary.edn")]
    (when (.exists file)
      (try
        (let [summary (git/load-summary (.getPath file))]
          {:generated-at (:generated-at summary)
           :dominant-sphere (some-> (git/dominant-sphere summary) name)
           :quiet-days (git/quiet-days summary)
           :streak (git/streak summary)
           :last-active (git/latest-active-day summary)})
        (catch Exception _ nil)))))

(defn- merge-boundary-entry [prev next]
  (let [titles (or (:missing_evidence_titles next)
                   (:missing_evidence_titles prev))
        total-titles (or (:missing_evidence_total_titles next)
                         (:missing_evidence_total_titles prev))]
    (cond-> (merge prev next)
      titles (assoc :missing_evidence_titles titles)
      total-titles (assoc :missing_evidence_total_titles total-titles))))

(defn- boundary-title-cache []
  (let [resource-file (existing-file (io/file repo-root "resources" "boundary.edn"))]
    (when resource-file
      (->> (read-edn-file resource-file)
           :futons
           (remove nil?)
           (map (fn [entry]
                  [(:id entry)
                   (select-keys entry [:missing_evidence_titles :missing_evidence_total_titles])]))
           (into {})))))

(defn- apply-boundary-title-cache [entry title-cache]
  (let [titles (get title-cache (:id entry))]
    (cond-> entry
      (and titles (nil? (:missing_evidence_titles entry)))
      (assoc :missing_evidence_titles (:missing_evidence_titles titles))
      (and titles (nil? (:missing_evidence_total_titles entry)))
      (assoc :missing_evidence_total_titles (:missing_evidence_total_titles titles)))))

(defn- boundary-status []
  (let [snapshots (->> (boundary-candidates)
                       (map existing-file)
                       (remove nil?)
                       (keep read-edn-file))
        title-cache (boundary-title-cache)
        futons (->> snapshots
                    (mapcat :futons)
                    (reduce (fn [acc entry]
                              (update acc (:id entry) merge-boundary-entry entry))
                            {})
                    vals
                    (map #(apply-boundary-title-cache % title-cache))
                    (sort-by :id))
        generated-at (->> snapshots
                          (keep :generated_at)
                          sort
                          last)
        milestone (->> snapshots
                       (keep :milestone_prototype)
                       sort
                       last)]
    (when (seq futons)
      {:generated-at generated-at
       :milestone-prototype milestone
       :critical (->> futons
                      (filter #(pos? (:missing_evidence % 0)))
                      (sort-by :missing_evidence >)
                      (take 4)
                      (map #(select-keys % [:id :missing_evidence :missing_evidence_titles :prototypes :last_modified :path]))
                      vec)
       :futons (vec futons)})))

(defn- focus-profile-status []
  (when-let [file (existing-file (io/file repo-root "resources" "vitality" "focus_profile.edn"))]
    (read-edn-file file)))

(def ^:private day->enum
  {:monday DayOfWeek/MONDAY
   :tuesday DayOfWeek/TUESDAY
   :wednesday DayOfWeek/WEDNESDAY
   :thursday DayOfWeek/THURSDAY
   :friday DayOfWeek/FRIDAY
   :saturday DayOfWeek/SATURDAY
   :sunday DayOfWeek/SUNDAY})

(def ^:private reminder-specs
  [{:id :weekly-review :label "Weekly review" :weekday :sunday :hour 17}
   {:id :midweek-aob :label "Midweek WIP audit" :weekday :wednesday :hour 12}
   {:id :monthly-audit :label "Monthly audit" :day-of-month 1 :hour 11}
   {:id :tai-chi-wed :label "Tai Chi (Wed)" :weekday :wednesday :hour 18 :minute 30}
   {:id :tai-chi-thu :label "Tai Chi (Thu)" :weekday :thursday :hour 18 :minute 30}])

(defn- clamp-day-of-month [dt day]
  (let [length (.lengthOfMonth (.toLocalDate dt))
        target (max 1 (min length day))]
    (.withDayOfMonth dt target)))

(defn- ->zoned [date hour minute]
  (-> date
      (.withHour hour)
      (.withMinute minute)
      (.withSecond 0)
      (.withNano 0)))

(defn- next-weekly [weekday hour minute]
  (let [now (ZonedDateTime/now zone)
        adjuster (TemporalAdjusters/nextOrSame weekday)
        candidate (-> now (.with adjuster) (->zoned hour minute))]
    (if (.isBefore candidate now)
      (.plusWeeks candidate 1)
      candidate)))

(defn- next-monthly [{:keys [day-of-month hour minute]}]
  (let [now (ZonedDateTime/now zone)
        base (-> now (clamp-day-of-month day-of-month) (->zoned hour minute))]
    (if (.isBefore base now)
      (-> base (.plusMonths 1) (clamp-day-of-month day-of-month) (->zoned hour minute))
      base)))

(defn- due-in-hours [^ZonedDateTime due]
  (let [now (ZonedDateTime/now zone)
        dur (Duration/between now due)]
    (/ (.toMinutes dur) 60.0)))

(defn- reminder-status [hours]
  (cond
    (neg? hours) :overdue
    (< hours 1) :now
    (< hours 6) :imminent
    (< hours 24) :soon
    (< hours 72) :upcoming
    :else :scheduled))

(defn- reminder->due [{:keys [weekday day-of-month hour minute] :as spec}]
  (let [hour (or hour 9)
        minute (or minute 0)]
    (cond
      day-of-month (next-monthly {:day-of-month day-of-month :hour hour :minute minute})
      weekday (when-let [enum (day->enum weekday)]
                (next-weekly enum hour minute))
      :else nil)))

(defn- reminder-statuses []
  (->> reminder-specs
       (map (fn [spec]
              (when-let [due (reminder->due spec)]
                (let [hours (due-in-hours due)]
                  {:id (:id spec)
                   :label (:label spec)
                   :due (.format iso-formatter due)
                   :display (.format display-formatter due)
                   :hours (double hours)
                   :status (reminder-status hours)}))))
       (remove nil?)
       (sort-by :hours)
       vec))

(defn- vitality-warnings [vitality]
  (let [{:keys [filesystem tatami storage futon-activity futon-activity-window-hours]} vitality
        futon-window (or futon-activity-window-hours 168)
        futon-active? (some (fn [entry]
                              (when-let [hours (:hours-since entry)]
                                (< hours futon-window)))
                            futon-activity)
        scanned-at (:generated-at vitality)
        base (cond-> []
               (and (seq futon-activity) (not futon-active?))
               (conj (format "No futon activity recorded in the last %sh. Last scan: %s."
                             futon-window
                             (or scanned-at "unknown")))
               (and tatami (false? (:exists tatami)))
               (conj "Tatami log path missing in vitality scanner.")
               (and tatami (:gap-warning tatami))
               (conj "Tatami activity gap exceeds configured window.")
               (and storage (:needs-backup storage))
               (conj (or (:note storage)
                         "Storage roots are not backed up yet.")))]
    (if (and storage (seq (:paths storage)))
      (into base
            (keep (fn [{:keys [label path exists]}]
                    (when (false? exists)
                      (format "Storage path missing: %s" (or label path))))
                  (:paths storage)))
      base)))

(defn- git-warnings [git]
  (when (and git (number? (:quiet-days git)) (>= (:quiet-days git) 3))
    [(format "Git activity quiet for %d days." (:quiet-days git))]))

(defn- boundary-warnings [_boundary]
  [])

(defn- reminder-warnings [reminders]
  (->> reminders
       (filter #(contains? #{:now :imminent} (:status %)))
       (map #(format "%s due %s" (:label %) (:display %)))
       vec))

(defn- merge-warnings [vitality git boundary reminders]
  (vec (concat (vitality-warnings vitality)
               (git-warnings git)
               (boundary-warnings boundary)
               (reminder-warnings reminders))))

(defn stack-status []
  (let [vitality (vitality-status)
        git (git-status)
        boundary (boundary-status)
        reminders (reminder-statuses)
        focus-profile (focus-profile-status)
        warnings (merge-warnings vitality git boundary reminders)]
    {:generated-at (.toString (Instant/now))
     :vitality vitality
     :git git
     :boundary boundary
     :reminders reminders
     :focus-profile focus-profile
     :warnings warnings}))
