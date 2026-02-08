(ns futon3.agency.codex-mirror
  "Mirror active Codex JSONL sessions to a remote Futon3 lab upload endpoint."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Version WebSocket WebSocket$Listener WebSocketHandshakeException)
           (java.util.concurrent ExecutionException)
           (java.util.concurrent CompletableFuture TimeUnit)))

(defn- env-int [key default]
  (or (try
        (some-> (System/getenv key) str/trim not-empty Long/parseLong)
        (catch Exception _e nil))
      default))

(defn- env-bool [key default]
  (let [value (some-> (System/getenv key) str/trim str/lower-case)]
    (cond
      (nil? value) default
      (contains? #{"1" "true" "yes"} value) true
      (contains? #{"0" "false" "no"} value) false
      :else default)))

(def ^:private default-config
  {:enabled? (env-bool "AGENCY_CODEX_MIRROR" false)
   :upload-url (or (System/getenv "AGENCY_CODEX_UPLOAD_URL")
                   (System/getenv "CODEX_UPLOAD_URL"))
   :scan-interval-ms (env-int "AGENCY_CODEX_SCAN_INTERVAL_MS" 10000)
   :active-window-ms (* 60 (env-int "AGENCY_CODEX_ACTIVE_MINUTES" 60) 1000)
   :originator (or (System/getenv "AGENCY_CODEX_ORIGINATOR")
                   "agency")})

(defonce ^:private !state (atom {:running? false :watchers {}}))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- now-iso []
  (str (java.time.Instant/ofEpochMilli (now-ms))))

(defn- normalize-project-path [path]
  (let [home (System/getProperty "user.home")]
    (if (and path home (str/starts-with? path home))
      (str "~" (subs path (count home)))
      path)))

(defn- read-first-jsonl [file]
  (try
    (with-open [reader (io/reader file)]
      (loop []
        (when-let [line (.readLine reader)]
          (if (str/blank? line)
            (recur)
            (let [parsed (try
                           (json/parse-string line true)
                           (catch Exception _e ::invalid))]
              (if (identical? parsed ::invalid)
                (recur)
                parsed))))))
    (catch Exception _e
      nil)))

(defn- codex-root []
  (io/file (System/getProperty "user.home") ".codex" "sessions"))

(defn- scan-codex-sessions
  [active-window-ms]
  (let [root (codex-root)
        cutoff (- (now-ms) active-window-ms)]
    (when (.exists root)
      (->> (file-seq root)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".jsonl")))
           (map (fn [f]
                  (let [path (.getAbsolutePath f)
                        modified (.lastModified f)
                        active? (> modified cutoff)
                        meta (read-first-jsonl f)
                        payload (:payload meta)
                        session-id (or (:id payload)
                                       (str/replace (.getName f) #"\.jsonl$" ""))
                        cwd (normalize-project-path (:cwd payload))
                        project (or cwd "codex")
                        originator (:originator payload)]
                    {:id session-id
                     :path path
                     :modified modified
                     :active? active?
                     :cwd cwd
                     :project project
                     :originator originator})))
           (filter :active?)
           (sort-by :modified #(compare %2 %1))))))

(defn- ws-listener []
  (reify WebSocket$Listener
    (onOpen [_ ws] (.request ws 1))
    (onText [_ ws _data _last] (.request ws 1) nil)
    (onBinary [_ ws _data _last] (.request ws 1) nil)
    (onPing [_ ws _data] (.request ws 1) nil)
    (onPong [_ ws _data] (.request ws 1) nil)
    (onClose [_ _ws _status _reason] nil)
    (onError [_ _ws _err] nil)))

(defn- open-websocket [url]
  (let [client (-> (HttpClient/newBuilder)
                   (.version HttpClient$Version/HTTP_1_1)
                   (.build))
        listener (ws-listener)
        fut (-> client
                (.newWebSocketBuilder)
                (.buildAsync (URI/create url) listener))]
    (.get ^CompletableFuture fut 10 TimeUnit/SECONDS)))

(defn- describe-handshake-error [^Throwable err]
  (let [cause (if (instance? ExecutionException err) (.getCause ^ExecutionException err) err)]
    (if (instance? WebSocketHandshakeException cause)
      (let [resp (.getResponse ^WebSocketHandshakeException cause)
            status (when resp (.statusCode resp))
            headers (when resp (.map (.headers resp)))]
        (str (.getName (class cause)) ": " (.getMessage cause)
             (when status (str " status=" status))
             (when (seq headers) (str " headers=" headers))))
      (str (.getName (class err)) ": " (.getMessage err)))))

(defn- send-json! [^WebSocket ws payload]
  (let [text (json/encode payload)
        fut (.sendText ws text true)]
    (.get fut 5 TimeUnit/SECONDS)))

(defn- read-lines [path]
  (with-open [rdr (io/reader path)]
    (vec (line-seq rdr))))

(defn- start-watcher! [cfg session]
  (let [{:keys [upload-url originator]} cfg
        stop-flag (atom false)
        state (atom {:last-count 0
                     :ws nil
                     :last-error nil
                     :last-attempt nil
                     :last-connected nil})]
    (future
      (while (not @stop-flag)
        (try
          (when (and (nil? (:ws @state)) (seq upload-url))
            (swap! state assoc :last-attempt (now-iso))
            (println "[agency.codex-mirror] connecting" {:session-id (:id session)
                                                         :url upload-url})
            (let [ws (open-websocket upload-url)
                  lines (read-lines (:path session))]
              (reset! state {:last-count (count lines)
                             :ws ws
                             :last-error nil
                             :last-attempt (:last-attempt @state)
                             :last-connected (now-iso)})
              (send-json! ws {:type "init"
                              :session-id (:id session)
                              :project (:project session)
                              :source "codex"
                              :originator (or originator (:originator session) "agency")
                              :cwd (:cwd session)
                              :events lines})))

          (when-let [ws (:ws @state)]
            (let [lines (read-lines (:path session))
                  last-count (:last-count @state)
                  current-count (count lines)]
              (when (> current-count last-count)
                (doseq [line (subvec lines last-count)]
                  (send-json! ws {:type "event"
                                  :session-id (:id session)
                                  :event line}))
                (swap! state assoc :last-count current-count))))

          (Thread/sleep 500)
          (catch Exception e
            (let [err (describe-handshake-error e)]
              (println "[agency.codex-mirror] watcher error" {:session-id (:id session)
                                                              :err err})
              (swap! state assoc :ws nil :last-error err))
            (Thread/sleep 1000)))))
    {:stop stop-flag
     :state state
     :session session}))

(defn- reconcile-watchers! [cfg]
  (let [active (scan-codex-sessions (:active-window-ms cfg))
        active-ids (set (map :id active))]
    (swap! !state
           (fn [st]
             (let [watchers (:watchers st)
                   to-stop (remove active-ids (keys watchers))
                   _ (doseq [sid to-stop]
                       (when-let [{:keys [stop]} (get watchers sid)]
                         (reset! stop true)))
                   watchers (apply dissoc watchers to-stop)
                   to-start (remove (set (keys watchers)) active-ids)
                   new-watchers (reduce (fn [acc sid]
                                          (let [session (first (filter #(= sid (:id %)) active))
                                                watcher (start-watcher! cfg session)]
                                            (assoc acc sid watcher)))
                                        watchers
                                        to-start)]
               (assoc st :watchers new-watchers))))))

(defn start!
  ([] (start! default-config))
  ([cfg]
   (let [cfg (merge default-config cfg)]
     (if-not (and (:enabled? cfg) (seq (:upload-url cfg)))
       (fn [] nil)
        (do
          (swap! !state assoc :running? true)
          (let [runner (future
                         (while (:running? @!state)
                           (reconcile-watchers! cfg)
                           (Thread/sleep (:scan-interval-ms cfg))))]
            (fn []
              (swap! !state assoc :running? false)
              (doseq [[_ {:keys [stop]}] (:watchers @!state)]
                (reset! stop true))
              (when (future? runner)
                (future-cancel runner)))))))))

(defn status []
  (let [{:keys [running? watchers]} @!state]
    {:enabled? (:enabled? default-config)
     :upload-url (:upload-url default-config)
     :running? running?
     :watchers (->> watchers
                    (map (fn [[sid {:keys [state session]}]]
                           {:session-id sid
                            :path (:path session)
                            :project (:project session)
                            :cwd (:cwd session)
                            :last-count (:last-count @state)
                            :connected? (boolean (:ws @state))
                            :last-error (:last-error @state)
                            :last-attempt (:last-attempt @state)
                            :last-connected (:last-connected @state)}))
                    (sort-by :session-id)
                    vec)}))
