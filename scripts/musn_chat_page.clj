#!/usr/bin/env clojure
;; Poll MUSN chat and page an agent via Agency for each new chat message.
;;
;; Usage:
;;   ./scripts/musn-chat-page --room futon --agent-id codex
;;
;; Env (infrastructure - shared):
;;   FUTON3_MUSN_URL / MUSN_URL  (default http://localhost:6065)
;;   AGENCY_URL                  (default http://localhost:7070)
;;
;; Env (per-instance - typically passed via CLI):
;;   MUSN_PAGE_ROOM    (default lab)
;;   MUSN_PAGE_AGENT   (required if not passed)
;;   MUSN_PAGE_POLL    (default 2.0 seconds)
;;   MUSN_PAGE_TIMEOUT (default 30000 ms)
;;   MUSN_PAGE_IGNORE  (comma-separated nicks to ignore)
(ns musn-chat-page
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]))

(defn env
  "Return first non-blank env var value, or default.
  Usage: (env \"NAME\" \"default\") or (env \"NAME1\" \"NAME2\" ... \"default\")"
  ([name default] (or (System/getenv name) default))
  ([name1 name2 default]
   (or (not-empty (System/getenv name1))
       (not-empty (System/getenv name2))
       default))
  ([name1 name2 name3 & rest]
   (let [default (last (cons name3 rest))
         names (concat [name1 name2 name3] (butlast rest))]
     (or (some->> names
                  (map #(System/getenv %))
                  (map not-empty)
                  (remove nil?)
                  first)
         default))))

(defn parse-args [args]
  (loop [args args
         opts {:musn-url (env "MUSN_PAGE_MUSN_URL" "FUTON3_MUSN_URL" "MUSN_URL" "http://localhost:6065")
               :agency-url (env "MUSN_PAGE_AGENCY_URL" "AGENCY_URL" "http://localhost:7070")
               :room (env "MUSN_PAGE_ROOM" "lab")
               :agent-id (env "MUSN_PAGE_AGENT" "")
               :poll-interval (Double/parseDouble (env "MUSN_PAGE_POLL" "2.0"))
               :timeout-ms (Long/parseLong (env "MUSN_PAGE_TIMEOUT" "30000"))
               :ignore-nicks (env "MUSN_PAGE_IGNORE" "")
               :cursor-file (env "MUSN_PAGE_CURSOR_FILE" "")}]
    (if (empty? args)
      (let [opts (update opts :ignore-nicks
                         (fn [s]
                           (->> (str/split (or s "") #",")
                                (map str/trim)
                                (remove str/blank?)
                                set)))]
        ;; Default: ignore the agent's own nick unless explicitly overridden.
        (cond
          (contains? (:ignore-nicks opts) "none")
          (update opts :ignore-nicks disj "none")

          (str/blank? (:agent-id opts))
          opts

          :else
          (update opts :ignore-nicks conj (:agent-id opts))))
      (let [[flag val & rest] args]
        (case flag
          "--musn-url" (recur rest (assoc opts :musn-url val))
          "--agency-url" (recur rest (assoc opts :agency-url val))
          "--room" (recur rest (assoc opts :room val))
          "--agent-id" (recur rest (assoc opts :agent-id val))
          "--poll-interval" (recur rest (assoc opts :poll-interval (Double/parseDouble val)))
          "--timeout-ms" (recur rest (assoc opts :timeout-ms (Long/parseLong val)))
          "--ignore-nicks" (recur rest (assoc opts :ignore-nicks val))
          "--cursor-file" (recur rest (assoc opts :cursor-file val))
          (recur rest opts))))))

(defn- default-cursor-file [room agent-id]
  (let [room (or room "room")
        agent-id (or agent-id "agent")]
    (str "/tmp/musn-chat-page.cursor." room "." agent-id)))

(defn- read-cursor [path]
  (try
    (when (seq path)
      (some-> (slurp path) str/trim not-empty Long/parseLong))
    (catch Exception _ nil)))

(defn- write-cursor! [path cursor]
  (try
    (when (seq path)
      (spit path (str cursor)))
    (catch Exception e
      (println (format "[musn-chat-page] cursor write failed: %s" (.getMessage e))))))

(defn- addressed-to? [text]
  (when-let [m (re-find #"(?i)^\s*@([A-Za-z0-9_-]+)" (or text ""))]
    (str/lower-case (second m))))

(defn- post-json!
  "POST JSON with configurable socket timeout (default 5s for polling)."
  ([url payload] (post-json! url payload 5000))
  ([url payload socket-timeout-ms]
   (let [resp (http/post url
                         {:content-type :json
                          :accept :json
                          :conn-timeout 3000
                          :socket-timeout socket-timeout-ms
                          :throw-exceptions false
                          :body (json/generate-string payload)})]
     {:status (:status resp)
      :body (when-let [raw (:body resp)]
              (try
                (json/parse-string raw true)
                (catch Exception _ nil)))})))

(defn- musn-state! [musn-url room cursor]
  (let [payload (cond-> {:room room}
                  (pos? cursor) (assoc :since cursor))
        {:keys [status body]} (post-json! (str (str/replace musn-url #"/+$" "") "/musn/chat/state")
                                          payload)]
    {:status status :body body}))

(defn- page-agent! [agency-url agent-id prompt timeout-ms]
  ;; Socket timeout should exceed page timeout to allow Agency to complete
  (post-json! (str (str/replace agency-url #"/+$" "") "/agency/page")
              {:agent-id agent-id
               :prompt prompt
               :timeout-ms timeout-ms}
              (+ timeout-ms 5000)))

(defn- event->prompt [room event]
  (let [payload (:payload event)
        author (:author payload)
        name (or (:name author) (:id author) "anon")
        text (or (:text payload) "")
        ts (or (:at event) (str (java.time.Instant/now)))]
    {:nick name
     :text text
     :prompt (str "IRC #" room " <" name "> " text "\n" "(ts " ts ")")}))

(defn -main [& args]
  (let [{:keys [musn-url agency-url room agent-id poll-interval timeout-ms ignore-nicks cursor-file]} (parse-args args)
        cursor-file (if (seq cursor-file)
                      cursor-file
                      (default-cursor-file room agent-id))
        start-cursor (or (read-cursor cursor-file) 0)
        agent-id-lc (str/lower-case agent-id)]
    (when (str/blank? agent-id)
      (println "Error: --agent-id required (or set MUSN_PAGE_AGENT)")
      (System/exit 1))
    (println (format "[musn-chat-page] room=%s agent=%s musn=%s agency=%s cursor=%s"
                     room agent-id musn-url agency-url start-cursor))
    (loop [cursor start-cursor
           error-count 0]
      (let [result (try
                     {:ok true
                      :data (musn-state! musn-url room cursor)}
                     (catch Exception e
                       {:ok false
                        :error (.getMessage e)}))
            {:keys [status body]} (get-in result [:data])
            poll-ok? (and (:ok result) (= 200 status) (:ok body))
            next-cursor (if poll-ok? (or (:cursor body) cursor) cursor)
            events (if poll-ok? (:events body) [])
            new-error-count (if (:ok result) 0 (inc error-count))]
        ;; Log errors but don't crash
        (when-not (:ok result)
          (println (format "[musn-chat-page] poll error (count=%d): %s" new-error-count (:error result))))
        (when (and poll-ok? (vector? events))
          (doseq [event events]
            (let [etype (:event/type event)
                  etype (cond
                          (keyword? etype) (name etype)
                          (string? etype) etype
                          :else "")]
              (when (= "chat/message" etype)
                (let [{:keys [nick text prompt]} (event->prompt room event)
                      mention (addressed-to? text)]
                  (when (and (seq text)
                             (not (contains? ignore-nicks nick))
                             (or (nil? mention) (= mention agent-id-lc)))
                    (try
                      (let [{:keys [status body]} (page-agent! agency-url agent-id prompt timeout-ms)]
                        (when (or (not= 200 status) (not (:ok body)))
                          (println (format "[musn-chat-page] page failed status=%s body=%s" status body))))
                      (catch Exception e
                        (println (format "[musn-chat-page] page error: %s" (.getMessage e)))))))))))
        (when poll-ok?
          (write-cursor! cursor-file next-cursor))
        (Thread/sleep (long (* 1000 poll-interval)))
        (recur next-cursor new-error-count)))))

(apply -main *command-line-args*)
