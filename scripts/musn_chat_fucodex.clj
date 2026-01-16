#!/usr/bin/env clojure
(ns scripts.musn-chat-fucodex
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.lang ProcessBuilder]))

(defn- parse-args [args]
  (loop [args args
         opts {:musn-url "http://localhost:6065"
               :room "lab"
               :poll-interval 2.0
               :bot-name "fucodex"
               :intent nil
               :context-limit 12
               :fucodex "fucodex"}]
    (if (empty? args)
      opts
      (let [[flag val & rest] args]
        (case flag
          "--musn-url" (recur rest (assoc opts :musn-url val))
          "--room" (recur rest (assoc opts :room val))
          "--poll-interval" (recur rest (assoc opts :poll-interval (Double/parseDouble val)))
          "--bot-name" (recur rest (assoc opts :bot-name val))
          "--intent" (recur rest (assoc opts :intent val))
          "--context-limit" (recur rest (assoc opts :context-limit (Integer/parseInt val)))
          "--fucodex" (recur rest (assoc opts :fucodex val))
          (recur rest opts))))))

(defn- log! [msg & [ctx]]
  (if ctx
    (println (str "[musn-chat] " msg " " (pr-str ctx)))
    (println (str "[musn-chat] " msg))))

(defn- strip-fulab-report [text]
  (let [text (or text "")]
    (-> text
        (str/replace #"\[FULAB-REPORT\][\s\S]*?\[/FULAB-REPORT\]" "")
        (str/replace #"[ \t]+\n" "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- musn-post! [musn-url path payload]
  (let [resp (http/post (str (str/replace musn-url #"/+$" "") path)
                        {:content-type :json
                         :accept :json
                         :throw-exceptions false
                         :body (json/generate-string payload)})
        body (when-let [raw (:body resp)]
               (try
                 (json/parse-string raw true)
                 (catch Exception _ nil)))]
    {:status (:status resp) :body body}))

(defn- normalize-event-type [etype]
  (cond
    (keyword? etype) (name etype)
    (string? etype) etype
    :else ""))

(defn- parse-chat-message [event]
  (let [etype (normalize-event-type (:event/type event))]
    (when (= "chat/message" etype)
      (let [payload (:payload event)
            author (:author payload)
            name (or (:name author) (:id author) "anon")
            text (strip-fulab-report (:text payload))]
        (when (seq text)
          {:msg-id (:msg-id payload)
           :chat-id (:chat/id payload)
            :name name
            :author-id (:id author)
            :text text
            :at (:at payload)})))))

(defn- parse-instant [value]
  (try
    (when (string? value)
      (Instant/parse value))
    (catch Exception _ nil)))

(defn- build-transcript [history limit]
  (->> history
       (take-last limit)
       (map (fn [{:keys [name text]}]
              (format "%s: %s" name text)))
       (str/join "\n")))

(defn- build-prompt [history limit intent message]
  (let [transcript (build-transcript history limit)
        intent-line (when (and intent (not (str/blank? intent)))
                      (format "Intent: %s\n" intent))]
    (str "You are Fucodex in a multi-user chat.\n"
         (or intent-line "")
         "Conversation so far:\n"
         (if (seq transcript) transcript "[no prior messages]")
         "\n\nNew message from " (:name message) ":\n"
         (:text message)
         "\n\nRespond in the chat, keep it concise and helpful.")))

(defn- run-fucodex! [fucodex musn-url room bot-name intent prompt]
  (let [args (cond-> [fucodex
                      "--live"
                      "--musn"
                      "--musn-url" musn-url
                      "--chat-room" room
                      "--chat-author" bot-name]
               (and intent (not (str/blank? intent))) (conj "--intent" intent)
               true (conj "exec" "--prompt" prompt))
        pb (doto (ProcessBuilder. ^java.util.List args)
             (.inheritIO))
        env (.environment pb)
        started-at (Instant/now)]
    (.put env "FUTON3_MUSN_REQUIRE_APPROVAL" "0")
    (.put env "FUTON3_MUSN_CHAT_TRIM" "1")
    (log! "starting fucodex run" {:room room :bot bot-name})
    (let [proc (.start pb)
          exit-code (.waitFor proc)
          duration-ms (.toMillis (java.time.Duration/between started-at (Instant/now)))]
      (log! "fucodex run complete" {:exit exit-code :ms duration-ms})
      (when (not= 0 exit-code)
        (log! "fucodex exited non-zero" {:exit exit-code})))
    true))

(defn- same-bot? [bot-name message]
  (let [bot-name (str/lower-case (or bot-name ""))
        author (str/lower-case (or (:name message) ""))]
    (= bot-name author)))

(defn- poll-loop! [{:keys [musn-url room poll-interval bot-name intent context-limit fucodex]}]
  (log! "starting chat relay" {:room room :bot bot-name :musn musn-url})
  (let [started-at (Instant/now)
        init-resp (musn-post! musn-url "/musn/chat/state" {:room room})
        init-cursor (if (and (= 200 (:status init-resp)) (get-in init-resp [:body :ok]))
                      (or (get-in init-resp [:body :cursor]) 0)
                      0)
        init-events (get-in init-resp [:body :events])
        init-messages (->> init-events (map parse-chat-message) (remove nil?) vec)
        seen-init (into #{} (map :msg-id init-messages))]
    (loop [cursor init-cursor
           history []
           seen seen-init]
      (let [payload (cond-> {:room room}
                      (pos? cursor) (assoc :since cursor))
            {:keys [status body]} (musn-post! musn-url "/musn/chat/state" payload)
            ok? (and (= 200 status) (:ok body))
            next-cursor (if ok? (or (:cursor body) cursor) cursor)
            events (when ok? (:events body))
            messages (->> events (map parse-chat-message) (remove nil?) vec)
            recent-messages (->> messages
                                 (filter (fn [msg]
                                           (when-let [at (parse-instant (:at msg))]
                                             (not (.isBefore at started-at)))))
                                 (remove #(same-bot? bot-name %))
                                 vec)
            history (into history recent-messages)
            history (if (> (count history) (* 3 context-limit))
                      (vec (take-last (* 3 context-limit) history))
                      history)
            new-messages (->> recent-messages
                              (remove #(contains? seen (:msg-id %)))
                              vec)
            seen (into seen (map :msg-id messages))]
        (when-let [message (last new-messages)]
          (let [prompt (build-prompt history context-limit intent message)]
            (run-fucodex! fucodex musn-url room bot-name intent prompt)))
        (Thread/sleep (long (* 1000 poll-interval)))
        (recur next-cursor history seen)))))

(defn -main [& args]
  (poll-loop! (parse-args args)))

(apply -main *command-line-args*)
