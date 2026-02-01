(ns futon3.dev.codex-upload-ws
  "Stream a local Codex JSONL session to a remote Futon3 lab upload WebSocket."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [f0.clock :as clock])
  (:import (java.net URI)
           (java.net.http HttpClient WebSocket WebSocket$Listener)
           (java.util.concurrent CompletableFuture TimeUnit)))

(defn- usage []
  (str "Usage: clojure -M -m futon3.dev.codex-upload-ws \\n  --url wss://host:5050/fulab/lab/upload/ws \\n  --path /home/you/.codex/sessions/<session>.jsonl \\n  [--project /path/to/project] [--originator laptop] [--cwd /path]\n"))

(defn- parse-args [args]
  (loop [remaining args
         opts {}]
    (if (empty? remaining)
      opts
      (let [[k v & more] remaining]
        (case k
          "--url" (recur more (assoc opts :url v))
          "--path" (recur more (assoc opts :path v))
          "--project" (recur more (assoc opts :project v))
          "--originator" (recur more (assoc opts :originator v))
          "--cwd" (recur more (assoc opts :cwd v))
          (recur more opts))))))

(defn- read-jsonl-lines [path]
  (with-open [rdr (io/reader path)]
    (vec (line-seq rdr))))

(defn- parse-json-line [line]
  (try
    (json/parse-string line true)
    (catch Exception _
      line)))

(defn- session-id-from [path first-entry]
  (or (get-in first-entry [:payload :id])
      (get-in first-entry [:payload :session-id])
      (:id first-entry)
      (str/replace (.getName (io/file path)) #"\.jsonl$" "")))

(defn- make-listener []
  (reify WebSocket$Listener
    (onOpen [_ ws]
      (.request ws 1))
    (onText [_ ws _data _last]
      (.request ws 1)
      nil)
    (onBinary [_ ws _data _last]
      (.request ws 1)
      nil)
    (onPing [_ ws _data]
      (.request ws 1)
      nil)
    (onPong [_ ws _data]
      (.request ws 1)
      nil)
    (onClose [_ _ws _status _reason]
      nil)
    (onError [_ _ws err]
      (binding [*out* *err*]
        (println "[codex-upload] WebSocket error:" (.getMessage err))))))

(defn- ws-send! [^WebSocket ws payload]
  (let [text (json/encode payload)
        fut (.sendText ws text true)]
    (.get fut 5 TimeUnit/SECONDS)))

(defn- stream-file! [^WebSocket ws path opts]
  (let [lines (read-jsonl-lines path)
        first-entry (parse-json-line (first lines))
        session-id (session-id-from path first-entry)
        init-events (mapv parse-json-line lines)
        meta (merge {:type "init"
                     :session-id session-id
                     :project (or (:project opts) (:cwd opts))
                     :source "codex"
                     :originator (:originator opts)
                     :cwd (:cwd opts)
                     :events init-events}
                    (select-keys opts [:remote-source]))]
    (ws-send! ws meta)
    (println "[codex-upload] init sent" session-id "events" (count init-events))
    (let [last-line-count (atom (count lines))
          file (io/file path)]
      (loop []
        (Thread/sleep 500)
        (when (.exists file)
          (let [all-lines (read-jsonl-lines path)
                current-count (count all-lines)]
            (when (> current-count @last-line-count)
              (doseq [line (subvec (vec all-lines) @last-line-count)]
                (ws-send! ws {:type "event"
                              :session-id session-id
                              :event (parse-json-line line)}))
              (reset! last-line-count current-count))))
        (recur)))))

(defn -main [& args]
  (let [{:keys [url path] :as opts} (parse-args args)]
    (when (or (nil? url) (nil? path))
      (println (usage))
      (System/exit 1))
    (when-not (.exists (io/file path))
      (println "Missing JSONL path:" path)
      (System/exit 1))
    (let [client (HttpClient/newHttpClient)
          listener (make-listener)
          ws-fut (-> client
                     (.newWebSocketBuilder)
                     (.buildAsync (URI/create url) listener))
          ws (.get ^CompletableFuture ws-fut 10 TimeUnit/SECONDS)]
      (println "[codex-upload] connected" url "at" (clock/->iso-string))
      (stream-file! ws path opts))))
