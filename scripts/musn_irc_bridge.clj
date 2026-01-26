#!/usr/bin/env clojure
;; MUSN IRC bridge.
;;
;; Options:
;;   --host HOST
;;   --port PORT
;;   --musn-url URL
;;   --poll-interval SECONDS
;;   --room NAME
;;   --password SECRET (or MUSN_IRC_PASSWORD)
;;   --allowlist CSV (or MUSN_IRC_ALLOWLIST, comma-separated IPs/CIDRs)
(ns scripts.musn-irc-bridge
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter BufferedWriter]
           [java.net ServerSocket Socket]
           [java.util UUID]))

(defn- parse-args [args]
  (loop [args args
         opts {:host "127.0.0.1"
               :port 6667
               :musn-url "http://localhost:6065"
               :poll-interval 2.0
               :room nil
               :password (System/getenv "MUSN_IRC_PASSWORD")
               :allowlist (some-> (System/getenv "MUSN_IRC_ALLOWLIST")
                                  (str/split #","))}]
    (if (empty? args)
      (update opts :allowlist (fn [entries]
                                (->> entries
                                     (map str/trim)
                                     (remove str/blank?)
                                     vec)))
      (let [[flag val & rest] args]
        (case flag
          "--host" (recur rest (assoc opts :host val))
          "--port" (recur rest (assoc opts :port (Integer/parseInt val)))
          "--musn-url" (recur rest (assoc opts :musn-url val))
          "--poll-interval" (recur rest (assoc opts :poll-interval (Double/parseDouble val)))
          "--room" (recur rest (assoc opts :room val))
          "--password" (recur rest (assoc opts :password val))
          "--allowlist" (recur rest (update opts :allowlist into (str/split (or val "") #",")))
          (recur rest opts))))))

(defonce server-state (atom {:clients {}
                             :rooms {}}))

(def ^:private log-path
  (or (System/getenv "MUSN_IRC_LOG") "/tmp/musn_irc_bridge.log"))

(defn- log! [msg]
  (let [line (str "[musn-irc] " msg)]
    (println line)
    (try
      (spit log-path (str (java.time.Instant/now) " " line "\n") :append true)
      (catch Throwable _))))

(defn- ipv4->int [ip]
  (when (string? ip)
    (let [parts (str/split ip #"\.")]
      (when (= 4 (count parts))
        (try
          (let [[a b c d] (map #(Integer/parseInt %) parts)]
            (when (every? #(<= 0 % 255) [a b c d])
              (-> (bit-shift-left a 24)
                  (bit-or (bit-shift-left b 16))
                  (bit-or (bit-shift-left c 8))
                  (bit-or d))))
          (catch Exception _ nil))))))

(defn- cidr-entry [token]
  (when (string? token)
    (if (str/includes? token "/")
      (let [[ip bits] (str/split token #"/" 2)
            ip-int (ipv4->int (str/trim ip))
            bits (try (Integer/parseInt (str/trim bits))
                      (catch Exception _ nil))]
        (when (and ip-int bits (<= 0 bits 32))
          {:kind :cidr
           :ip ip-int
           :bits bits
           :raw token}))
      {:kind :ip
       :ip (str/trim token)
       :raw token})))

(defn- allowlist-entries [entries]
  (->> entries
       (map cidr-entry)
       (remove nil?)
       vec))

(defn- ipv4-in-cidr? [ip-int {:keys [ip bits]}]
  (let [mask (if (zero? bits) 0 (bit-shift-left -1 (- 32 bits)))]
    (= (bit-and ip-int mask) (bit-and ip mask))))

(defn- allowed-remote? [allowlist remote-ip]
  (let [entries (allowlist-entries allowlist)]
    (if (empty? entries)
      true
      (let [ipv4 (ipv4->int remote-ip)]
        (boolean
         (some (fn [{:keys [kind ip] :as entry}]
                 (case kind
                   :ip (= ip remote-ip)
                   :cidr (and ipv4 (ipv4-in-cidr? ipv4 entry))
                   false))
               entries))))))

(defn- auth-required? [password]
  (and (string? password) (not (str/blank? password))))

(defn- strip-fulab-report [text]
  (let [text (or text "")]
    (-> text
        (str/replace #"\[FULAB-REPORT\][\s\S]*?\[/FULAB-REPORT\]" "")
        (str/replace #"[ \t]+\n" "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- send-line! [client line]
  (let [writer (:out client)]
    (locking writer
      (.write writer (str line "\r\n"))
      (.flush writer))))

(defn- broadcast! [clients line]
  (doseq [client clients]
    (send-line! client line)))

(defn- broadcast-except! [clients exclude-nick line]
  (let [exclude-nick (some-> exclude-nick str/trim not-empty)]
    (doseq [client clients]
      (when-not (and exclude-nick
                     (= exclude-nick (:nick client)))
        (send-line! client line)))))

(defn- room-clients [room-id]
  (let [clients (vals (:clients @server-state))]
    (filter #(= room-id (:room %)) clients)))

(defn- room-nicks [room-id]
  (let [clients (room-clients room-id)
        client-nicks (->> clients (map :nick) (filter seq) set)
        seen-nicks (get-in @server-state [:rooms room-id :seen-nicks] #{})]
    (-> (into client-nicks seen-nicks)
        (disj nil)
        (disj ""))))

(defn- note-room-nick! [room-id nick]
  (when (seq nick)
    (swap! server-state update-in [:rooms room-id :seen-nicks] (fnil conj #{}) nick)))

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

(defn- poll-room! [musn-url room-id stop-flag poll-interval]
  (log! (format "poller started for #%s" room-id))
  (loop [cursor 0]
    (if @stop-flag
      (log! (format "poller stopped for #%s" room-id))
      (let [payload (cond-> {:room room-id}
                      (pos? cursor) (assoc :since cursor))
            {:keys [status body]} (musn-post! musn-url "/musn/chat/state" payload)
            ok? (and (= 200 status) (:ok body))
            next-cursor (if ok?
                          (or (:cursor body) cursor)
                          cursor)]
            (when (and ok? (vector? (:events body)))
          (doseq [event (:events body)]
            (let [etype (:event/type event)
                  etype (cond
                         (keyword? etype) (name etype)
                         (string? etype) etype
                         :else "")]
              (when (= "chat/message" etype)
                (let [payload (:payload event)
                      author (:author payload)
                      name (or (:name author) (:id author) "anon")
                      text (strip-fulab-report (:text payload))]
                  (when (seq text)
                    (note-room-nick! room-id name)
                    (doseq [line (str/split-lines text)]
                      (broadcast-except! (room-clients room-id) name
                                         (format ":%s!%s@musn PRIVMSG #%s :%s"
                                                 name name room-id line)))
                    (log! (format "relay #%s <%s> %s"
                                  room-id
                                  name
                                  (if (> (count text) 120)
                                    (str (subs text 0 117) "...")
                                    text)))))))))
        (Thread/sleep (long (* 1000 poll-interval)))
        (recur next-cursor)))))

(defn- ensure-room-poller! [musn-url room-id poll-interval]
  (swap! server-state
         (fn [state]
           (if (get-in state [:rooms room-id :poller])
             state
             (let [stop-flag (atom false)
                   poller (future (poll-room! musn-url room-id stop-flag poll-interval))]
               (assoc-in state [:rooms room-id] {:stop? stop-flag :poller poller}))))))

(defn- stop-room-poller! [room-id]
  (when-let [stop-flag (get-in @server-state [:rooms room-id :stop?])]
    (reset! stop-flag true)))

(defn- maybe-stop-room! [room-id]
  (when (empty? (room-clients room-id))
    (stop-room-poller! room-id)
    (swap! server-state update :rooms dissoc room-id)))

(defn- welcome! [client]
  (let [nick (:nick client)]
    (when (and nick (:user client) (not (:registered? client)))
      (send-line! client (format ":musn 001 %s :Welcome to MUSN IRC bridge" nick))
      (send-line! client (format ":musn 002 %s :Your host is musn, running version 0.1" nick))
      (send-line! client (format ":musn 003 %s :This server was created just now" nick))
      (send-line! client (format ":musn 004 %s musn 0.1 iowghraAsORTVSxNCWqBzvdHtGp lvhopsmntikrRaqbeI" nick))
      (send-line! client (format ":musn 005 %s NETWORK=MUSNIRC CHANTYPES=# PREFIX=(ohv)@%%+ :are supported by this server" nick))
      (send-line! client (format ":musn 376 %s :End of /MOTD command." nick))
      true)))

(defn- join-room! [client room-id musn-url poll-interval]
  (let [room-id (str/replace room-id #"^#" "")]
    (swap! server-state assoc-in [:clients (:id client) :room] room-id)
    (ensure-room-poller! musn-url room-id poll-interval)
    (note-room-nick! room-id (:nick client))
    (send-line! client (format ":%s!%s@musn JOIN #%s" (:nick client) (:user client) room-id))
    (send-line! client (format ":musn 332 %s #%s :MUSN room %s" (:nick client) room-id room-id))
    (send-line! client (format ":musn 353 %s = #%s :%s"
                               (:nick client)
                               room-id
                               (str/join " " (sort (room-nicks room-id)))))
    (send-line! client (format ":musn 366 %s #%s :End of /NAMES list." (:nick client) room-id))))

(defn- handle-privmsg! [client target text musn-url]
  (let [room-id (str/replace target #"^#" "")]
    (if (and (:room client) (not= room-id (:room client)))
      (send-line! client (format ":musn NOTICE %s :Unknown room #%s" (:nick client) room-id))
      (let [room-id (or (:room client) room-id)
            payload {:room room-id
                     :msg-id (str (UUID/randomUUID))
                     :author {:id (:nick client) :name (:nick client)}
                     :text text}
            {:keys [status]} (musn-post! musn-url "/musn/chat/message" payload)]
        (when (not= 200 status)
          (send-line! client (format ":musn NOTICE %s :MUSN chat post failed" (:nick client))))))))

(defn- handle-line! [client line musn-url poll-interval default-room password]
  (let [line (str/trimr line)]
    (cond
      (and (auth-required? password)
           (not (:authed? client))
           (not (str/starts-with? line "PASS "))
           (not (str/starts-with? line "PING"))
           (not (str/starts-with? line "CAP "))
           (not (str/starts-with? line "QUIT")))
      (send-line! client ":musn NOTICE * :PASS required")

      (str/starts-with? line "PING")
      (let [token (second (str/split line #"\s+" 2))]
        (send-line! client (format "PONG %s" (or token ""))))

      (str/starts-with? line "CAP ")
      (do
        (when (str/includes? line "LS")
          (send-line! client "CAP * LS :"))
        (when (str/includes? line "REQ")
          (send-line! client "CAP * NAK :")))

      (str/starts-with? line "PASS ")
      (let [pass (second (str/split line #"\s+" 2))]
        (if (= pass password)
          (do
            (swap! server-state assoc-in [:clients (:id client) :authed?] true)
            (send-line! client ":musn NOTICE * :PASS accepted"))
          (do
            (send-line! client ":musn NOTICE * :PASS rejected")
            (throw (ex-info "auth-failed" {})))))

      (str/starts-with? line "NICK ")
      (let [nick (second (str/split line #"\s+" 2))]
        (swap! server-state assoc-in [:clients (:id client) :nick] (str/trim nick))
        (when (welcome! (get-in @server-state [:clients (:id client)]))
          (swap! server-state assoc-in [:clients (:id client) :registered?] true))
        (when (and default-room (:user client) (nil? (:room client)) (:authed? client))
          (join-room! (get-in @server-state [:clients (:id client)]) default-room musn-url poll-interval)))

      (str/starts-with? line "USER ")
      (let [[_ user] (str/split line #"\s+" 3)]
        (swap! server-state assoc-in [:clients (:id client) :user] (str/trim user))
        (when (welcome! (get-in @server-state [:clients (:id client)]))
          (swap! server-state assoc-in [:clients (:id client) :registered?] true))
        (when (and default-room (:nick client) (nil? (:room client)) (:authed? client))
          (join-room! (get-in @server-state [:clients (:id client)]) default-room musn-url poll-interval)))

      (str/starts-with? line "JOIN ")
      (let [room (second (str/split line #"\s+" 2))]
        (when (and room (:nick client))
          (join-room! (get-in @server-state [:clients (:id client)]) room musn-url poll-interval)))

      (str/starts-with? line "PRIVMSG ")
      (let [[_ target rest] (str/split line #"\s+" 3)
            rest (or rest "")
            text (cond
                   (str/starts-with? rest ":") (subs rest 1)
                   (str/includes? rest " :") (or (second (str/split rest #" :" 2)) "")
                   :else rest)]
        (when (and target (:nick client))
          (handle-privmsg! (get-in @server-state [:clients (:id client)]) target text musn-url)))

      (str/starts-with? line "MODE ")
      (let [[_ target] (str/split line #"\s+" 3)]
        (when (and target (:nick client))
          (if (str/starts-with? target "#")
            (send-line! client (format ":musn 324 %s %s +" (:nick client) target))
            (send-line! client (format ":musn 221 %s +" (:nick client))))))

      (str/starts-with? line "NAMES")
      (let [parts (str/split line #"\s+")
            room (some-> (second parts) (str/replace #"^#" ""))
            room (or room (:room client))]
        (when (and room (:nick client))
          (send-line! client (format ":musn 353 %s = #%s :%s"
                                     (:nick client)
                                     room
                                     (str/join " " (sort (room-nicks room)))))
          (send-line! client (format ":musn 366 %s #%s :End of /NAMES list." (:nick client) room))))

      (str/starts-with? line "WHO ")
      (let [room (second (str/split line #"\s+" 2))
            room (str/replace room #"^#" "")
            room (or room (:room client))]
        (when (and room (:nick client))
          (send-line! client (format ":musn 315 %s #%s :End of /WHO list." (:nick client) room))))

      (str/starts-with? line "QUIT")
      (throw (ex-info "quit" {}))

      :else nil)))

(defn- handle-client! [^Socket socket musn-url poll-interval default-room password allowlist]
  (let [id (str (UUID/randomUUID))
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket)))
        remote-ip (try (.getHostAddress (.getInetAddress socket)) (catch Throwable _ "unknown"))
        client {:id id
                :socket socket
                :in reader
                :out writer
                :nick nil
                :user nil
                :registered? false
                :room nil
                :authed? (not (auth-required? password))
                :remote-ip remote-ip}]
    (try
      (.setKeepAlive socket true)
      (catch Throwable _))
    (log! (format "client %s connected from %s"
                  id
                  (try (.getRemoteSocketAddress socket) (catch Throwable _ "unknown"))))
    (swap! server-state assoc-in [:clients id] client)
    (try
      (when-not (allowed-remote? allowlist remote-ip)
        (send-line! client ":musn NOTICE * :Connection rejected (allowlist)")
        (throw (ex-info "allowlist-reject" {:remote remote-ip})))
      (loop []
        (when-let [line (.readLine reader)]
          (when-not (str/starts-with? line "PING")
            (log! (format "client %s line: %s" id line)))
          (try
            (handle-line! (get-in @server-state [:clients id]) line musn-url poll-interval default-room password)
            (catch Throwable t
              (log! (format "handler error for %s: %s" id (.getMessage t)))))
          (recur)))
      (catch Exception e
        (log! (format "client %s read error: %s" id (.getMessage e))))
      (finally
        (let [room (:room (get-in @server-state [:clients id]))]
          (swap! server-state update :clients dissoc id)
          (when room
            (maybe-stop-room! room)))
        (log! (format "client %s disconnected" id))
        (try (.close socket) (catch Exception _ nil))))))

(defn -main [& args]
  (let [{:keys [host port musn-url poll-interval room password allowlist]} (parse-args args)]
    (log! (format "listening on %s:%d (musn=%s)" host port musn-url))
    (with-open [server (ServerSocket. port 50 (java.net.InetAddress/getByName host))]
      (while true
        (let [socket (.accept server)]
          (future (handle-client! socket musn-url poll-interval room password allowlist)))))))

(when (= *file* (or (System/getProperty "babashka.file") *file*))
  (apply -main *command-line-args*))
