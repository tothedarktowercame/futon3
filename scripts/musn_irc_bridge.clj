#!/usr/bin/env clojure
;; MUSN IRC bridge with WebSocket support.
;;
;; Options:
;;   --host HOST           Bind address (default 127.0.0.1)
;;   --port PORT           IRC port (default 6667)
;;   --ws-port PORT        WebSocket port (default: none, disabled)
;;   --musn-url URL        MUSN service URL (default http://localhost:6065)
;;   --poll-interval SECS  Room poll interval (default 2.0)
;;   --room NAME           Default room to auto-join
;;   --password SECRET     Auth password (or MUSN_IRC_PASSWORD env)
;;   --allowlist CSV       Allowed IPs/CIDRs (or MUSN_IRC_ALLOWLIST env)
;;
;; WebSocket clients connect to ws://host:ws-port/?nick=NAME&room=ROOM&pass=SECRET
;; and receive JSON messages: {type: "message", room, nick, text, timestamp}
(ns scripts.musn-irc-bridge
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as str]
            [org.httpkit.server :as httpkit])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter BufferedWriter]
           [java.net ServerSocket Socket]
           [java.util UUID]))

(defn- parse-args [args]
  (loop [args args
         opts {:host "127.0.0.1"
               :port 6667
               :ws-port nil
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
          "--ws-port" (recur rest (assoc opts :ws-port (Integer/parseInt val)))
          "--musn-url" (recur rest (assoc opts :musn-url val))
          "--poll-interval" (recur rest (assoc opts :poll-interval (Double/parseDouble val)))
          "--room" (recur rest (assoc opts :room val))
          "--password" (recur rest (assoc opts :password val))
          "--allowlist" (recur rest (update opts :allowlist into (str/split (or val "") #",")))
          (recur rest opts))))))

(defonce server-state (atom {:clients {}      ;; IRC clients
                             :ws-clients {}   ;; WebSocket clients
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
  "Get IRC clients in a room."
  (let [clients (vals (:clients @server-state))]
    (filter #(= room-id (:room %)) clients)))

(defn- ws-room-clients [room-id]
  "Get WebSocket clients in a room."
  (let [clients (vals (:ws-clients @server-state))]
    (filter #(= room-id (:room %)) clients)))

(defn- send-ws! [client data]
  "Send JSON data to a WebSocket client."
  (when-let [ch (:channel client)]
    (try
      (httpkit/send! ch (json/generate-string data))
      (catch Throwable _))))

(defn- broadcast-ws! [room-id exclude-nick msg-data]
  "Broadcast a message to all WebSocket clients in a room."
  (doseq [client (ws-room-clients room-id)]
    (when-not (and exclude-nick (= exclude-nick (:nick client)))
      (send-ws! client msg-data))))

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
                         :conn-timeout 3000
                         :socket-timeout 5000
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
                      text (strip-fulab-report (:text payload))
                      timestamp (or (:at event) (str (java.time.Instant/now)))]
                  (when (seq text)
                    (note-room-nick! room-id name)
                    ;; Broadcast to IRC clients
                    (doseq [line (str/split-lines text)]
                      (broadcast-except! (room-clients room-id) name
                                         (format ":%s!%s@musn PRIVMSG #%s :%s"
                                                 name name room-id line)))
                    ;; Broadcast to WebSocket clients
                    (broadcast-ws! room-id name
                                   {:type "message"
                                    :room room-id
                                    :nick name
                                    :text text
                                    :timestamp timestamp})
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
               (update state :rooms assoc room-id
                       (merge {:stop? stop-flag :poller poller}
                              (get-in state [:rooms room-id] {}))))))))

(defn- stop-room-poller! [room-id]
  (when-let [stop-flag (get-in @server-state [:rooms room-id :stop?])]
    (reset! stop-flag true)))

(defn- maybe-stop-room! [room-id]
  (when (and (empty? (room-clients room-id))
             (empty? (ws-room-clients room-id)))
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
      (let [pass (-> (second (str/split line #"\s+" 2))
                     (str/replace #"^:" ""))]  ;; Strip IRC trailing-param colon
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
              (log! (format "handler error for %s: %s" id (.getMessage t)))
              (when (= "auth-failed" (.getMessage t))
                (throw t))))
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

(defonce ^:private bridge-state (atom nil))

;; ---------------------------------------------------------------------------
;; WebSocket Handler
;; ---------------------------------------------------------------------------

(defn- ws-handle-message! [client-id text musn-url poll-interval]
  "Handle incoming WebSocket message (JSON)."
  (try
    (let [data (json/parse-string text true)
          msg-type (:type data)]
      (case msg-type
        "message"
        (let [client (get-in @server-state [:ws-clients client-id])
              room-id (:room client)
              nick (:nick client)
              msg-text (:text data)]
          (when (and room-id nick (seq msg-text))
            (let [payload {:room room-id
                           :msg-id (str (UUID/randomUUID))
                           :author {:id nick :name nick}
                           :text msg-text}]
              (musn-post! musn-url "/musn/chat/message" payload))))

        "join"
        (let [room-id (str/replace (or (:room data) "") #"^#" "")]
          (when (seq room-id)
            (swap! server-state assoc-in [:ws-clients client-id :room] room-id)
            (let [client (get-in @server-state [:ws-clients client-id])]
              (ensure-room-poller! musn-url room-id poll-interval)
              (note-room-nick! room-id (:nick client))
              (send-ws! client {:type "joined" :room room-id :nicks (vec (room-nicks room-id))}))))

        ;; Unknown message type - ignore
        nil))
    (catch Throwable t
      (log! (format "ws message parse error: %s" (.getMessage t))))))

(defn- first-forwarded-for [value]
  (some-> value
          (str/split #",")
          first
          str/trim
          not-empty))

(defn- parse-query [query]
  (if (str/blank? query)
    {}
    (->> (str/split query #"&")
         (keep (fn [pair]
                 (let [[k v] (str/split pair #"=" 2)]
                   (when (and k v)
                     [(java.net.URLDecoder/decode k "UTF-8")
                      (java.net.URLDecoder/decode v "UTF-8")]))))
         (into {}))))

(defn- ws-handler [musn-url poll-interval default-room password allowlist]
  "Create WebSocket handler for http-kit."
  (fn [req]
    (let [params (parse-query (:query-string req))
          nick (get params "nick")
          room (or (get params "room") default-room)
          pass (get params "pass")
          remote-ip (or (:remote-addr req)
                        (first-forwarded-for (get-in req [:headers "x-forwarded-for"]))
                        "unknown")]
      ;; Check auth
      (cond
        (and (auth-required? password) (not= pass password))
        {:status 403 :body "Forbidden: invalid password"}

        (not (allowed-remote? allowlist remote-ip))
        {:status 403 :body "Forbidden: IP not allowed"}

        (str/blank? nick)
        {:status 400 :body "Bad Request: nick parameter required"}

        :else
        #_{:clj-kondo/ignore [:unresolved-symbol]}
        (httpkit/with-channel req channel
          (let [client-id (str (UUID/randomUUID))
                room-id (str/replace (or room "") #"^#" "")
                client {:id client-id
                        :channel channel
                        :nick nick
                        :room (when (seq room-id) room-id)
                        :remote-ip remote-ip}]
            (log! (format "ws client %s connected as %s from %s" client-id nick remote-ip))
            (swap! server-state assoc-in [:ws-clients client-id] client)

            ;; Ensure room poller is running if room specified
            (when (seq room-id)
              (ensure-room-poller! musn-url room-id poll-interval)
              (note-room-nick! room-id nick))

            ;; Send welcome message
            (send-ws! client {:type "welcome"
                              :nick nick
                              :room room-id
                              :nicks (when (seq room-id) (vec (room-nicks room-id)))})

            ;; Handle incoming messages
            (httpkit/on-receive channel
                                (fn [data]
                                  (ws-handle-message! client-id data musn-url poll-interval)))

            ;; Handle close
            (httpkit/on-close channel
                              (fn [_status]
                                (log! (format "ws client %s disconnected" client-id))
                                (let [room (:room (get-in @server-state [:ws-clients client-id]))]
                                  (swap! server-state update :ws-clients dissoc client-id)
                                  (when room
                                    (maybe-stop-room! room)))))))))))

(defn- close-all-ws-clients! []
  (doseq [[_ client] (:ws-clients @server-state)]
    (try
      (when-let [ch (:channel client)]
        (httpkit/close ch))
      (catch Throwable _)))
  (swap! server-state assoc :ws-clients {}))

(defn- accept-loop! [^ServerSocket server musn-url poll-interval default-room password allowlist stop-flag]
  (try
    (while (not @stop-flag)
      (try
        (let [socket (.accept server)]
          (future (handle-client! socket musn-url poll-interval default-room password allowlist)))
        (catch java.net.SocketException _
          ;; Server socket closed, exit loop
          nil)))
    (catch Throwable t
      (when-not @stop-flag
        (log! (format "accept loop error: %s" (.getMessage t)))))))

(defn- stop-all-rooms! []
  (doseq [[room-id _] (:rooms @server-state)]
    (stop-room-poller! room-id))
  (swap! server-state assoc :rooms {}))

(defn- close-all-clients! []
  (doseq [[_ client] (:clients @server-state)]
    (try
      (when-let [socket (:socket client)]
        (.close ^Socket socket))
      (catch Throwable _)))
  (swap! server-state assoc :clients {}))

(defn start!
  "Start the IRC bridge server with optional WebSocket support. Returns a stop function.
   Options:
     :host - bind address (default 127.0.0.1)
     :port - IRC port (default 6667)
     :ws-port - WebSocket port (default nil, disabled)
     :musn-url - MUSN service URL (default http://localhost:6065)
     :poll-interval - room poll interval in seconds (default 2.0)
     :room - default room to join
     :password - auth password
     :allowlist - allowed IPs/CIDRs"
  ([] (start! {}))
  ([opts]
   (let [host (or (:host opts) "127.0.0.1")
         port (or (:port opts) 6667)
         ws-port (:ws-port opts)
         musn-url (or (:musn-url opts) "http://localhost:6065")
         poll-interval (or (:poll-interval opts) 2.0)
         default-room (:room opts)
         password (:password opts)
         allowlist (:allowlist opts)
         stop-flag (atom false)
         server (ServerSocket. port 50 (java.net.InetAddress/getByName host))
         accept-thread (future (accept-loop! server musn-url poll-interval default-room password allowlist stop-flag))
         ;; Start WebSocket server if port specified
         ws-stop-fn (when ws-port
                      (let [handler (ws-handler musn-url poll-interval default-room password allowlist)]
                        (log! (format "ws listening on %s:%d" host ws-port))
                        (httpkit/run-server handler {:ip host :port ws-port})))]
     (log! (format "irc listening on %s:%d (musn=%s)" host port musn-url))
     (reset! bridge-state {:server server
                           :stop-flag stop-flag
                           :accept-thread accept-thread
                           :ws-stop-fn ws-stop-fn})
     (fn []
       (reset! stop-flag true)
       (try (.close server) (catch Throwable _))
       (when ws-stop-fn (ws-stop-fn))
       (stop-all-rooms!)
       (close-all-clients!)
       (close-all-ws-clients!)
       (reset! bridge-state nil)))))

(defn stop!
  "Stop the IRC bridge server."
  []
  (when-let [{:keys [server stop-flag ws-stop-fn]} @bridge-state]
    (reset! stop-flag true)
    (try (.close ^ServerSocket server) (catch Throwable _))
    (when ws-stop-fn (ws-stop-fn))
    (stop-all-rooms!)
    (close-all-clients!)
    (close-all-ws-clients!)
    (reset! bridge-state nil)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (start! opts)
    @(promise)))
