(ns repl.http
  "Drawbridge (nREPL over HTTP) helper for remote admin access."
  (:require [cemerick.drawbridge :as db]
            [org.httpkit.server :as http]
            [ring.util.request :as req]))

(defn wrap-token [handler token allow]
  (fn [request]
    (let [remote (:remote-addr request)
          allowed? (if (seq allow)
                     (contains? (set allow) remote)
                     true)
          supplied (or (get-in request [:headers "x-admin-token"])
                       (some-> (req/get-query request) (get "token")))]
      (if (and allowed? (= supplied token))
        (handler request)
        {:status 403
         :headers {"content-type" "text/plain"}
         :body "forbidden"}))))

(defonce server (atom nil))

(defn start!
  "Start a Drawbridge endpoint. Options: {:port 7000 :bind "127.0.0.1" :token "secret" :allow ["127.0.0.1"]}."
  [{:keys [port token bind allow]
    :or {port 7000
         bind "127.0.0.1"
         token "change-me"
         allow ["127.0.0.1" "::1"]}}]
  (when-let [stop-fn @server]
    (stop-fn))
  (let [handler (-> (db/ring-handler)
                    (wrap-token token allow))
        stop (http/run-server handler {:ip bind :port port})]
    (reset! server stop)
    (println (format "Drawbridge on http://%s:%s/repl (allowed: %s)" bind port (str allow)))
    stop))

(defn stop! []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    (println "Drawbridge stopped")))
