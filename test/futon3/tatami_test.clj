(ns futon3.tatami-test
  (:require [clojure.test :refer :all]
            [futon3.tatami :as tatami]
            [futon3.tatami-store :as store])
  (:import (java.io File)))

(deftest log-event-accepts-string-session-id
  (let [path-atom (deref #'futon3.tatami-store/tatami-log-path)
        json-atom (deref #'futon3.tatami-store/tatami-json-log-path)
        session-atom (deref #'futon3.tatami/!session)
        orig-path @path-atom
        orig-json @json-atom
        tmp (doto (File/createTempFile "tatami-test" ".edn")
              (.delete))
        json-tmp (doto (File/createTempFile "tatami-test" ".jsonl")
                   (.delete))
        temp-path (.getAbsolutePath tmp)
        json-path (.getAbsolutePath json-tmp)]
    (try
      (store/set-log-path! temp-path)
      (store/set-json-log-path! json-path)
      (store/reset-events!)
      (reset! session-atom nil)
      (let [session (tatami/start-session {:prototypes [:f3/p0]
                                           :intent "keyword intents"})
            sid (str (:session-id session))
            event (tatami/log-event {:session-id sid
                                     :activity :coding
                                     :felt-state :ok})]
        (is (= sid (str (:session-id event))))
        (is (= :coding (:activity event))))
      (finally
        (reset! session-atom nil)
        (store/set-log-path! orig-path)
        (store/set-json-log-path! orig-json)
        (store/reset-events!)
        (when (.exists tmp)
          (.delete tmp))
        (when (.exists json-tmp)
          (.delete json-tmp))))))
