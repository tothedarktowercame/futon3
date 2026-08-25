(ns futon3.inbox-zero.gates
  "Validation and execution of ordered inbox-zero gates."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def output-limit 4096)

(defn- bounded [value]
  (let [text (str value)]
    (subs text 0 (min output-limit (count text)))))

(defn validate-gates! [gate-specs]
  (when-not (vector? gate-specs)
    (throw (ex-info "Inbox-zero gates must be a vector"
                    {:error/type :inbox-zero/invalid-gates})))
  (doseq [gate gate-specs]
    (let [paths? (:gate/paths? gate)
          extensions (:gate/extensions gate)]
      (when-not (and (keyword? (:gate/name gate))
                     (vector? (:cmd gate))
                     (seq (:cmd gate))
                     (every? string? (:cmd gate))
                     (or (nil? paths?) (boolean? paths?))
                     (or (nil? extensions)
                         (and (set? extensions)
                              (every? string? extensions)))
                     (or (nil? extensions) (true? paths?)))
        (throw (ex-info "Inbox-zero gate is malformed"
                        {:error/type :inbox-zero/invalid-gate
                         :gate gate})))))
  gate-specs)

(defn- extension [path]
  (let [filename (last (str/split path #"/"))
        dot (.lastIndexOf ^String filename ".")]
    (when (and (pos? dot) (< dot (dec (count filename))))
      (str/lower-case (subs filename (inc dot))))))

(defn- selected-paths [{:gate/keys [paths? extensions]} paths]
  (when paths?
    (if extensions
      (let [wanted (set (map str/lower-case extensions))]
        (filterv #(contains? wanted (extension %)) paths))
      paths)))

(defn run-gates [repo-root gate-specs paths]
  (loop [remaining gate-specs results []]
    (if-let [{:gate/keys [name paths?] :keys [cmd] :as gate}
             (first remaining)]
      (let [selected (selected-paths gate paths)]
        (if (and paths? (empty? selected))
          (recur (next remaining)
                 (conj results {:gate/name name :exit 0 :skipped? true
                                :output "no matching paths"}))
          (let [{:keys [exit out err]}
                (try
                  (apply shell/sh
                         (concat cmd (or selected []) [:dir repo-root]))
                  (catch Exception error
                    {:exit -1 :out "" :err (.getMessage error)}))
                gate-result {:gate/name name :exit exit
                             :output (bounded (str out err))}
                results* (conj results gate-result)]
            (if (zero? exit)
              (recur (next remaining) results*)
              {:passed? false :results results*}))))
      {:passed? true :results results})))
