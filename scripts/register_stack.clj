(ns scripts.register-stack
  "Register core FUTON3 artifacts for the hypertext demo."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.hx.api :as hx]))

(defn- file-ext [^String path]
  (when-let [idx (str/last-index-of path ".")]
    (subs path (inc idx))))

(defn- artifact-type [path]
  (case (file-ext path)
    "clj" :clojure
    "cljc" :clojure
    "md" :markdown
    "org" :org
    "devmap" :devmap
    :file))

(defn- artifact-title [path]
  (-> path
      (str/replace-first #"^futon3/" "")
      (str/replace #"_" " ")))

(defn- register-file! [^java.io.File file]
  (let [path (str file)
        artifact {:artifact/id path
                  :artifact/type (artifact-type path)
                  :artifact/title (artifact-title path)
                  :artifact/path path}]
    (hx/register-artifact! artifact)))

(defn- files-under [dir pred]
  (->> (file-seq (io/file dir))
       (filter #(.isFile ^java.io.File %))
       (filter (fn [f] (pred (str f))))))

(defn- register-paths! [paths pred]
  (reduce
   (fn [acc file]
     (let [result (register-file! file)
           ok? (:ok result)]
       (cond-> acc
         ok? (update :ok inc)
         (not ok?) (update :errors conj result))))
   {:ok 0 :errors []}
   (mapcat #(files-under % pred) paths)))

(defn -main [& _]
  (let [targets ["/home/joe/code/futon3/src/f2"
                 "/home/joe/code/futon3/src/futon3"
                 "/home/joe/code/futon3/holes"
                 "/home/joe/code/futon3/docs"]
        pred (fn [path]
               (or (str/ends-with? path ".clj")
                   (str/ends-with? path ".cljc")
                   (str/ends-with? path ".devmap")
                   (str/ends-with? path ".md")))
        result (register-paths! targets pred)]
    (println (pr-str result))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
