(ns scripts.extract-anchors
  "Extract anchors from Clojure files and upsert them into the hypertext store."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.hx.api :as hx]))

(def clj-dirs ["/home/joe/code/futon3/src/f2"
               "/home/joe/code/futon3/src/futon3"])

(def elisp-dirs ["/home/joe/code/futon3/contrib"
                 "/home/joe/code/futon3/scripts"
                 "/home/joe/code/futon3/holes"])

(defn- slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-|-$)" "")))

(defn- next-count! [seen key]
  (let [m (swap! seen update key (fnil inc 0))]
    (get m key)))

(defn- anchors-for-clj [file]
  (let [lines (line-seq (io/reader file))
        base (str file)
        seen (atom {})]
    (->> lines
         (map-indexed (fn [idx line]
                        {:line (inc idx) :text line}))
         (keep (fn [{:keys [line text]}]
                 (cond
                   (re-find #"^\(ns\s+([^\s\)]+)" text)
                   (let [name (second (re-find #"^\(ns\s+([^\s\)]+)" text))
                         slug (slugify (str "ns-" name))
                         n (next-count! seen slug)
                         anchor-id (if (> n 1)
                                     (str slug "-" n)
                                     slug)]
                     {:anchor/id anchor-id
                      :anchor/artifact base
                      :anchor/kind :ns
                      :anchor/selector {:kind :line :line line}
                      :anchor/line line
                      :anchor/text text
                      :anchor/title name})

                   (re-find #"^\(defn-?\s+([^\s\)]+)" text)
                   (let [name (second (re-find #"^\(defn-?\s+([^\s\)]+)" text))
                         slug (slugify name)
                         n (next-count! seen slug)
                         anchor-id (if (> n 1)
                                     (str slug "-" n)
                                     slug)]
                     {:anchor/id anchor-id
                      :anchor/artifact base
                      :anchor/kind :defn
                      :anchor/selector {:kind :line :line line}
                      :anchor/line line
                      :anchor/text text
                      :anchor/title name})

                   (re-find #"^\(defmulti\s+([^\s\)]+)" text)
                   (let [name (second (re-find #"^\(defmulti\s+([^\s\)]+)" text))
                         slug (slugify name)
                         n (next-count! seen slug)
                         anchor-id (if (> n 1)
                                     (str slug "-" n)
                                     slug)]
                     {:anchor/id anchor-id
                      :anchor/artifact base
                      :anchor/kind :defmulti
                      :anchor/selector {:kind :line :line line}
                      :anchor/line line
                      :anchor/text text
                      :anchor/title name})

                   :else
                   nil))))))

(defn- anchors-for-elisp [file]
  (let [lines (line-seq (io/reader file))
        base (str file)
        seen (atom {})]
    (->> lines
         (map-indexed (fn [idx line]
                        {:line (inc idx) :text line}))
         (keep (fn [{:keys [line text]}]
                 (when-let [m (re-find #"^\s*\(defun\s+([^\s\)]+)" text)]
                   (let [name (second m)
                         slug (slugify name)
                         n (next-count! seen slug)
                         anchor-id (if (> n 1)
                                     (str slug "-" n)
                                     slug)]
                     {:anchor/id anchor-id
                      :anchor/artifact base
                      :anchor/kind :defun
                      :anchor/selector {:kind :line :line line}
                      :anchor/line line
                      :anchor/text text
                      :anchor/title name})))))))

(defn- clj-files []
  (->> clj-dirs
       (mapcat #(file-seq (io/file %)))
       (filter #(.isFile ^java.io.File %))
       (filter (fn [f]
                 (let [path (str f)]
                   (or (str/ends-with? path ".clj")
                       (str/ends-with? path ".cljc")))))))

(defn- elisp-files []
  (->> elisp-dirs
       (mapcat #(file-seq (io/file %)))
       (filter #(.isFile ^java.io.File %))
       (filter (fn [f]
                 (str/ends-with? (str f) ".el")))))

(defn -main [& _]
  (let [files (concat (clj-files) (elisp-files))
        results (for [file files
                      :let [path (str file)
                            elisp? (str/ends-with? path ".el")
                            anchors (vec (if elisp?
                                           (anchors-for-elisp file)
                                           (anchors-for-clj file)))
                            artifact {:artifact/id path
                                      :artifact/type (if elisp? :elisp :clojure)
                                      :artifact/title (.getName ^java.io.File file)
                                      :artifact/path path}
                            _ (hx/register-artifact! artifact)
                            resp (hx/upsert-anchors! path anchors)]]
                  {:file path
                   :anchors (count anchors)
                   :ok (:ok resp)})]
    (println (pr-str {:files (count results)
                      :anchors (reduce + 0 (map :anchors results))
                      :results results}))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
