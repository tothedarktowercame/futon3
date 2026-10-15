(ns scripts.refresh-f11-source-pins
  "Relocate already-reviewed exact citation text and refresh byte pins after
  directive-only library maintenance. Refuses if cited text is absent or no
  longer unique; it does not invent or alter an interpretation."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(def manifest-path "checks/F11-find-comparison-manifest.edn")

(defn- digest [algorithm bytes]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (MessageDigest/getInstance algorithm) bytes))))

(defn- file-bytes [path]
  (java.nio.file.Files/readAllBytes (.toPath (java.io.File. path))))

(defn- blob-sha [content]
  (let [prefix (.getBytes (str "blob " (alength content) "\u0000") "UTF-8")]
    (digest "SHA-1" (byte-array (concat prefix content)))))

(defn- relocate [span]
  (let [file (:file span)
        source (vec (str/split-lines (slurp file)))
        cited (vec (str/split-lines (:text span)))
        width (count cited)
        starts (vec (for [i (range (inc (- (count source) width)))
                          :when (= cited (subvec source i (+ i width)))]
                      (inc i)))]
    (when-not (= 1 (count starts))
      (throw (ex-info "Reviewed citation text is absent or ambiguous"
                      {:file file :matches starts :text (:text span)})))
    (let [start (first starts)
          end (+ start (dec width))
          lines [start end]]
      (assoc span :lines lines :pointer (str file ":" start "-" end)))))

(defn- refresh-row [row]
  (let [spans (into {} (map (fn [[kind span]] [kind (relocate span)]))
                    (:authored-spans row))]
    (-> row
        (assoc :authored-spans spans)
        (update :query-mappings
                (fn [mappings]
                  (mapv (fn [mapping]
                          (let [{:keys [file pointer]} (get spans (:kind mapping))]
                            (assoc mapping :authored-span pointer)))
                        mappings))))))

(defn- refresh-pin [pin]
  (let [content (file-bytes (:file pin))]
    (assoc pin
           :sha256 (digest "SHA-256" content)
           :git-blob (blob-sha content))))

(defn -main [& _]
  (let [manifest (edn/read-string (slurp manifest-path))
        refreshed (-> manifest
                      (update :interpretation-table #(mapv refresh-row %))
                      (update-in [:source-basis :pins] #(mapv refresh-pin %)))]
    (with-open [writer (java.io.PrintWriter. manifest-path)]
      (binding [*out* writer]
        (pprint/pprint refreshed)))
    (println "refreshed" manifest-path
             "rows" (count (:interpretation-table refreshed))
             "pins" (count (get-in refreshed [:source-basis :pins])))))
