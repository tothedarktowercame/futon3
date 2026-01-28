(ns scripts.iiching-sync-exotype
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-edn [path]
  (edn/read-string (slurp path)))

(defn hamming-weight [bits]
  (count (filter #{\1} bits)))

(defn zero-pad [n width]
  (format (str "%0" width "d") n))

(defn bits->int [bits]
  (Integer/parseInt bits 2))

(defn bits->hex [bits]
  (format "0x%02X" (bits->int bits)))

(defn manifest-entry [manifest bits]
  (first (filter #(= bits (:bits %)) manifest)))

(defn lift-patterns [lift-registry sigil]
  (vec (filter #(= sigil (:sigil %)) (:patterns lift-registry))))

(defn encode-bits-list [bits]
  (str/join " " (map str bits)))

(defn exotype-path [root n]
  (str (io/file root (str "exotype-" (zero-pad n 3) ".flexiarg"))))

(defn program-block [entry]
  (let [inputs (:inputs entry)
        program (:program entry)]
    (str "@exotype-program\n"
         "  :program-id " (:template program) "\n"
         "  :program-name \"" (name (:template program)) "\"\n"
         "  :program-kind " (:semantics program) "\n"
         "  :tier " (:tier entry) "\n"
         "  :scope " (:scope entry) "\n"
         "  :inputs " (pr-str inputs) "\n"
         "  :outputs " (pr-str (:outputs entry)) "\n"
         "  :invariants " (pr-str (:invariants entry)) "\n"
         "  :word-variation " (:word-variation entry) "\n"
         "  :notes \"From futon5/resources/exotype-program-manifest.edn.\"\n")))

(defn lift-block [entry lift]
  (let [pattern-ids (mapv :pattern-id lift)
        evidence (mapv (fn [m]
                         (cond-> {:pattern-id (:pattern-id m)
                                  :source (:source m)
                                  :source-path "futon5/resources/exotype-xenotype-lift.edn"}
                           (:ct-template m) (assoc :ct-template (:ct-template m))
                           (:lift-rules m) (assoc :lift-rules (:lift-rules m))))
                       lift)
        ct-template (some :ct-template lift)
        lift-rules (some :lift-rules lift)]
    (str "@exotype-lift\n"
         "  :pattern-ids " (pr-str pattern-ids) "\n"
         "  :ct-template " (if ct-template (pr-str ct-template) "nil") "\n"
         "  :lift-rules " (if lift-rules (pr-str lift-rules) "nil") "\n"
         "  :evidence " (pr-str evidence) "\n"
         "  :notes \""
         (if (seq lift)
           "Lift registry evidence from futon5/resources/exotype-xenotype-lift.edn."
           (str "No lift mapping for bits " (:bits entry)
                " in futon5/resources/exotype-xenotype-lift.edn."))
         "\"\n")))

(defn build-content [template entry lift]
  (let [bits (:bits entry)
        number (bits->int bits)
        hex (bits->hex bits)
        hamming (hamming-weight bits)
        base (-> template
                 (str/replace "Exotype XYZ is encoded; program and lift semantics pending futon5 mapping."
                              (if (seq lift)
                                (str "Exotype " (zero-pad number 3) " is encoded; program/params mapped from futon5; lift registry evidence recorded.")
                                (str "Exotype " (zero-pad number 3) " is encoded; program/params mapped from futon5; lift mapping not yet in registry.")))
                 (str/replace "Exotype XYZ (0xYY)" (str "Exotype " (zero-pad number 3) " (" hex ")"))
                 (str/replace "Exotype XYZ" (format "Exotype %s" (zero-pad number 3)))
                 (str/replace "@bits 00000000" (str "@bits " bits))
                 (str/replace "@number 0" (str "@number " number))
                 (str/replace "@hex 0x00" (str "@hex " hex))
                 (str/replace "@hamming-weight 0" (str "@hamming-weight " hamming))
                 (str/replace "iiching/exotype-XYZ" (str "iiching/exotype-" (zero-pad number 3)))
                 (str/replace "Canonical 8-bit encoding for exotype XYZ."
                              (str "Canonical 8-bit encoding for exotype " (zero-pad number 3) "."))
                 (str/replace "b7 b6 b5 b4 b3 b2 b1 b0" (encode-bits-list bits))
                 (str/replace "blocked-by[futon5-exotypes]"
                              (if (seq lift) "Confirm lift rules for this exotype in futon5 lift registry."
                                  "blocked-by[futon5-exotype-lift]"))
                 (str/replace "Program, lift, and parameter mappings are not yet recorded here."
                              (if (seq lift)
                                "The lift registry records pattern bindings but no lift-rules."
                                "The lift registry has no mapping for this exotype yet."))
                 (str/replace "Use the encoding fields for identification and attach futon5 semantics\n    when available."
                              (if (seq lift)
                                "Use the encoding fields for identification, the manifest for program/params,\n    and the lift registry for pattern evidence."
                                "Use the encoding fields for identification and the futon5 manifest for\n    program/params until lift mappings land."))
                 (str/replace "@futon5-ref" "@futon5-ref futon5/resources/exotype-program-manifest.edn; futon5/resources/exotype-xenotype-lift.edn; futon5/src/futon5/ct/exotype_programs.clj; futon5/src/futon5/mmca/exotype.clj"))]
    (-> base
        (str/replace #"(?s)@exotype-program[\s\S]*\z"
                     (str (program-block entry) "\n\n"
                          (lift-block entry lift) "\n\n"
                          "@exotype-params\n"
                          "  :rotation " (get-in entry [:params :rotation]) "\n"
                          "  :match-threshold " (get-in entry [:params :match-threshold]) "\n"
                          "  :invert-on-phenotype? " (get-in entry [:params :invert-on-phenotype?]) "\n"
                          "  :update-prob " (get-in entry [:params :update-prob]) "\n"
                          "  :mix-mode " (get-in entry [:params :mix-mode]) "\n"
                          "  :mix-shift " (get-in entry [:params :mix-shift]) "\n"
                          "  :notes \"From futon5/resources/exotype-program-manifest.edn.\"\n")))))

(defn -main [& args]
  (let [[root manifest-path lift-path template-path & nums] args]
    (when (or (nil? root) (nil? manifest-path) (nil? lift-path) (nil? template-path) (empty? nums))
      (binding [*out* *err*]
        (println "Usage: clojure -M -m iiching.sync-exotype <iiching-root> <manifest.edn> <lift.edn> <template.flexiarg> <exotype-number>...")
        (System/exit 1)))
    (let [manifest (parse-edn manifest-path)
          lift (parse-edn lift-path)
          template (slurp template-path)]
      (doseq [n-str nums]
        (let [n (Integer/parseInt n-str)
              bits (format "%8s" (Integer/toBinaryString n))
              bits (str/replace bits " " "0")
              entry (manifest-entry manifest bits)
              _ (when-not entry
                  (binding [*out* *err*]
                    (println "No manifest entry for" bits)
                    (System/exit 1)))
              sigil (:sigil entry)
              lift-entries (lift-patterns lift sigil)
              content (build-content template entry lift-entries)
              path (exotype-path root n)]
          (spit path content)
          (println "Wrote" path))))))
