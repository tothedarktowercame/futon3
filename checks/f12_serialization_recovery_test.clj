(ns f12-serialization-recovery-test
  "Proposal qualification only: no recovered deposit written or primary executed."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [f12-preparation :as a]))
(def paths
  ["/home/joe/code/futon3/checks/find_organise.clj"
   "/home/joe/code/futon2/holes/labs/wm-contract/runs/separated-risk-certificate/runtime-mass-binding.lean"])
(def raw-dir "checks/fixtures/f12-preparation/")
(def specimens
  [["primary-attempt-1-2026-09-10.edn.txt"
    "7e00351de0433994904c33b7355f5bf13b2873c98ff735c10b5450dab0e3f536" [:before :after]]
   ["adapter-diagnostic.edn"
    "2dff3708000161a7d7ca2217739980ea5feb2043ce41a2ba587131170a910dcc" [:baseline :intervention]]])
(defn replacements []
  (mapv (fn [path] [(str "#:{:" (subs path 1)) (str "{" (pr-str path))]) paths))
(defn candidate [raw]
  (assert (= 8 (count (re-seq #"#:\{:home/joe/" raw))))
  (let [text (reduce (fn [s [old new]] (str/replace s old new)) raw (replacements))
        inverse (reduce (fn [s [old new]] (str/replace s new old)) text (replacements))]
    (assert (= raw inverse) "transformation not byte-reversible")
    text))
(defn hash-text [s]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest d (.getBytes s "UTF-8"))))))
(deftest qualify-proposed-recovery
  (doseq [[file sha arms] specimens]
    (let [path (str raw-dir file)
          raw (slurp path)]
      (is (= sha (a/sha256-file path)))
      (is (thrown? Exception (edn/read-string raw)))
      (let [text (candidate raw)
            reader (java.io.PushbackReader. (java.io.StringReader. text))
            deposit (edn/read reader)
            record (if (= file "primary-attempt-1-2026-09-10.edn.txt") (:result deposit) deposit)]
        (is (= ::eof (edn/read {:eof ::eof} reader)))
        (is (= deposit (edn/read-string (a/deposit-text deposit))))
        (doseq [arm arms
                :let [episode (get record arm)]]
          (is (= :both-match (get-in episode [:state :disposition])))
          (is (= (:acting-order episode) (mapv :rule (:transcript episode))))
          (doseq [id [:F :S]
                  :let [receipt (get-in episode [:state id])
                        mapping (:mapping receipt)
                        [logical physical] (first mapping)
                        emitted (first (filter #(= id (get-in % [:emission :key])) (:transcript episode)))]]
            (is (= receipt (get-in emitted [:emission :receipt])))
            (is (= 1 (count mapping)))
            (is (contains? (set paths) logical))
            (is (= (str (:cwd receipt) "/target") physical))
            (is (= (str/replace (:logical_stdin receipt) logical physical) (:stdin receipt)))
            (is (= (str physical ": OK\n") (:stdout receipt)))
            (is (= 0 (:exit receipt)))
            (doseq [k [:stdin :stdout :stderr]]
              (is (= (hash-text (get receipt k)) (get receipt (keyword (str (name k) "_sha256"))))))))
        (when (= file "primary-attempt-1-2026-09-10.edn.txt")
          (is (= [1 1] (mapv #(get-in record [% :primary-score]) arms)))
          (is (true? (:instance-o4 record)))
          (is (false? (:parent-o4 record))))))))
(let [r (run-tests)] (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))
