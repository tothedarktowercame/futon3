(ns python-projection
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def src-exts #{"py"})

(def ^:private python-bin
  (or (System/getenv "PYTHON_BIN")
      (System/getenv "PYTHON")
      "python3"))

(def ^:private helper-path
  (str (fs/parent *file*) "/python_ast_helper.py"))

(defn- run-helper-many [paths]
  (let [input (str (str/join "\n" paths) "\n")
        {:keys [exit out err]} (sh python-bin helper-path :in input)]
    (when-not (zero? exit)
      (throw (ex-info "python_ast_helper.py failed"
                      {:paths paths :exit exit :err err})))
    (->> (str/split-lines out)
         (remove str/blank?)
         (map edn/read-string))))

(defn- ->alias-map [imports]
  (into {}
        (map (fn [[local target]]
               [(symbol local) (symbol target)]))
        imports))

(defn- ->body-syms [syms]
  (set (map symbol syms)))

(defn- ->vars [ns-name defs]
  (mapv (fn [{:strs [name kind has-doc body-syms]}]
          {:vertex/type :var
           :var/ns ns-name
           :var/name name
           :var/qname (str ns-name "/" name)
           :var/kind kind
           :var/has-doc has-doc
           :var/syms (->body-syms body-syms)})
        defs))

(defn- ->tests [ns-name tests]
  (mapv (fn [{:strs [name body-syms]}]
          {:vertex/type :test
           :test/ns ns-name
           :test/name name
           :test/qname (str ns-name "/" name)
           :test/syms (->body-syms body-syms)})
        tests))

(defn collect-files
  "Project many .py files and return a path-keyed projection map."
  [paths]
  (let [abs-paths (mapv str paths)]
    (into {}
          (map (fn [{:strs [path module imports defs tests is-test?]}]
                 [path
                  {:ns module
                   :aliases (->alias-map imports)
                   :vars (->vars module defs)
                   :tests (->tests module tests)
                   :is-test? is-test?}]))
          (run-helper-many abs-paths))))

(defn collect-file
  "Project one .py file into the shared substrate-2 metadata shape."
  [path]
  (get (collect-files [path]) (str path)))
