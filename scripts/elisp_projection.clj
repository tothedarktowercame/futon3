(ns elisp-projection
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def src-exts #{"el"})

(def def-forms
  #{'defun 'defun-
    'defvar 'defconst 'defcustom
    'defmacro 'cl-defmacro
    'cl-defun 'cl-defmethod 'cl-defgeneric})

(def test-forms #{'ert-deftest})

(defn sanitize-source [text]
  ;; Elisp uses builtin function symbols like `1+` / `1-` that Clojure's
  ;; reader treats as malformed numbers. We only need stable form structure
  ;; for symbol harvesting, so reader-safe placeholders are sufficient here.
  (-> text
      (str/replace "1+" "one-plus")
      (str/replace "1-" "one-minus")))

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (java.io.StringReader.
                                           (sanitize-source (slurp f))))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try
                     (read {:eof ::eof} pbr)
                     (catch Exception _ ::eof))]
          (if (= form ::eof)
            acc
            (recur (conj acc form))))))))

(defn quoted-symbol [form]
  (cond
    (symbol? form) form
    (and (seq? form)
         (= 'quote (first form))
         (symbol? (second form)))
    (second form)
    :else nil))

(defn provide-feature [forms]
  (some->> forms
           reverse
           (some (fn [form]
                   (when (and (seq? form)
                              (= 'provide (first form)))
                     (quoted-symbol (second form)))))
           str))

(defn file-feature [path]
  (-> path
      fs/file-name
      (str/replace #"\.el$" "")
      (str/replace #"_" "-")))

(defn file-namespace [path forms]
  (or (provide-feature forms)
      (file-feature path)))

(defn require-features [forms]
  (into {}
        (keep (fn [form]
                (when (and (seq? form)
                           (= 'require (first form)))
                  (when-let [feature (quoted-symbol (second form))]
                    [feature feature]))))
        forms))

(defn defn-body [form]
  (drop 2 form))

(defn collect-symbols [body]
  (let [syms (atom #{})]
    (walk/postwalk
     (fn [x]
       (when (symbol? x)
         (swap! syms conj x))
       x)
     body)
    @syms))

(defn elisp-test-file? [path forms]
  (or (str/includes? path "/test/")
      (str/includes? path "/tests/")
      (str/ends-with? path "-test.el")
      (str/ends-with? path "_test.el")
      (some (fn [form]
              (and (seq? form)
                   (test-forms (first form))))
            forms)))

(defn extract-defs
  "Return {:vars [...] :tests [...]} for one elisp file."
  [ns-name is-test? forms]
  (let [vars (atom [])
        tests (atom [])]
    (doseq [form forms :when (and (seq? form) (symbol? (first form)))]
      (let [head (first form)]
        (cond
          (test-forms head)
          (let [tname (second form)]
            (when (symbol? tname)
              (swap! tests conj
                     {:vertex/type :test
                      :test/ns     ns-name
                      :test/name   (str tname)
                      :test/qname  (str ns-name "/" tname)
                      :test/syms   (collect-symbols (defn-body form))})))

          (def-forms head)
          (let [vname (second form)
                rest-f (drop 2 form)
                docstr (when (string? (first rest-f)) (first rest-f))]
            (when (symbol? vname)
              (swap! vars conj
                     {:vertex/type :var
                      :var/ns      ns-name
                      :var/name    (str vname)
                      :var/qname   (str ns-name "/" vname)
                      :var/kind    (str head)
                      :var/has-doc (some? docstr)
                      :var/syms    (collect-symbols (defn-body form))}))))))
    {:vars (if is-test? [] @vars)
     :tests (if is-test? @tests [])
     :aliases {}}))

(defn collect-file
  "Project one .el file into {:ns :aliases :vars :tests :is-test?}."
  [path]
  (let [forms (read-forms (io/file path))
        ns-name (file-namespace path forms)
        aliases (require-features forms)
        is-test? (elisp-test-file? path forms)
        {:keys [vars tests]} (extract-defs ns-name is-test? forms)]
    {:ns ns-name
     :aliases aliases
     :vars vars
     :tests tests
     :is-test? is-test?}))
