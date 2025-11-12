(ns f2.repl
  "Optional REPL module with SAFE (SCI) and ADMIN (unsafe) evaluators."
  (:require [sci.core :as sci]
            [clojure.set :as set]))

(def max-code-chars (* 64 1024))
(def max-result-chars (* 1024 1024))

(def disallowed-symbols
  "Symbols that are not permitted inside the SAFE interpreter."
  #{'ns 'in-ns 'require 'use 'import 'load-file 'load-reader 'load-string
    'refer 'refer-clojure 'set! 'alter-var-root 'binding 'eval 'future 'promise
    'atom 'reset! 'swap! 'def 'defn 'defmacro 'var 'resolve 'bean 'proxy 'reify
    'doseq 'dotimes 'loop 'while 'agent 'send 'send-off})

(def safe-core
  {'+ +
   '- -
   '* *
   '/ /
   '= =
   'not not
   '< <
   '> >
   '<= <=
   '>= >=
   'str str
   'subs subs
   'count count
   'first first
   'rest rest
   'next next
   'seq seq
   'empty? empty?
   'not-empty not-empty
   'list list
   'vector vector
   'hash-map hash-map
   'zipmap zipmap
   'get get
   'get-in get-in
   'assoc assoc
   'assoc-in assoc-in
   'dissoc dissoc
   'update update
   'update-in update-in
   'merge merge
   'merge-with merge-with
   'keys keys
   'vals vals
   'contains? contains?
   'into into
   'conj conj
   'cons cons
   'map map
   'mapv mapv
   'reduce reduce
   'filter filter
   'remove remove
   'keep keep
   'some some
   'every? every?
   'any? any?
   'take take
   'drop drop
   'take-while take-while
   'drop-while drop-while
   'partition partition
   'partition-all partition-all
   'range range
   'apply apply
   'identity identity
   'constantly constantly
   'boolean boolean
   'true? true?
   'false? false?
   'nil? nil?
   'inc inc
   'dec dec
   'max max
   'min min
   'ffirst ffirst
   'frequencies frequencies
   'group-by group-by
   'sort sort
   'sort-by sort-by
   'reverse reverse
   'interleave interleave
   'interpose interpose
   'dedupe dedupe})

(def safe-set
  {'union set/union
   'intersection set/intersection
   'difference set/difference
   'subset? set/subset?
   'superset? set/superset?})

(defn- namespace-config [dsl]
  (cond-> {'clojure.core safe-core
           'clojure.set safe-set}
    (seq dsl) (assoc 'musn.api dsl)))

(defn- make-sci-context [dsl]
  (sci/init {:classes {:allow []}
             :namespaces (namespace-config dsl)
             :deny disallowed-symbols}))

(defn- ensure-code! [code]
  (when-not (string? code)
    (throw (ex-info "code-required" {:code code})))
  (when (empty? code)
    (throw (ex-info "code-required" {:code code})))
  (when (> (count code) max-code-chars)
    (throw (ex-info "code-too-large" {:limit max-code-chars :size (count code)})))
  code)

(defn- ensure-result! [value]
  (let [rendered (pr-str value)]
    (when (> (count rendered) max-result-chars)
      (throw (ex-info "result-too-large" {:limit max-result-chars :size (count rendered)})))
    value))

(defn- safe-exec [code dsl]
  (let [ctx (make-sci-context dsl)
        result (sci/eval-string* ctx code)]
    (ensure-result! result)))

(defn- admin-exec [code]
  (let [result (binding [*ns* (the-ns 'user)]
                 (load-string code))]
    (ensure-result! result)))

(defn- allowed-remote? [config remote]
  (let [allow (or (:admin-allow config) #{"127.0.0.1" "::1"})]
    (contains? (set allow) remote)))

(defn- admin-authorized? [config remote token]
  (and (= token (:admin-token config))
       (some? token)
       remote
       (allowed-remote? config remote)))

(defn execute
  "Evaluate CODE according to CONFIG and REQUEST data.
   CONFIG {:mode :off|:safe|:admin, :admin-token 'secret, :admin-allow #{...}}
   REQUEST {:code \"(+ 1 2)\" :mode :safe|:admin :token \"...\" :dsl {...} :remote-addr \"...\"}"
  [config {:keys [code mode token dsl remote-addr]}]
  (let [config-mode (:mode config :off)
        mode* (keyword (or mode config-mode))]
    (try
      (ensure-code! code)
      (cond
        (= config-mode :off)
        {:ok false :err "repl-disabled"}

        (and (= mode* :admin) (not= config-mode :admin))
        {:ok false :err "admin-disabled"}

        (not (#{:safe :admin} mode*))
        {:ok false :err "invalid-mode"}

        (= mode* :safe)
        (try
          {:ok true :mode :safe :result (safe-exec code (or dsl {}))}
          (catch Exception ex
            {:ok false :err "safe-eval-error" :message (.getMessage ex)}))

        (= mode* :admin)
        (if (admin-authorized? config remote-addr token)
          (try
            {:ok true :mode :admin :result (admin-exec code)}
            (catch Exception ex
              {:ok false :err "admin-eval-error" :message (.getMessage ex)}))
          {:ok false :err "admin-auth-failed"}))
      (catch Exception ex
        (let [data (ex-data ex)]
          {:ok false
           :err (or (:err data) (.getMessage ex) "repl-error")
           :message (.getMessage ex)})))))
