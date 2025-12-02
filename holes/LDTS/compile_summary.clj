(ns compile-summary
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; Data loading

(def specs-path (or (System/getenv "FLEXIARG_SPECS") "library/t4r/specs.edn"))

(defn load-specs [path]
  (try
    (-> path slurp edn/read-string)
    (catch java.io.FileNotFoundException _
      (throw (ex-info (str "Spec file not found: " path) {:path path})))
    (catch Exception ex
      (throw (ex-info (str "Unable to parse specs: " path)
                      {:path path}
                      ex)))))

(def section-specs (load-specs specs-path))

;;; Source code

(defn arg-nodes
  "Depth-first walk of the argument tree."
  [node]
  (when node
    (let [supports (concat (:supports node) (:illustrates node))]
      (cons node (mapcat arg-nodes supports)))))

(defn include-node? [node]
  (contains? #{:conclusion :premise :example} (:role node)))

(defn format-length [[lo hi]]
  (cond
    (and lo hi) (format "%s–%s words" lo hi)
    lo          (format "at least %s words" lo)
    hi          (format "up to %s words" hi)
    :else       "unspecified length"))

(defn tone->text [tone]
  (cond
    (keyword? tone) (name tone)
    (string? tone)  tone
    :else           "unspecified"))

(defn join-list [xs]
  (->> xs (remove nil?) (remove str/blank?) (str/join ", ")))

(def stop-words
  #{"a" "an" "and" "are" "as" "at" "be" "by" "for" "from" "in" "into" "is" "it"
    "its" "of" "on" "or" "that" "the" "their" "this" "to" "with" "we" "our" "your"
    "they" "them" "can" "will" "not" "but" "so" "such" "these" "those" "each" "more"
    "most" "across" "one" "two" "three"})

(defn tokenize [s]
  (when s
    (re-seq #"[a-z0-9\-]+" (str/lower-case s))))

(defn significant-words [s]
  (->> (tokenize s)
       (remove #(or (stop-words %) (<= (count %) 3)))
       set))

(defn role->label [node]
  (-> (:role node) name str/upper-case))

(defn rel->label [rel]
  (when rel
    (str " (" (name rel) ")")))

(defn build-outline [spec]
  (let [root (:argument spec)
        nodes (arg-nodes root)]
    {:meta {:id (:id spec)
            :title (:title spec)
            :audience (:audience spec)
            :tone (:tone spec)
            :length (:length spec)
            :constraints (:constraints spec)}
     :backbone (->> nodes
                    (filter include-node?)
                    (map (fn [n]
                           {:id (:id n)
                            :role (:role n)
                            :rel (:relation-to-parent n)
                            :text (:text n)})))
     :holes (->> nodes
                 (filter #(= (:role %) :hole))
                 (map (fn [n]
                        {:id (:id n)
                         :prompt (:prompt n)
                         :required? (:required? n)})))}))

(def coverage-roles #{:premise :conclusion})

(defn coverage-threshold [words]
  (let [cnt (max 1 (count words))]
    (max 1 (min 5 (int (Math/ceil (* 0.3 cnt)))))))

(defn coverage-report [spec summary]
  (let [summary-words (significant-words summary)
        nodes (->> spec :argument arg-nodes (filter #(coverage-roles (:role %))))]
    (reduce (fn [acc node]
              (let [node-words (significant-words (:text node))]
                (if (empty? node-words)
                  acc
                  (let [overlap (count (set/intersection summary-words node-words))
                        needed (coverage-threshold node-words)
                        ok? (>= overlap needed)]
                    (if ok?
                      acc
                      (conj acc {:id (:id node)
                                 :text (:text node)
                                 :overlap overlap
                                 :needed needed}))))))
            []
            nodes)))

(defn grade-coverage [spec summary]
  (let [missing (coverage-report spec summary)]
    {:ok? (empty? missing)
     :missing missing}))

(def banned-phrases
  [{:label :supplement :pattern #"\bsupplement\b"}
   {:label :pilot :pattern #"\bpilot\b"}
   {:label :consent-ledger :pattern #"consent ledger[s]?"}
   {:label :multilingual-tags :pattern #"multilingual tag[s]?"}
   {:label :os-metaphor :pattern #"operating system|\bros\b"}])

(defn spec-text [spec]
  (->> (:argument spec)
       arg-nodes
       (map :text)
       (remove str/blank?)
       (str/join " \n")))

(defn banned-content-report [spec summary]
  (let [spec-text (str/lower-case (spec-text spec))
        summary-text (str/lower-case summary)]
    (reduce (fn [acc {:keys [label pattern]}]
              (if (and (re-find pattern summary-text)
                       (not (re-find pattern spec-text)))
                (conj acc label)
                acc))
            []
            banned-phrases)))

(defn grade-garbage [constraints summary]
  (let [phrases (:banned-phrases constraints)
        lower (str/lower-case summary)
        hits (if (seq phrases)
               (->> phrases
                    (filter #(str/includes? lower (str/lower-case %)))
                    vec)
               [])]
    {:ok? (empty? hits)
     :hits hits}))

(def relation-hints
  {:because        [#"\bbecause\b" #"\bsince\b" #"\bas\b"]
   :therefore      [#"\btherefore\b" #"\bso\b" #"as a result" #"consequently" #"thus"]
   :underpinned-by [#"underpin" #"principle" #"ground" #"anchored"]
   :specifies      [#"principle" #"includes" #"consists" #"comprises"]})

(defn relation-report [spec summary]
  (let [summary-text (str/lower-case summary)
        relations (->> (:argument spec)
                       arg-nodes
                       (keep :relation-to-parent)
                       set)]
    (reduce (fn [acc rel]
              (if-let [patterns (relation-hints rel)]
                (if (some #(re-find % summary-text) patterns)
                  acc
                  (conj acc rel))
                acc))
            []
            relations)))

(defn grade-relations [spec summary]
  (let [missing (relation-report spec summary)]
    {:ok? (empty? missing)
     :missing missing}))

(def tone-banned-words
  #{"transformative" "revolutionary" "unlock" "radically" "game-changing"})

(defn tone-report [summary]
  (let [words (set (tokenize summary))]
    (->> tone-banned-words
         (filter #(contains? words %))
         vec)))

(defn grade-tone [summary]
  (let [violations (tone-report summary)]
    {:ok? (empty? violations)
     :violations violations}))

(defn word-count [s]
  (if (str/blank? s)
    0
    (count (str/split (str/trim s) #"\s+"))))

(defn word-count-report [summary [lo hi]]
  (let [cnt (word-count summary)
        lo* (or lo 0)
        hi* (or hi (+ lo* 100))
        slack 10
        ok? (and (>= cnt (- lo* slack)) (<= cnt (+ hi* slack)))]
    {:word-count cnt :ok? ok? :expected [lo hi]}))

(defn salient-terms [text]
  (let [tokens (vec (remove stop-words (tokenize text)))
        singles (filter #(>= (count %) 5) tokens)
        bigrams (->> (partition 2 1 tokens)
                     (map #(str (first %) " " (second %)))
                     (filter (fn [phrase]
                               (some #(>= (count %) 5)
                                     (str/split phrase #" ")))))]
    (set (concat singles bigrams))))

(defn grade-drift [spec summary]
  (let [allow? (get-in spec [:constraints :allow-new-claims?] true)
        arg-terms (salient-terms (spec-text spec))
        sum-terms (salient-terms summary)
        new-terms (set/difference sum-terms arg-terms)
        ordered (sort new-terms)]
    {:ok? (or allow? (empty? new-terms))
     :new-candidates ordered}))

(defn review-penalty [review]
  (+ (* 3 (count (get-in review [:coverage :missing])))
     (* 2 (count (get-in review [:garbage :hits])))
     (* 2 (if (get-in review [:drift :ok?]) 0 (count (get-in review [:drift :new-candidates]))))
     (if (get-in review [:relations :ok?]) 0 1)
     (if (get-in review [:tone :ok?]) 0 1)
     (if (get-in review [:word-count :ok?]) 0 1)))

(defn better-review? [a b]
  (or (nil? b)
      (< (review-penalty a) (review-penalty b))))

(defn critique-lines [review]
  (let [lines []
        lines (if-let [missing (seq (get-in review [:coverage :missing]))]
                 (into lines (map (fn [{:keys [id text]}]
                                    (format "Missing backbone %s: %s" id text))
                                  missing))
                 lines)
        lines (if-let [hits (seq (get-in review [:garbage :hits]))]
                 (conj lines (str "Banned phrases present: " (str/join ", " hits)))
                 lines)
        lines (if (and (not (get-in review [:drift :ok?]))
                       (seq (get-in review [:drift :new-candidates])))
                 (conj lines (str "Unapproved new claims: "
                                  (str/join ", " (take 10 (get-in review [:drift :new-candidates])))))
                 lines)
        lines (if-let [rels (seq (get-in review [:relations :missing]))]
                 (conj lines (str "Relation cues missing for: " (str/join ", " (map name rels))))
                 lines)
        lines (if-let [tones (seq (get-in review [:tone :violations]))]
                 (conj lines (str "Tone violations: " (str/join ", " tones)))
                 lines)
        lines (if-not (get-in review [:word-count :ok?])
                 (conj lines (format "Word count %d outside expected %s"
                                     (get-in review [:word-count :word-count])
                                     (pr-str (get-in review [:word-count :expected]))))
                 lines)]
    lines))

(defn critique-text [review]
  (let [lines (critique-lines review)]
    (if (empty? lines)
      "All constraints satisfied."
      (str "The previous draft had the following issues:\n"
           (->> lines (map #(str "- " %)) (str/join "\n"))))))

(declare build-prompt review)

(defn revision-directive [spec review]
  (let [issues (critique-text review)
        length-note (format-length (:length spec))]
    (str issues
         "\nRewrite the section so that every backbone point is covered, banned phrases are removed, and no unapproved claims remain.\n"
         "Keep the tone aligned with the style guidance and stay within " length-note ".")))

(defn shell-generator [cmd]
  (fn [{:keys [prompt iteration feedback]}]
    (let [base-env (into {} (System/getenv))
          env (merge base-env
                     {"PROMPT" prompt
                      "ITERATION" (str iteration)
                      "FEEDBACK" (or feedback "")})
          {:keys [exit out err]} (shell/sh "bash" "-lc" cmd :env env)]
      (when-not (zero? exit)
        (throw (ex-info "Generator command failed" {:exit exit :err err})))
      (str/trim out))))

(defn run-loop
  "Iteratively generate and grade summaries.
   `generator` takes {:prompt :iteration :feedback :history} and returns summary text.
   Returns {:prompt ... :history [...] :completed? bool :final {:iteration ... :summary ... :review ...}}."
  [spec generator]
  (let [prompt (build-prompt spec)
        max-it (max 1 (get-in spec [:constraints :max-iterations] 1))]
    (loop [iteration 1
           feedback nil
           history []
           best-entry nil]
      (let [request {:prompt prompt
                     :iteration iteration
                     :feedback feedback
                     :history history}
            summary (generator request)]
        (when (nil? summary)
          (throw (ex-info "Generator returned nil" {:iteration iteration})))
        (let [analysis (review spec summary)
              entry {:iteration iteration
                     :summary summary
                     :review analysis}
              history' (conj history entry)
              best' (if (better-review? analysis (some-> best-entry :review)) entry best-entry)]
          (cond
            (:ok? analysis)
            {:prompt prompt
             :completed? true
             :history history'
             :final entry}

            (>= iteration max-it)
            {:prompt prompt
             :completed? false
             :history history'
             :final (or best' entry)
             :feedback (critique-text analysis)}

            :else
            (recur (inc iteration)
                   (revision-directive spec analysis)
                   history'
                   best')))))))

(defn review [spec summary]
  (let [coverage (grade-coverage spec summary)
        garbage (grade-garbage (:constraints spec) summary)
        drift (grade-drift spec summary)
        relations (grade-relations spec summary)
        tone (grade-tone summary)
        wc (word-count-report summary (:length spec))
        checks [coverage garbage drift relations tone wc]
        ok? (every? :ok? checks)]
    {:ok? ok?
     :coverage coverage
     :garbage garbage
     :drift drift
     :relations relations
     :tone tone
     :word-count wc}))

(defn validate-summary [spec summary]
  (review spec summary))

(defn format-backbone [backbone]
  (->> backbone
       (map-indexed (fn [idx node]
                      (format "%d. %s%s: %s"
                              (inc idx)
                              (role->label node)
                              (or (rel->label (:rel node)) "")
                              (:text node))))
       (str/join "\n")))

(defn build-prompt [spec]
  (let [{:keys [meta backbone holes]} (build-outline spec)
        {:keys [id title audience tone length constraints]} meta
        {:keys [style word-limit banned-phrases allow-new-claims?]} constraints
        instructions (->> [(str "- Use every backbone point; merge related ones where natural.")
                           (when word-limit
                             (str "- Stay within ~" word-limit " words unless the length range requires otherwise."))
                           (when style (str "- Match the tone/style: " style "."))
                           (when (seq banned-phrases)
                             (str "- Do not use these banned phrases: " (str/join ", " banned-phrases) "."))
                           (when (false? allow-new-claims?)
                             "- Do not introduce new factual claims beyond what the backbone implies.")
                           "- Output one cohesive paragraph (no bullets)."
                           "- Keep language concrete; avoid vague promises or hype."]
                         (remove nil?)
                         (str/join "\n"))
        hole-lines (when (seq holes)
                     (str "\nOutstanding prompts:\n"
                          (->> holes
                               (map (fn [{:keys [prompt required?]}]
                                      (format "- %s%s" prompt (when required? " (required)"))))
                               (str/join "\n"))
                          "\n"))]
    (str "You are drafting the section \"" title "\".\n\n"
         (when (seq audience)
           (str "Audience: " (join-list audience) "\n"))
         (when tone
           (str "Tone hint: " (tone->text tone) "\n"))
         (when style
           (str "Style: " style "\n"))
         (when length
           (str "Length target: " (format-length length) "\n"))
         "\nRequired argument backbone:\n\n"
         (format-backbone backbone)
         "\n\nInstructions:\n"
         instructions
         (when hole-lines hole-lines))))

(defn spec->prompt [spec]
  (build-prompt spec))

(def specs-by-id
  (into {} (map (fn [spec] [(:id spec) spec]) section-specs)))

(def prompts-by-id
  (into {} (map (fn [spec] [(:id spec) (spec->prompt spec)]) section-specs)))

(defn print-prompt! [id]
  (if-let [prompt (get prompts-by-id id)]
    (do (println "=====" id "=====")
        (println prompt)
        (println))
    (binding [*out* *err*]
      (println "Unknown prompt id" id))))

(defn print-all-prompts! []
  (doseq [id (map :id section-specs)]
    (print-prompt! id)))

(defn validate-file! [id path]
  (if-let [spec (get specs-by-id id)]
    (let [summary (slurp path)
          report (validate-summary spec summary)]
      (println (format "Validation report for %s (source: %s):" id path))
      (println (with-out-str (pprint/pprint report)))
      report)
    (binding [*out* *err*]
      (println "Unknown spec id" id)
      nil)))

(defn drift-report! [id path]
  (if-let [spec (get specs-by-id id)]
    (let [summary (slurp path)
          report (grade-drift spec summary)]
      (println (format "Drift report for %s (source: %s):" id path))
      (println (with-out-str (pprint/pprint report)))
      report)
    (binding [*out* *err*]
      (println "Unknown spec id" id)
      nil)))

(defn print-run-history [history]
  (doseq [{:keys [iteration summary review]} history]
    (println "===== Iteration" iteration "=====")
    (println summary)
    (println)
    (println "Review OK?:" (:ok? review))
    (when-not (:ok? review)
      (println (critique-text review)))
    (println)))

(defn run-with-command! [id cmd]
  (if-let [spec (get specs-by-id id)]
    (let [result (run-loop spec (shell-generator cmd))]
      (print-run-history (:history result))
      (if (:completed? result)
        (println "Loop completed successfully in" (count (:history result)) "iteration(s).")
        (do (println "Loop hit max iterations; best attempt shown above.")
            (println "Final feedback:")
            (println (critique-text (get-in result [:final :review])))))
      result)
    (binding [*out* *err*]
      (println "Unknown spec id" id)
      nil)))

(defn -main
  "CLI entrypoint. Usage:
   - No args: print all prompts.
   - prompt <id>... : print specific prompts.
   - validate <id> <summary-file>: run validation heuristics on summary text.
   - drift <id> <summary-file>: show candidate new claims.
   - run <id> \"<shell command>\": execute iterative loop via external generator command (PROMPT/FEEDBACK env vars)."
  [& args]
  (let [[cmd & rest] args]
    (case cmd
      nil (print-all-prompts!)
      "prompt" (if (seq rest)
                  (doseq [id rest] (print-prompt! id))
                  (print-all-prompts!))
      "validate" (let [[id path] rest]
                    (if (and id path)
                      (validate-file! id path)
                      (binding [*out* *err*]
                        (println "Usage: clojure -M -m compile-summary validate <id> <file>"))))
      "drift" (let [[id path] rest]
                 (if (and id path)
                   (drift-report! id path)
                   (binding [*out* *err*]
                     (println "Usage: clojure -M -m compile-summary drift <id> <file>"))))
      "run" (let [[id & cmd-parts] rest
                   cmd (str/join " " cmd-parts)]
              (if (and id (not (str/blank? cmd)))
                (run-with-command! id cmd)
                (binding [*out* *err*]
                  (println "Usage: clojure -M -m compile-summary run <id> \"<shell command>\""))))
      (binding [*out* *err*]
        (println "Unknown command" cmd)
        (println "Usage: clojure -M -m compile-summary [prompt <ids> | validate <id> <file> | drift <id> <file> | run <id> <cmd>]")))))
