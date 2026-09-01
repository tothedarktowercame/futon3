#!/usr/bin/env bb
(ns checks.library-graph-lint
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def edge-kinds
  "The three relations declared in README-flexiarg §5a. Each is a claim someone
  makes in their own voice: @why by the pattern's author, @how and @see-also by
  an editor."
  #{:why :how :see-also})

;; A post-hoc @why is a trace, not a cause (README-flexiarg §5a, Joe 2026-08-23),
;; and the standard asks that it be marked inline. The interim spelling is its
;; own directive -- holes/labs/library-contract/decisions.edn :posthoc-why-syntax,
;; arm :distinct-directive -- so that every consumer that matches the literal
;; `@why ` keeps reading zero of them until it opts in. What the lint counts
;; separately, and what it counts together, is written at each use below.
(def posthoc-why-kind :why-posthoc)
(def counted-edge-kinds (conj edge-kinds posthoc-why-kind))
(def target-pattern #"[A-Za-z0-9_.-]+/[A-Za-z0-9_./'-]+")
(def evidence-store "http://127.0.0.1:7073")
(def evidence-page-limit 1000)
(def reflection-rule-version 3)
;; v3 adds the authoring-turn class: v2 knew only who ran the spider, so a
;; pattern's own author listing the references they had just written into the
;; file read as external corroboration (wave 2, system-coherence, e-7a50b862).
;; The class is the record author's OWN authoring act. A record that reports
;; somebody else's -- an operator relaying what codex-15 wrote -- is external
;; use and stays non-reflection; see authoring-verb-attributed-to-author?.
(def authoring-verb-window 120)
(def pattern-file-path #"[A-Za-z0-9_./'-]+\.flexiarg")
(def authoring-verb
  #"(?i)\b(?:authored|authoring|wrote|written|writing|created|creating|drafted|drafting|added|adding)\b")
;; An authoring verb near a path says a pattern file was authored; it does not
;; say the record's author is who authored it. These three read the clause
;; around the verb to settle that.
(def authoring-subject-tail
  "The token immediately before an authoring verb, adverbs and quoting skipped."
  #"(?i)[`\"'(\[]?([A-Za-z][A-Za-z0-9_.'-]*)[`\"')\]:;,]*\s+(?:(?:just|already|then|later|recently|independently|subsequently|also|finally|first|initially|earlier|since)\s+)*$")
(def first-person-subject #"(?i)i|we|us|my|our|i've|we've|i'd|we'd")
(def named-agent-subject
  "A subject that names somebody: a proper name, an agent id, or a job id --
  capitalised, hyphenated, or carrying a digit."
  #"[A-Z][A-Za-z]*|[A-Za-z][A-Za-z0-9_.']*(?:-[A-Za-z0-9_.']+)+|[A-Za-z][A-Za-z0-9_.'-]*\d[A-Za-z0-9_.'-]*")
(def authoring-complement
  "A preposition right after the verb: the clause says where the file was
  authored and names no agent at all (`Pattern authored at <path>`)."
  #"(?i)^[\s`\"'(]*(?:at|in|into|to|under|as|onto|within|beside|alongside|here|there)\b")
(def authoring-complement-window 24)

(defn sha256 [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest digest (.getBytes s "UTF-8"))))))

(defn now [] (.toString (java.time.Instant/now)))

(defn read-http-edn [url]
  (loop [attempt 0]
    (let [result (try {:value (edn/read-string (slurp url))}
                      (catch Exception e {:error e}))]
      (if-let [error (:error result)]
        (if (< attempt 5)
          (do (Thread/sleep (* 1000 (inc attempt))) (recur (inc attempt)))
          (throw (ex-info "live evidence request failed"
                          {:url url :attempts (inc attempt)} error)))
        (:value result)))))

(defn encode-query [x]
  (java.net.URLEncoder/encode (str x) "UTF-8"))

(defn evidence-page-url [{:keys [limit since cursor]}]
  (str evidence-store "/api/alpha/evidence?limit=" (or limit evidence-page-limit)
       (when since (str "&since=" (encode-query since)))
       (when cursor
         (str "&cursor-at=" (encode-query (:at cursor))
              "&cursor-id=" (encode-query (:id cursor))))))

(defn fetch-evidence-page [request]
  (read-http-edn (evidence-page-url request)))

(defn page-evidence
  "Read every keyset page. The empty terminal page remains visible in :pages."
  ([fetch-page] (page-evidence fetch-page {}))
  ([fetch-page {:keys [limit since on-page collect?]
                :or {limit evidence-page-limit on-page (fn [_ _] nil) collect? true}}]
   (loop [cursor nil page-number 0 entries [] pages [] seen-cursors #{}]
     (let [page (fetch-page {:limit limit :since since :cursor cursor})
           page-entries (vec (:entries page))
           next-cursor (:next-cursor page)]
       (when (and next-cursor (contains? seen-cursors next-cursor))
         (throw (ex-info "live evidence paging repeated a cursor"
                         {:page page-number :cursor next-cursor})))
       (on-page page-number page)
       (if next-cursor
         (recur next-cursor (inc page-number)
                (if collect? (into entries page-entries) entries)
                (conj pages (select-keys page [:count :scanned :incomplete :next-cursor]))
                (conj seen-cursors next-cursor))
         {:entries (if collect? (into entries page-entries) entries)
          :pages (conj pages (select-keys page [:count :scanned :incomplete :next-cursor]))})))))

(defn live-evidence-basis []
  (let [count-response (read-http-edn (str evidence-store "/api/alpha/evidence/count"))
        newest (fetch-evidence-page {:limit 1})]
    {:count (:count count-response)
     :max-at (get-in newest [:entries 0 :evidence/at])}))

(defn evidence-cache-path
  "The rule version is part of the name because the cached hits carry their
  :reflection flag. A corpus pin names the version it was built under, so a
  record written under an older rule keeps resolving to the index that
  classified it rather than silently missing and rebuilding from the live store."
  ([basis] (evidence-cache-path basis #{} nil reflection-rule-version))
  ([basis worker-seats] (evidence-cache-path basis worker-seats nil reflection-rule-version))
  ([basis worker-seats spider-agent]
   (evidence-cache-path basis worker-seats spider-agent reflection-rule-version))
  ([basis worker-seats spider-agent rule-version]
   (str (fs/path "/tmp"
                 (str "futon3-spider-evidence-occurrences-v3-"
                      (:count basis) "-"
                      (subs (sha256 (pr-str [basis (sort worker-seats) spider-agent
                                             rule-version])) 0 16)
                      ".edn")))))

(defn pattern-id [library file]
  (-> (str (fs/relativize library file))
      (str/replace #"\\" "/")
      (str/replace #"\.flexiarg$" "")))

(defn parse-pattern [library file]
  (let [text (slurp (str file))
        lines (str/split-lines text)
        from (pattern-id library file)
        body-start (or (first (keep-indexed
                               (fn [i line]
                                 (when (and (not (str/blank? line))
                                            (not (str/starts-with? line "@"))
                                            (not (str/starts-with? (str/triml line) ";;")))
                                   i))
                               lines))
                       (count lines))
        body (str/join "\n" (drop body-start lines))
        edges (mapcat
               (fn [[i line]]
                 (let [code (first (str/split line #";;" 2))]
                   (if-let [[_ kind tail] (re-matches #"\s*@(why-posthoc|why|how|see-also)\s+(.+?)\s*" code)]
                     (for [token (str/split tail #"\s+")
                           :when (re-matches target-pattern token)]
                       {:from from :to token :kind (keyword kind)
                        :file (str (fs/relativize library file)) :line (inc i)})
                     [])))
               (map-indexed vector lines))]
    {:id from :file (str (fs/relativize library file))
     :body-line (when (< body-start (count lines)) (inc body-start))
     :body-digest (sha256 body) :edges (vec edges)}))

(defn scan-library [library]
  (let [files (sort (fs/glob library "**.flexiarg"))
        patterns (mapv #(parse-pattern library %) files)]
    {:patterns patterns
     :ids (set (map :id patterns))
     :edges (vec (mapcat :edges patterns))
     :body-digests (into (sorted-map) (map (juxt :file :body-digest) patterns))}))

(defn snapshot [scan]
  {:baseline/schema 1
   :edges (mapv #(select-keys % [:from :to :kind :file :line]) (:edges scan))
   :body-digests (:body-digests scan)})

(defn edge-key [edge] (select-keys edge [:from :to :kind]))

(defn cycle-failures
  "Acyclicity is a law about AUTHORITY: a pattern cannot rest on something that
  rests on it. @why-posthoc asserts no authority -- it says a later reading makes
  sense of the pattern -- so a post-hoc edge is not admitted here, and a
  post-hoc edge closing a loop with an authored one is not a cycle failure. The
  other checks make no such distinction: dangling targets and the
  new-edge-attested rule apply to post-hoc edges exactly as to authored ones,
  because resolving a target and earning an attestation are not authority claims."
  [edges]
  (let [adj (reduce (fn [m {:keys [from kind] :as edge}]
                      (if (= kind :why) (update m from (fnil conj []) edge) m)) {} edges)
        colour (atom {})
        failures (atom [])]
    (letfn [(visit [node path via-edge]
              (case (get @colour node)
                :black nil
                :grey (swap! failures conj
                             {:check :why-acyclic
                              :file (:file via-edge) :line (:line via-edge)
                              :edge (edge-key via-edge)
                              :reason :why-cycle :cycle (conj (vec (drop-while #(not= % node) path)) node)})
                (do (swap! colour assoc node :grey)
                    (doseq [edge (get adj node)]
                      (visit (:to edge) (conj path node) edge))
                    (swap! colour assoc node :black))))]
      (doseq [node (keys adj)] (visit node [] nil))
      (vec (distinct @failures)))))

(defn nonblank-string? [x] (and (string? x) (not (str/blank? x))))

(defn valid-evidence? [x]
  (and (map? x)
       (every? #(contains? x %) [:id :via :query :excerpt])
       (some? (:id x)) (#{:tag :text} (:via x))
       (nonblank-string? (:query x)) (nonblank-string? (:excerpt x))))

(defn valid-state? [x]
  (or (#{:proposed :refused} x)
      (and (vector? x) (= 2 (count x)) (= :attested-by (first x))
           (nonblank-string? (second x)))))

;; Two refusals, two acts. `:state :refused` refuses the EDGE: the reviewer says
;; the relation does not hold, and the directive line leaves the pattern file
;; (:refused-edge-removed below fails if it is still there). `:warrant-refused`
;; refuses the WARRANT: the evidence offered does not establish the edge, so the
;; record keeps :state :proposed and the directive line stays, unwarranted,
;; until some other reading carries it. Before 2026-09-01 both acts were written
;; as `:refused` and the difference lived only in prose.
(defn valid-warrant-refusal? [x]
  (and (map? x)
       (every? #(contains? x %) [:by :at :note])
       (every? nonblank-string? ((juxt :by :at :note) x))))

(defn valid-attestation? [x]
  (and (map? x)
       (every? #(contains? x %) [:edge :by :at :read :cited :evidence :rung :state])
       ;; A non-keyword key is EDN corruption, not an extension: an unescaped
       ;; quote inside :reason splits the string and leaves the tail as a
       ;; symbol key (library/writing-coherence/attestations.edn carried one).
       (every? keyword? (keys x))
       (let [edge (:edge x)]
         (and (map? edge) (every? #(contains? edge %) [:from :to :kind])
              (nonblank-string? (:from edge)) (nonblank-string? (:to edge))
              (counted-edge-kinds (:kind edge))))
       (nonblank-string? (:by x)) (nonblank-string? (:at x))
       (vector? (:read x)) (seq (:read x)) (every? nonblank-string? (:read x))
       (nonblank-string? (:cited x))
       (vector? (:evidence x)) (seq (:evidence x)) (every? valid-evidence? (:evidence x))
       (#{1 2} (:rung x)) (valid-state? (:state x))
       (or (not (contains? x :reason)) (nonblank-string? (:reason x)))
       (or (not= :refused (:state x)) (nonblank-string? (:reason x)))
       (or (not (contains? x :warrant-refused))
           (valid-warrant-refusal? (:warrant-refused x)))))

(defn refusal-act-failures
  "The two acts are exclusive on one record. Refusing the warrant leaves the
  edge :proposed; refusing the edge is :state :refused. A record carrying
  :warrant-refused with any other state claims both happened at once."
  [i att]
  (when (and (map? att) (contains? att :warrant-refused)
             (not= :proposed (:state att)))
    [{:check :refusal-acts-distinct :line (inc i) :edge (:edge att)
      :reason :warrant-refusal-requires-proposed-state :detail (:state att)}]))

(defn string-leaves [x]
  (cond (map? x) (mapcat string-leaves (concat (keys x) (vals x)))
        (sequential? x) (mapcat string-leaves x)
        (string? x) [x]
        (keyword? x) [(subs (str x) 1)]
        :else []))

(defn exact-occurrence? [text token]
  (boolean (re-find (re-pattern (str "(?<![A-Za-z0-9_./'-])"
                                     (java.util.regex.Pattern/quote token)
                                     "(?![A-Za-z0-9_./'-])")) text)))

(defn occurrence-excerpt [text token]
  (let [at (.indexOf text token)
        start (max 0 (- at 200))
        end (min (count text) (+ at (count token) 200))]
    (subs text start end)))

(defn context-retrieval-listing? [record]
  (let [body (:evidence/body record)]
    (if (map? body)
      (= "context-retrieval" (some-> (or (:event body) (get body "event")) name))
      (boolean
       (and (string? body)
            (re-find #"(?i)(?:\"event\"|:event)\s*(?:[:=]\s*)?(?:\"context-retrieval\"|:context-retrieval)"
                     body))))))

(defn spider-self-text? [record]
  (let [text (str/join " " (string-leaves (:evidence/body record)))]
    (boolean (or (str/includes? text "You are the spider for library/")
                 (str/includes? text "Runner-supplied rung-1 exact-occurrence hits")
                 (str/includes? text "exact live occurrence:")
                 (str/includes? text "spider-self-text?")))))

(defn record-event [record]
  (let [body (:evidence/body record)]
    (cond
      (map? body) (some-> (or (:event body) (get body "event")) name)
      (and (string? body)
           (re-find #"(?i)(?:\"event\"|:event)\s*(?:[:=]\s*)?(?:\"invoke-complete\"|:invoke-complete)"
                    body)) "invoke-complete"
      :else nil)))

(defn agency-job-envelope? [record]
  (let [text (str/join " " (string-leaves (:evidence/body record)))]
    (or (= "invoke-complete" (record-event record))
        (and (re-find #"(?i)\b(?:job-id|job_id|invoke-job|invoke_id)\b" text)
             (re-find #"(?i)\b(?:agent-id|agent_id|seat|target|to)\b" text)))))

(defn worker-seat-mentioned? [record worker-seats]
  (let [text (str/join " " (string-leaves (:evidence/body record)))]
    (boolean (some #(exact-occurrence? text %) worker-seats))))

(defn regex-spans [re text]
  (let [matcher (re-matcher re text)]
    (loop [spans []]
      (if (.find matcher)
        (recur (conj spans [(.start matcher) (.end matcher)]))
        spans))))

(defn record-author [record]
  (let [raw (:evidence/author record)]
    (if (keyword? raw) (name raw) (str raw))))

(defn authoring-verb-attributed-to-author?
  "Whether this authoring verb states an act by the record's own author. Three
  clause shapes say it does: no subject at all; an impersonal clause, where a
  preposition follows the verb and no agent is named (`Pattern authored at
  <path>`); and a first-person subject or one naming the record's author. An
  active clause whose subject names someone else -- `codex-15 wrote five @how
  patterns`, `invoke-1787649777853-1018-f0693af4: Created the five uncommitted
  files` in a record joe wrote -- reports another agent's authoring, which is
  the external use the rule exists to keep, not reflection.

  A subject this cannot read (a lowercase common noun, an ellipsis, a bullet)
  falls through to true: the verb keeps counting, so an unreadable clause
  refuses a warrant rather than admitting one."
  [text author [verb-start verb-end]]
  (let [subject (second (re-find authoring-subject-tail (subs text 0 verb-start)))
        after (subs text verb-end (min (count text) (+ verb-end authoring-complement-window)))]
    (cond
      (nil? subject) true
      (re-matches first-person-subject subject) true
      (.equalsIgnoreCase ^String subject (str author)) true
      (re-find authoring-complement after) true
      (re-matches named-agent-subject subject) false
      :else true)))

(defn authoring-turn?
  "Reflection rule v3. True when the record announces the record author's OWN
  authoring of a library pattern FILE -- an authoring verb they are the agent
  of, within `authoring-verb-window` characters of a *.flexiarg path. Such a
  turn is the pattern's own author describing the references they have just
  written into it, so a relation it states between that pattern and another
  comes from the authoring act and not from anyone using the two together. A
  verb some other agent is the subject of does not count, or an operator's
  report of what a seat wrote would be discarded as the fleet's own record.
  `library/<target>.flexiarg` in the spider's own prompt is not a path match:
  the placeholder brackets break it."
  [record]
  (let [text (str/join " " (string-leaves (:evidence/body record)))
        author (record-author record)
        paths (regex-spans pattern-file-path text)
        verbs (filter #(authoring-verb-attributed-to-author? text author %)
                      (regex-spans authoring-verb text))]
    (boolean
     (some (fn [[path-start path-end]]
             (some (fn [[verb-start verb-end]]
                     (or (and (<= verb-end path-start)
                              (<= (- path-start verb-end) authoring-verb-window))
                         (and (<= path-end verb-start)
                              (<= (- verb-start path-end) authoring-verb-window))))
                   verbs))
           paths))))

(defn reflection-record? [record worker-seats spider-agent]
  (let [author (record-author record)]
    (boolean
     (or (contains? worker-seats author)
         (and (agency-job-envelope? record)
              (or (worker-seat-mentioned? record worker-seats)
                  (and spider-agent
                       (worker-seat-mentioned? record #{spider-agent}))))
         (spider-self-text? record)
         (authoring-turn? record)))))

(defn reflection-rule [worker-seats spider-agent]
  {:version reflection-rule-version
   :worker-seats (vec (sort worker-seats))
   :worker-author :evidence-author-in-worker-seats
   :spider-agent spider-agent
   :agency-job :invoke-complete-or-job-envelope-naming-worker-or-spider-agent
   :self-text :spider-self-text-prompt-markers
   :authoring-turn :author-attributed-authoring-verb-within-window-of-a-flexiarg-path
   :authoring-attribution :impersonal-first-person-or-subject-is-the-record-author
   :authoring-verb-window authoring-verb-window})

(defn clean-hit? [hit]
  (and (false? (:listing hit))
       (false? (:self-text hit))
       (false? (:co-mention hit))))

(defn clean-non-reflection-hit? [hit]
  (and (clean-hit? hit) (false? (:reflection hit))))

(defn add-record-occurrences [index ids wr-aliases worker-seats spider-agent record]
  (let [text (str/join " " (string-leaves record))
        record-id (or (:evidence/id record) (:xt/id record))
        listing (context-retrieval-listing? record)
        self-text (spider-self-text? record)
        reflection (reflection-record? record worker-seats spider-agent)
        path-hits (filter ids (re-seq target-pattern text))
        alias-tokens (re-seq #"WR-[0-9]+" text)
        alias-hits (keep wr-aliases alias-tokens)
        patterns (distinct (concat path-hits alias-hits))
        co-mention (> (count patterns) 1)]
    (reduce (fn [m pattern]
              (let [token (if (exact-occurrence? text pattern)
                            pattern
                            (first (filter #(= pattern (get wr-aliases %)) alias-tokens)))]
                (if (and record-id token)
                  (update m pattern (fnil conj [])
                          {:id record-id :via :tag :listing listing
                           :self-text self-text :co-mention co-mention
                           :reflection reflection
                           :query (str "exact live occurrence: " token)
                           :excerpt (occurrence-excerpt text token)})
                  m))) index patterns)))

(defn build-evidence-index
  ([ids] (build-evidence-index ids {}))
  ([ids {:keys [fetch-page since page-observer worker-seats spider-agent]
         :or {fetch-page fetch-evidence-page page-observer (fn [_ _] nil)}}]
   (let [worker-seats (set worker-seats)
         wr-aliases (into {} (keep (fn [id]
                                     (when-let [[_ n] (re-matches #"war-room/wr-([0-9]+)-.*" id)]
                                       [(str "WR-" n) id]))) ids)
         index (atom {})]
     (page-evidence fetch-page
                    {:since since
                     :collect? false
                     :on-page (fn [n page]
                                (doseq [record (:entries page)]
                                  (swap! index add-record-occurrences ids wr-aliases
                                         worker-seats spider-agent record))
                                (page-observer n page))})
     @index)))

(defn read-index-cache [path]
  (when (fs/exists? path) (edn/read-string (slurp (str path)))))

(defn write-index-cache! [path value]
  (let [tmp (str path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint value)))
    (fs/move tmp path {:replace-existing true :atomic-move true})))

(defn ensure-live-evidence-index!
  "Return a live index named by the count/max-at basis observed immediately
  before its keyset scan. The store does not expose a transaction-snapshot
  token on this route, so the start basis is recorded rather than inventing
  quiescence while live writers continue."
  ([library] (ensure-live-evidence-index! library #{} nil))
  ([library worker-seats] (ensure-live-evidence-index! library worker-seats nil))
  ([library worker-seats spider-agent]
  (let [ids (set (map #(pattern-id library %) (fs/glob library "**.flexiarg")))
        worker-seats (set worker-seats)
        basis (live-evidence-basis)
        path (evidence-cache-path basis worker-seats spider-agent)]
    (or (read-index-cache path)
        (let [index (build-evidence-index
                     ids {:worker-seats worker-seats :spider-agent spider-agent
                          :page-observer
                          (fn [n _]
                            (when (zero? (mod (inc n) 25))
                              (binding [*out* *err*]
                                (println "live evidence index pages:" (inc n)))) )})
              cache {:schema 3 :store evidence-store :basis basis
                     :cache-path path
                     :reflection-rule (reflection-rule worker-seats spider-agent)
                     :built-at (now) :projection :full-record-with-body
                     :index index}]
          (write-index-cache! path cache)
          cache)))))

(defn normalized [x]
  (-> (str x) str/lower-case (str/replace #"\s+" " ") str/trim))

(defn evidence-record [records id]
  (if records
    (get records id)
    (try (edn/read-string (slurp (str "http://127.0.0.1:7073/api/alpha/evidence/" id)))
         (catch Exception _ nil))))

(defn rung-one-record [index pattern id]
  (when (some #(= id (:id %)) (get index pattern))
    (evidence-record nil id)))

(defn attestation-semantic-failures [records live-index i att]
  (when (valid-attestation? att)
    (let [evidence (:evidence att)
          rung (:rung att)
          vias (set (map :via evidence))
          base {:check :attestation-semantics :line (inc i) :edge (:edge att)}]
      (concat
       (when (and (= rung 1) (not (contains? vias :tag)))
         [(assoc base :reason :rung-via-mismatch :detail :rung-1-requires-tag)])
       (when (and (= rung 2) (contains? vias :tag))
         [(assoc base :reason :rung-via-mismatch :detail :rung-2-forbids-tag)])
       (for [{:keys [id excerpt via]} evidence
             :let [record (if (= via :tag)
                            (if records (evidence-record records id)
                                (rung-one-record live-index (get-in att [:edge :from]) id))
                            (evidence-record records id))
                   haystack (normalized (str/join " " (string-leaves record)))]
             ;; Refused rows are retained negative audit records, not evidence
             ;; that can authorize a directive. Accepted/proposed rows remain
             ;; fail-closed under live excerpt verification.
             :when (and (not= :refused (:state att))
                        (or (nil? record)
                            (not (str/includes? haystack (normalized excerpt)))))]
         (assoc base :reason :evidence-excerpt-mismatch :evidence-id id))))))

(defn read-edn-or [path fallback]
  (if (and path (fs/exists? path)) (edn/read-string (slurp path)) fallback))

(defn section-summary [scan section]
  (let [prefix (str section "/")
        patterns (filter #(str/starts-with? (:id %) prefix) (:patterns scan))
        edges (filter #(str/starts-with? (:from %) prefix) (:edges scan))
        why-from (set (map :from (filter #(= :why (:kind %)) edges)))
        posthoc-from (set (map :from (filter #(= posthoc-why-kind (:kind %)) edges)))
        n (count patterns)]
    {:section section :patterns n
     :edges-by-kind (merge (zipmap counted-edge-kinds (repeat 0))
                           (frequencies (map :kind edges)))
     :patterns-with-outgoing-why (count why-from)
     ;; Reported beside the authored count, never folded into it.
     ;; :fraction-organised stays the fraction with an AUTHORED @why: §5a says a
     ;; post-hoc edge should earn attestation before it counts, so a section
     ;; cannot raise its organised fraction by adding traces.
     :patterns-with-outgoing-why-posthoc (count posthoc-from)
     :fraction-organised (if (zero? n) 0.0 (/ (double (count why-from)) n))}))

(defn lint [{:keys [library section baseline attestations evidence-records]}]
  (let [scan (scan-library library)
        base (read-edn-or baseline {:edges [] :body-digests {}})
        atts (read-edn-or attestations [])
        att-rows (if (vector? atts) atts [])
        records (when evidence-records (read-edn-or evidence-records {}))
        corpus (some identity (keep :corpus (reverse att-rows)))
        corpus-basis (:basis corpus)
        corpus-worker-seats (set (get-in corpus [:reflection-rule :worker-seats]))
        corpus-spider-agent (get-in corpus [:reflection-rule :spider-agent])
        pinned-cache-path (or (System/getenv "SPIDER_EVIDENCE_CACHE")
                              (when corpus-basis
                                (evidence-cache-path corpus-basis corpus-worker-seats
                                                     corpus-spider-agent
                                                     (or (get-in corpus [:reflection-rule :version])
                                                         reflection-rule-version))))
        live-cache (when (and (nil? records)
                              (some #(some (fn [e] (= :tag (:via e))) (:evidence %)) att-rows))
                     (or (when pinned-cache-path (read-index-cache pinned-cache-path))
                         (ensure-live-evidence-index! library)))
        live-index (:index live-cache)
        baseline-edges (set (map edge-key (:edges base)))
        attested-edges (set (keep #(when (valid-attestation? %) (edge-key (:edge %))) att-rows))
        refused-edges (set (keep #(when (and (valid-attestation? %)
                                             (= :refused (:state %)))
                                    (edge-key (:edge %))) att-rows))
        prefix (str section "/")
        section-edges (filter #(str/starts-with? (:from %) prefix) (:edges scan))
        dangling (for [edge (:edges scan) :when (not (contains? (:ids scan) (:to edge)))]
                   (assoc (select-keys edge [:file :line :from :to :kind])
                          :check :targets-resolve :edge (edge-key edge) :reason :dangling-target))
        cycles (cycle-failures (:edges scan))
        missing-atts (for [edge section-edges
                           :let [k (edge-key edge)]
                           :when (and (not (contains? baseline-edges k))
                                      (not (contains? attested-edges k)))]
                       (assoc (select-keys edge [:file :line]) :check :new-edge-attested
                              :edge k :reason :new-edge-without-attestation))
        refused-present (for [edge section-edges
                              :let [k (edge-key edge)]
                              :when (contains? refused-edges k)]
                          (assoc (select-keys edge [:file :line]) :check :refused-edge-removed
                                 :edge k :reason :refused-edge-still-present))
        malformed (cond
                    (not (vector? atts))
                    [{:check :attestation-schema :file (str attestations)
                      :reason :malformed-attestation :detail :root-must-be-vector}]
                    :else
                    (keep-indexed (fn [i att]
                                    (when-not (valid-attestation? att)
                                      {:check :attestation-schema :file (str attestations)
                                       :line (inc i) :edge (:edge att)
                                       :reason :malformed-attestation}))
                                  att-rows))
        semantic (mapcat #(attestation-semantic-failures records live-index %1 %2)
                         (range) att-rows)
        refusal-acts (mapcat refusal-act-failures (range) att-rows)
        body-failures (for [{:keys [file body-line body-digest]} (:patterns scan)
                            :when (str/starts-with? file prefix)
                            :let [old (get (:body-digests base) file)]
                            :when (not= old body-digest)]
                        {:check :argument-bodies-unchanged :file file :line body-line :edge nil
                         :reason (if old :argument-body-changed :argument-body-not-in-baseline)
                         :expected old :actual body-digest})
        failures (mapv #(merge {:file nil :line nil :edge nil} %)
                       (concat dangling cycles missing-atts refused-present malformed
                               refusal-acts semantic body-failures))
        kind-counts (merge (zipmap counted-edge-kinds (repeat 0))
                           (frequencies (map :kind (:edges scan))))
        why-nodes (set (mapcat (juxt :from :to) (filter #(= :why (:kind %)) (:edges scan))))]
    {:checks failures
     :summary (merge {:pass? (empty? failures)
                      :files (count (:patterns scan))
                      :edges-by-kind kind-counts
                      :patterns-in-why-graph (count why-nodes)
                      :unresolved-targets (count dangling)
                      :edge-refusals (count (filter #(and (valid-attestation? %)
                                                          (= :refused (:state %))) att-rows))
                      :warrant-refusals (count (filter #(and (valid-attestation? %)
                                                             (contains? % :warrant-refused))
                                                       att-rows))
                      :why-cycles (count cycles)
                      :failures (count failures)
                      :edge-counting :path-shaped-targets
                      :baseline-note (:measurement-note base)}
                     {:section (section-summary scan section)})}))

(defn parse-args [args]
  (when (odd? (count args)) (throw (ex-info "arguments must be --key value pairs" {})))
  (into {} (map (fn [[k v]] [(keyword (str/replace k #"^--" "")) v]) (partition 2 args))))

(defn -main [& args]
  (let [opts (parse-args args)
        report-path (:report opts)]
    (when-not (every? opts [:library :section :baseline :attestations :report])
      (binding [*out* *err*]
        (println "usage: library_graph_lint.clj --library DIR --section NAME --baseline FILE --attestations FILE --report FILE"))
      (System/exit 2))
    (let [report (try (lint opts)
                      (catch Exception e
                        {:checks [{:check :linter :file nil :line nil :edge nil
                                   :reason :linter-error :message (.getMessage e)}]
                         :summary {:pass? false :failures 1}}))]
      (fs/create-dirs (fs/parent report-path))
      (spit report-path (str (pr-str report) "\n"))
      (println (pr-str (:summary report)))
      (System/exit (if (get-in report [:summary :pass?]) 0 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
