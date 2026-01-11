;;; aob-chatgpt-test.el --- ERT coverage for Tatami context -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the futon3 repo for resolving relative paths in tests.")

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(unless (boundp 'my-tatami--clojure)
  (defvar my-tatami--clojure "clj"))

(load (expand-file-name "contrib/aob-chatgpt.el" futon3--test-root))
(load (expand-file-name "contrib/fubar-hud.el" futon3--test-root))

(defconst futon3-test-hud-fixture-file
  (expand-file-name "resources/hud-fixtures.edn" futon3--test-root)
  "Recorded Tatami HUD fixture samples for regression tests.")

(defun futon3-test--read-hud-fixtures ()
  "Return every HUD fixture stored in `futon3-test-hud-fixture-file'."
  (when (file-readable-p futon3-test-hud-fixture-file)
    (with-temp-buffer
      (insert-file-contents futon3-test-hud-fixture-file)
      (goto-char (point-min))
      (let (forms)
        (condition-case _
            (while t
              (push (read (current-buffer)) forms))
          (end-of-file))
        (nreverse forms)))))

(defun futon3-test--support-snippet (edn)
  "Return the rendered cues snippet for EDN or nil when no cues exist."
  (when (my-chatgpt-shell--support-present-p edn)
    (with-temp-buffer
      (my-chatgpt-shell--insert-inbound-summary edn)
      (string-trim (buffer-string)))))

(defmacro futon3--with-chat-buffer (&rest body)
  "Evaluate BODY inside a temporary chatgpt-shell buffer with safe hooks."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (chatgpt-shell-mode)
     (setq my-chatgpt-shell-profile "General")
     (let ((my-chatgpt-shell-context-buffer-name "*Tatami Context Test*")
           ;; The real Tatami context log can be huge, so tests disable it
           ;; by default and rebind explicitly when persistence is required.
           (my-chatgpt-shell-edn-log-file nil))
       (cl-letf (((symbol-function 'my-chatgpt-shell--maybe-render-context)
                  (lambda (&rest _) (my-chatgpt-shell--render-context t)))
                 ((symbol-function 'my-futon3-running-p) (lambda () nil))
                 ;; Avoid recursive attempts to hit the live Futon3 stack when
                 ;; tests tweak intents or prototypes.
                 ((symbol-function 'my-futon3-sync-selection)
                  (lambda (&rest _) nil))
                 ((symbol-function 'my-futon3-refresh-status) (lambda () nil)))
         ,@body))))

(ert-deftest aob-chatgpt-context-shows-inbound-patterns ()
  "Tatami Context should show candidate clauses even when FROM-CHATGPT omits them."
  (let ((my-futon3-tatami-default-prototypes '("f3/p4")))
    (futon3--with-chat-buffer
      (let ((my-chatgpt-shell-edn-log-file nil))
        (let* ((pattern '(:id "f2/p7"
                         :title "Evening hacking"
                         :summary "Short proofwork warm-up"
                         :score 0.73))
               (inbound `(:session-id "S-42"
                          :clock "2024-05-10T20:15:00Z"
                          :mode :ambient
                          :intent "evening hacking"
                          :patterns [,pattern]
                          :fruits [(:fruit/id "viriya" :emoji "🍊" :score 0.61)]
                          :paramitas []
                          :events [])))
          (setq my-chatgpt-shell-last-inbound-edn inbound)
          (let ((edn-block (my-chatgpt-shell--format-edn-block
                            my-chatgpt-shell-tatami-out-marker
                            '(:session-id "S-42"
                              :mode :ambient
                              :clock "2024-05-10T20:15:00Z"
                              :intent "evening hacking"
                              :events [(:kind :note :pattern "f2/p7"
                                         :notes "Pattern applies to evening hacking.")]))))
            (insert "Model reply\n\n" edn-block)
            (my-chatgpt-shell--process-buffer-edn (current-buffer))
            (should (equal (plist-get my-chatgpt-shell-last-pattern-outcome :status)
                           :match))
          (with-current-buffer (my-chatgpt-shell--context-buffer t)
          (let ((contents (buffer-string))
                (required '("Pattern reasoning & Tatami hints"
                            "Nearest pattern"
                            "Candidate clauses"
                            "f2/p7"
                            "Pattern f2/p7"
                            "FUTON3 — Prototype 4")))
            (dolist (needle required)
              (should (string-match-p (regexp-quote needle) contents)))))))))))

(ert-deftest futon3-prototype-details-respect-devmap-cache ()
  "Prototype detail lines should surface devmap titles when available."
  (let ((table (make-hash-table :test 'equal)))
    (puthash "f3/p4" "FUTON3 — Prototype 4 — Trail journal" table)
    (cl-letf (((symbol-function 'my-futon3--prototype-display-map)
               (lambda () table)))
      (let ((lines (my-futon3--prototype-detail-lines '("f3/p4"))))
        (should (= (length lines) 1))
        (should (string-match-p "FUTON3 — Prototype 4" (car lines)))))))

(ert-deftest futon3-prototype-details-from-real-devmap ()
  "Prototype detail lines should resolve using the repo devmaps."
  (let ((my-futon3-devmap-directory (expand-file-name "holes" futon3--test-root)))
    (let ((lines (my-futon3--prototype-detail-lines '("f3/p4"))))
      (should (>= (length lines) 1))
      (should (string-match-p "Prototype 4" (car lines))))))

(ert-deftest aob-chatgpt-loads-futon3-modules ()
  "aob-chatgpt should expose functions moved into futon3 contrib modules."
  (dolist (fn '(my-futon3--embed-sigils-from-text
                my-futon3--prototype-detail-lines
                my-futon3--format-counts
                my-futon3--log-pattern-edit
                my-futon3--tatami-request
                my-futon3-fetch-hints))
    (should (fboundp fn))))

(ert-deftest tatami-hints-drop-null-patterns ()
  "Tatami summaries should ignore null placeholders while showing real patterns."
  (with-temp-buffer
    (let ((edn (list :patterns (vector '(:id :null :summary :null :score 0.5)
                                      '(:id "t4r/dtp-fit" :summary "Warm-up" :score 0.1))
                     :events []
                     :fruits (vector '(:fruit/id :null :emoji :null)
                                     '(:fruit/id "viriya" :emoji "🍊"))
                     :paramitas (vector '(:paramita/id :null)
                                        '(:paramita/id "śīla" :emoji "🕊")))))
      (my-chatgpt-shell--insert-edn-summary edn)
      (let ((contents (buffer-string)))
        (should-not (string-match-p ":null" contents))
        (should (string-match-p "t4r/dtp-fit" contents))))))

(ert-deftest fake-hud-from-text-updates-state ()
  "Simulated HUD runs should parse sample transcript blocks."
  (let ((sample "[Tuesday]\n---FROM-CHATGPT-EDN---\n{:intent \"demo\" :events [{:kind :note :pattern \"smoke-test\" :notes \"HUD round-trip OK?\"}]}\n---END-FROM-CHATGPT-EDN---\n---FROM-TATAMI-EDN---\n{:session-id \"S-fake\" :patterns [{:id \"t4r/dtp-fit\" :summary \"Warm-up\" :score 0.2}]}\n---END-FROM-TATAMI-EDN---"))
    (futon3--with-chat-buffer
      (let ((my-chatgpt-shell-edn-log-file nil))
        (my-chatgpt-shell-fake-hud-from-text sample)
        (should (equal (plist-get my-chatgpt-shell-last-pattern-outcome :status) :match))
        (should (string= (plist-get my-chatgpt-shell-last-edn :intent) "demo"))
        (with-current-buffer (my-chatgpt-shell--context-buffer t)
          (should (string-match-p "smoke-test" (buffer-string))))))))

(ert-deftest futon3-intent-sigils-refreshes-with-pattern-overlap ()
  "Intent sigils should recompute when Tatami patterns overlap with the current intent."
  (cl-letf (((symbol-function 'my-futon3--embed-sigils-from-text) (lambda (&rest _) nil)))
    (futon3--with-chat-buffer
      (let* ((sigil '(:emoji "🎯" :hanzi "矢"))
             (pattern (list :id "f3/p2"
                            :title "CORPS transitions"
                            :summary "Assess sufficiency and rightness of chat patterns via CORPS Five Arrows"
                            :sigils (vector sigil)))
             (intent "Assess sufficiency and rightness of chat patterns"))
        (setq my-chatgpt-shell-last-edn nil
              my-chatgpt-shell-last-inbound-edn
              (list :intent intent
                    :tatami (list :patterns (vector pattern))
                    :prototypes (vector "f3/p2")))
        (let ((resolved (my-chatgpt-shell--resolve-intent-sigils)))
          (should (equal resolved (list sigil)))
          (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils)
                         (list sigil)))
          (should (eq (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-source)
                      :pattern))
          (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-intent)
                         intent)))))))

(ert-deftest futon3-intent-sigils-consider-chatgpt-patterns ()
  "Embedding should reflect patterns reported in FROM-CHATGPT-EDN."
  (cl-letf (((symbol-function 'my-futon3--embed-sigils-from-text) (lambda (&rest _) nil)))
    (futon3--with-chat-buffer
      (let* ((sigil '(:emoji "🧭" :hanzi "道"))
             (pattern (list :id "f4/p1"
                            :title "Onward processing diagnostics"
                            :summary "Onward processing deficiency observed; apply diagnostics to locate distorted Role × Value invariants"
                            :sigils (vector sigil)))
             (intent "Test onward processing of embedded intent v2"))
        (setq my-chatgpt-shell-last-inbound-edn nil
              my-chatgpt-shell-last-edn
              (list :intent intent
                    :patterns (vector pattern)
                    :prototypes (vector "f4/p1")))
        (let ((resolved (my-chatgpt-shell--resolve-intent-sigils)))
          (should (equal resolved (list sigil)))
          (should (equal (plist-get my-chatgpt-shell-last-edn :intent-sigils)
                         (list sigil)))
          (should (eq (plist-get my-chatgpt-shell-last-edn :intent-sigils-source)
                      :pattern))
          (should (equal (plist-get my-chatgpt-shell-last-edn :intent-sigils-intent)
                         intent)))))))

(ert-deftest futon3-intent-sigils-embed-uses-events ()
  "Embedding text should include the intent and any reasoning events."
  (futon3--with-chat-buffer
    (let ((captured nil)
          (sigil '(:emoji "🧮" :hanzi "算")))
      (cl-letf (((symbol-function 'my-futon3--embed-sigils-from-text)
                 (lambda (text &optional _limit)
                   (setq captured text)
                   (list sigil))))
        (let* ((events (vector '(:kind :note :pattern "stack-blocker" :notes "handshake metadata")))
               (edn (list :events events)))
          (let ((result (my-chatgpt-shell--sigils-from-intent edn "metadata test handshake and echo")))
            (should (equal result (list sigil)))
            (should (string-match-p "metadata test handshake and echo" captured))
            (should (string-match-p "handshake metadata" captured))
            (should (eq my-chatgpt-shell--last-intent-sigil-origin :embedded))))))))

(ert-deftest futon3-hud-resume-session-quotes-and-continues ()
  "Resume should quote session IDs and fall back to --continue on blanks."
  (let ((fubar-hud-futon3-root futon3--test-root)
        (fubar-hud-buffer-name "*FuLab HUD Test*")
        (fubar-hud-stream-buffer-name "*FuLab Raw Stream Test*")
        (commands nil))
    (with-temp-buffer
      (rename-buffer fubar-hud-buffer-name)
      (setq-local fubar-hud--session-id "sid with space")
      (cl-letf (((symbol-function 'start-process-shell-command)
                 (lambda (_name _buffer cmd)
                   (push cmd commands)
                   'fubar-hud-test-proc))
                ((symbol-function 'set-process-sentinel)
                 (lambda (&rest _) nil))
                ((symbol-function 'display-buffer)
                 (lambda (&rest _) nil)))
        (fubar-hud-resume-session "Hello" "sid with space")
        (let ((cmd (car commands)))
          (should (string-match-p "--resume" cmd))
          (should (string-match-p "--resume 'sid with space'" cmd)))
        (setq commands nil)
        (fubar-hud-resume-session "Hello" "")
        (let ((cmd (car commands)))
          (should (string-match-p "--continue" cmd))
          (should-not (string-match-p "--resume" cmd)))))))

(ert-deftest futon3-pattern-sigils-fallback-when-no-overlap ()
  "Patterns should still contribute sigils when no keywords overlap the intent."
  (let* ((sigil '(:emoji "🌙" :hanzi "休"))
         (pattern (list :id "t4r/rest"
                        :title "Ensure restful stop"
                        :summary "Honor rest cycles"
                        :sigils (vector sigil)))
         (edn (list :patterns (vector pattern))))
    (should (equal (my-chatgpt-shell--pattern-derived-sigils edn "unrelated text")
                   (list sigil)))))

(ert-deftest futon3-intent-sigils-refresh-when-intent-mismatches ()
  "Sigils should recompute when the cached intent snapshot differs."
  (futon3--with-chat-buffer
    (let ((new-sigil '(:emoji "🔁" :hanzi "新")))
      (cl-letf (((symbol-function 'my-chatgpt-shell--sigils-from-intent)
                 (lambda (&rest _)
                   (setq my-chatgpt-shell--last-intent-sigil-origin :embedded)
                   (list new-sigil))))
        (setq my-chatgpt-shell-last-inbound-edn
              (list :intent "align trails"
                    :intent-sigils (list (list :emoji "🥚" :hanzi "旧"))
                    :intent-sigils-source :embedded
                    :intent-sigils-intent "old intent"
                    :patterns []
                    :prototypes (vector "f3/p2")))
        (let ((resolved (my-chatgpt-shell--resolve-intent-sigils)))
          (should (equal resolved (list new-sigil)))
          (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-intent)
                         "align trails"))
          (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils)
                         (list new-sigil)))
          (should (eq (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-source)
                      :embedded)))))))

(ert-deftest futon3-refresh-context-hints-rerenders ()
  "Refreshing hints should update cached sigils and redraw the HUD."
  (futon3--with-chat-buffer
    (let ((render-count 0))
      (cl-letf (((symbol-function 'my-futon3-fetch-hints)
                 (lambda (&rest _)
                   '(:patterns [] :fruits [] :paramitas [])))
                ((symbol-function 'my-chatgpt-shell--enrich-with-cues)
                 (lambda (edn) edn))
                ((symbol-function 'my-chatgpt-shell--sigils-from-intent)
                 (lambda (&rest _)
                   (setq my-chatgpt-shell--last-intent-sigil-origin :embedded)
                   '((:emoji "⭐" :hanzi "新"))))
                ((symbol-function 'my-chatgpt-shell--render-context)
                 (lambda (&optional _ensure)
                   (setq render-count (1+ render-count)))))
        (setq my-chatgpt-shell-last-edn '(:intent "refresh me"
                                         :events []))
        (should (my-chatgpt-shell--refresh-context-hints "refresh me"))
        (should (= render-count 1))
        (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils)
                       '((:emoji "⭐" :hanzi "新"))))
        (should (eq (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-source)
                    :embedded))
        (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-intent)
                       "refresh me"))))))

(defun futon3-test--refresh-hints-derives-after-patterns ()
  (futon3--with-chat-buffer
    (let ((render-count 0)
          (sigil-calls 0))
      (cl-letf (((symbol-function 'my-futon3-fetch-hints)
                 (lambda (&rest _)
                   '(:patterns [(:id "t4r/rest" :title "Rest cycle" :summary "Honor rest" :sigils [(:emoji "🌙" :hanzi "休")])]
                     :fruits [] :paramitas [])))
                ((symbol-function 'my-chatgpt-shell--enrich-with-cues)
                 (lambda (edn) edn))
                ((symbol-function 'my-chatgpt-shell--sigils-from-intent)
                 (lambda (edn intent)
                   (setq sigil-calls (1+ sigil-calls))
                   (when (plist-get edn :patterns)
                     (setq my-chatgpt-shell--last-intent-sigil-origin :pattern)
                     '((:emoji "🌙" :hanzi "休")))))
                ((symbol-function 'my-chatgpt-shell--render-context)
                 (lambda (&optional _ensure)
                   (setq render-count (1+ render-count)))))
        (setq my-chatgpt-shell-last-edn '(:intent "rest check" :events []))
        (should (my-chatgpt-shell--refresh-context-hints "rest check"))
        (should (= sigil-calls 2))
        (should (= render-count 1))
        (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils)
                       '((:emoji "🌙" :hanzi "休"))))
        (should (eq (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils-source)
                    :pattern))))))

(ert-deftest futon3-refresh-hints-derives-after-patterns ()
  "Refresh should recompute sigils once Tatami patterns arrive."
  (futon3-test--refresh-hints-derives-after-patterns))

(ert-deftest futon3-prototype-refresh-hints-derives-after-patterns ()
  "Prototype 5 readiness: ensure Tatami patterns trigger sigil derivation."
  (futon3-test--refresh-hints-derives-after-patterns))


(ert-deftest futon3-refresh-clears-stale-sigils ()
  "Refreshing with a new intent should drop prior sigils before embedding."
  (futon3--with-chat-buffer
    (let ((captured nil))
      (setq my-chatgpt-shell-last-edn '(:intent "new intent" :prototypes (vector "f3/p3")))
      (setq my-chatgpt-shell-last-inbound-edn
            '(:intent "old intent"
              :prototypes (vector "f3/p3")
              :intent-sigils ((:emoji "👐" :hanzi "么"))
              :intent-sigils-source :embedded
              :intent-sigils-intent "old intent"))
      (cl-letf (((symbol-function 'my-futon3-fetch-hints)
                 (lambda (&rest _) '(:patterns [] :fruits [] :paramitas [])))
                ((symbol-function 'my-chatgpt-shell--enrich-with-cues)
                 (lambda (edn) edn))
                ((symbol-function 'my-chatgpt-shell--sigils-from-intent)
                 (lambda (&rest _)
                   (setq captured '((:emoji "📁" :hanzi "門")))
                   captured)))
        (my-chatgpt-shell--refresh-context-hints "new intent")
        (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent-sigils)
                       '((:emoji "📁" :hanzi "門"))))))))

(ert-deftest futon3-refresh-from-hud-buffer-updates-owner ()
  "HUD-triggered refreshes should mutate the chat buffer, not the HUD clone."
  (let ((chat (generate-new-buffer "*hud-refresh-chat*"))
        (hud-name "*Tatami Context HUD Test*")
        (hud nil)
        (my-chatgpt-shell--hud-state-registry (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (chatgpt-shell-mode)
            (setq my-chatgpt-shell-last-edn '(:intent "hud refresh" :events []))
            (setq my-chatgpt-shell--context-source (current-buffer)))
          (let ((my-chatgpt-shell-context-buffer-name hud-name))
            (setq hud (my-chatgpt-shell--context-buffer t))
            (with-current-buffer hud
              (cl-letf (((symbol-function 'my-futon3-fetch-hints)
                         (lambda (&rest _)
                           '(:patterns []
                             :fruits [(:fruit/id "dana")]
                             :paramitas [(:paramita/id "ksanti" :emoji "🟣" :score 0.42)])))
                        ((symbol-function 'my-chatgpt-shell--enrich-with-cues)
                         (lambda (edn) edn))
                        ((symbol-function 'my-chatgpt-shell--render-context)
                         (lambda (&optional _) nil)))
                (setq my-chatgpt-shell--context-source chat)
                (my-chatgpt-shell--refresh-context-hints "hud refresh"))))
          (with-current-buffer chat
            (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :fruits)
                           [(:fruit/id "dana")]))
            (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :paramitas)
                           [(:paramita/id "ksanti" :emoji "🟣" :score 0.42)])))
          (with-current-buffer hud
            (should (null my-chatgpt-shell-last-inbound-edn))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (when (get-buffer hud-name) (kill-buffer hud-name)))))

(ert-deftest futon3-hud-context-switches-between-chat-buffers ()
  "Tatami Context should reflect whichever chat buffer last recorded HUD state."
  (let ((chat-a (generate-new-buffer "*hud-chat-a*"))
        (chat-b (generate-new-buffer "*hud-chat-b*"))
        (hud-name "*Tatami Context HUD Test*")
        (my-chatgpt-shell--hud-state-registry (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (dolist (spec `((,chat-a . "viriya") (,chat-b . "dana")))
            (with-current-buffer (car spec)
              (chatgpt-shell-mode)
              (setq my-chatgpt-shell-last-inbound-edn
                    (list :session-id "S"
                          :paramitas (vector (list :paramita/id (cdr spec)))))
              (setq my-chatgpt-shell-last-edn (list :intent (cdr spec)))
              (my-chatgpt-shell--record-hud-state)))
          (should (= (hash-table-count my-chatgpt-shell--hud-state-registry) 2))
          (let ((my-chatgpt-shell-context-buffer-name hud-name)
                (hud nil))
            (setq hud (my-chatgpt-shell--context-buffer t))
            (with-current-buffer hud
              (setq my-chatgpt-shell--context-source chat-a)
              (my-chatgpt-shell--maybe-render-context)
              (should (string-match-p "viriya" (buffer-string)))
              (setq my-chatgpt-shell--context-source chat-b)
              (my-chatgpt-shell--maybe-render-context)
              (should (string-match-p "dana" (buffer-string)))
              (should (eq (my-chatgpt-shell--state-buffer) chat-b)))))
      (dolist (buf (list chat-a chat-b))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (when (get-buffer hud-name) (kill-buffer hud-name)))))

(ert-deftest futon3-hud-cues-use-inbound-when-present ()
  "HUD should render cues from inbound hints when they already contain fruits."
  (futon3--with-chat-buffer
    (setq my-chatgpt-shell-last-inbound-edn '(:intent "demo"
                                              :fruits ((:fruit/id "fresh"
                                                                  :emoji "🍉"
                                                                  :summary "updated"
                                                                  :score 0.42))
                                              :paramitas []))
    (setq my-chatgpt-shell-last-edn '(:intent "demo"
                                     :events []
                                     :fruits ((:fruit/id "stale"
                                                 :emoji "🍋"
                                                 :summary "old"
                                                 :score 0.11))
                                     :paramitas []))
    (my-chatgpt-shell--render-context t)
    (with-current-buffer (my-chatgpt-shell--context-buffer t)
      (let* ((contents (buffer-string))
             (start (string-match "Tatami cues" contents))
             (end (and start (string-match "Latest FROM-CHATGPT state" contents start)))
             (section (and start end (substring contents start end))))
        (should section)
        (should (string-match-p "🍉 fresh (42%)" section))
        (should-not (string-match-p "🍋 stale" section))))))

(ert-deftest futon3-hud-cues-fall-back-to-last-edn ()
  "HUD should fall back to FROM-CHATGPT cues when inbound hints are empty."
  (futon3--with-chat-buffer
    (setq my-chatgpt-shell-last-inbound-edn '(:intent "demo"
                                              :fruits []
                                              :paramitas []))
    (setq my-chatgpt-shell-last-edn '(:intent "demo"
                                     :events []
                                     :fruits ((:fruit/id "fresh"
                                                 :emoji "🍉"
                                                 :summary "updated"
                                                 :score 0.42))
                                     :paramitas []))
    (my-chatgpt-shell--render-context t)
    (with-current-buffer (my-chatgpt-shell--context-buffer t)
      (let* ((contents (buffer-string))
             (start (string-match "Tatami cues" contents))
             (end (and start (string-match "Latest FROM-CHATGPT state" contents start)))
             (section (and start end (substring contents start end))))
        (should section)
        (should (string-match-p "🍉 fresh (42%)" section))))))

(ert-deftest futon3-hud-fixtures-replay-cues ()
  "Recorded HUD fixtures should replay cues and guidance consistently."
  (let ((fixtures (futon3-test--read-hud-fixtures)))
    (should fixtures)
    (dolist (fixture fixtures)
      (futon3--with-chat-buffer
        (setq my-chatgpt-shell-last-inbound-edn (plist-get fixture :inbound))
        (setq my-chatgpt-shell-last-edn (plist-get fixture :chatgpt))
        (setq my-chatgpt-shell--last-hints-error (plist-get fixture :hints-error))
        (setq my-chatgpt-shell--last-cues-error (plist-get fixture :cues-error))
        (setq my-chatgpt-shell--last-sigil-salient nil
              my-chatgpt-shell--last-cue-salient nil)
        (my-chatgpt-shell--render-context t)
        (let* ((cue-salient my-chatgpt-shell--last-cue-salient)
               (status (plist-get cue-salient :status))
               (reason (plist-get cue-salient :reason)))
          (pcase (plist-get fixture :expect)
            (:inbound (should (eq status :inbound)))
            (:fallback (should (eq status :chatgpt)))
            (:missing (should (eq status :empty)))
            (_ (should status)))
          (with-current-buffer (my-chatgpt-shell--context-buffer t)
            (let ((contents (buffer-string)))
              (pcase status
                (:inbound
                 (let ((snippet (futon3-test--support-snippet (plist-get cue-salient :edn))))
                   (should snippet)
                   (should (string-match-p (regexp-quote snippet) contents))))
                ((or :chatgpt :empty)
                 (let ((msg (my-chatgpt-shell--cue-guidance status reason)))
                   (should msg)
                   (should (string-match-p (regexp-quote msg) contents))))
                (_ (should status))))))))))

(ert-deftest futon3-replay-last-turn-restores-state ()
  "Replaying the last turn should restore cached EDNs without network calls."
  (futon3--with-chat-buffer
    (let ((inbound '(:intent "demo" :fruits [] :paramitas []))
          (chatgpt '(:intent "demo"
                      :events [(:kind :note :pattern "foo" :notes "bar")]
                      :patterns [])))
      (cl-letf (((symbol-function 'my-chatgpt-shell--enrich-with-cues) (lambda (edn) edn))
                ((symbol-function 'my-chatgpt-shell-persist-edn) (lambda (&rest _) nil))
                ((symbol-function 'my-chatgpt-shell--validate-pattern-events) (lambda (&rest _) t))
                ((symbol-function 'my-chatgpt-shell--maybe-update-intent) (lambda (&rest _) nil))
                ((symbol-function 'my-chatgpt-shell--refresh-context-hints) (lambda (&rest _) nil))
                ((symbol-function 'my-chatgpt-shell--maybe-render-context) (lambda (&rest _) nil))
                ((symbol-function 'my-chatgpt-shell--render-context) (lambda (&optional _) nil)))
        (setq my-chatgpt-shell-last-inbound-edn inbound
              my-chatgpt-shell--last-replay-tatami-edn (cl-copy-list inbound)
              my-chatgpt-shell-last-edn nil)
        (my-chatgpt-shell--apply-chatgpt-edn chatgpt)
        (setq my-chatgpt-shell-last-edn nil
              my-chatgpt-shell-last-inbound-edn nil)
        (my-chatgpt-shell-replay-last-turn)
        (should (equal (plist-get my-chatgpt-shell-last-edn :intent) "demo"))
        (should (equal (plist-get my-chatgpt-shell-last-inbound-edn :intent) "demo"))))))

(ert-deftest futon3-replay-turn-carries-salients ()
  "Replaying should reuse intent sigils from the stored Tatami payload."
  (futon3--with-chat-buffer
    (cl-letf (((symbol-function 'my-chatgpt-shell--render-context) (lambda (&optional _) nil))
              ((symbol-function 'my-chatgpt-shell-persist-edn) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--validate-pattern-events) (lambda (&rest _) t))
              ((symbol-function 'my-chatgpt-shell--maybe-update-intent) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--refresh-context-hints) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--maybe-render-context) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--enrich-with-cues) (lambda (edn) edn)))
      (let* ((inbound '(:intent "demo"
                         :fruits ((:fruit/id "fresh" :emoji "🍉" :summary "updated" :score 0.42))
                         :paramitas []))
             (chatgpt '(:intent "demo"
                        :events []
                        :patterns []
                        :intent-sigils ((:emoji "📁" :hanzi "門") (:emoji "🚴" :hanzi "井")))))
        (setq my-chatgpt-shell--last-replay-chatgpt-edn (cl-copy-list chatgpt)
              my-chatgpt-shell--last-replay-tatami-edn (cl-copy-list inbound)
              my-chatgpt-shell-last-edn nil
              my-chatgpt-shell-last-inbound-edn nil
              my-chatgpt-shell--last-sigil-salient nil
              my-chatgpt-shell--last-cue-salient nil)
        (my-chatgpt-shell-replay-last-turn)
        (should (member "sigils=FROM-CHATGPT" (my-chatgpt-shell--salient-tags)))))))

(ert-deftest futon3-retarget-clears-stale-sigils ()
  "Retargeting should drop cached sigils so new defaults propagate."
  (let ((my-futon3-tatami-default-prototypes '("f3/p1"))
        (my-futon3-tatami-default-intent "old intent"))
    (futon3--with-chat-buffer
      (setq my-chatgpt-shell-last-edn '(:intent "old intent"
                                      :prototypes (vector "f3/p1")))
      (cl-letf (((symbol-function 'my-futon3--prototype-sigils)
                 (lambda (value)
                   (let ((ids (my-futon3--prototype-list value)))
                     (cond
                      ((equal ids '("f3/p1")) '((:emoji "🥚" :hanzi "旧")))
                      ((equal ids '("f2/p2")) '((:emoji "🌱" :hanzi "新")))
                      (t nil)))))
                ((symbol-function 'my-futon3-fetch-hints)
                 (lambda (&rest _) '(:patterns [] :fruits [] :paramitas [])))
                ((symbol-function 'my-futon3-ensure-tatami-session)
                 (lambda (&rest _) nil))
                ((symbol-function 'my-futon3--embed-sigils-from-text)
                 (lambda (&rest _) nil)))
        (setq my-futon3-tatami-default-prototypes '("f2/p2")
              my-futon3-tatami-default-intent "new intent")
        (my-chatgpt-shell--reset-context-state)
        (let ((edn (my-chatgpt-shell--build-inbound-edn)))
          (should (equal (plist-get edn :intent) "new intent"))
          (should (equal (plist-get edn :intent-sigils)
                         '((:emoji "🌱" :hanzi "新"))))
          (should (eq (plist-get edn :intent-sigils-source) :prototype))
          (should (equal (plist-get edn :intent-sigils-intent) "new intent")))))))


(ert-deftest tatami-context-log-persists-from-chatgpt-edn ()
  "FROM-CHATGPT payloads should be appended to the context log."
  (let ((log-file (make-temp-file "tatami-context" nil ".edn")))
    (unwind-protect
        (progn
          (with-temp-file log-file (insert "[]\n"))
          (futon3--with-chat-buffer
            (let ((my-chatgpt-shell-edn-log-file log-file))
              (setq my-chatgpt-shell-last-inbound-edn '(:session-id "S-log"))
              (my-chatgpt-shell--apply-chatgpt-edn '(:session-id "S-log"
                                                      :intent "demo"
                                                      :events []))
              (with-temp-buffer
                (insert-file-contents log-file)
                (goto-char (point-min))
                (let* ((records (read (current-buffer)))
                       (entry (and (> (length records) 0)
                                   (aref records (1- (length records))))))
                  (should entry)
                  (should (equal (plist-get entry :intent) "demo"))
                  (should (equal (plist-get entry :session-id) "S-log"))
                  (let ((payload (plist-get entry :payload)))
                    (should payload)
                    (should (equal (plist-get payload :session-id) "S-log"))
                    (should (equal (plist-get payload :intent) "demo"))
                    (let* ((raw (plist-get payload :events))
                           (normalized (cond ((vectorp raw) (append raw nil))
                                             (t raw))))
                      (should (equal normalized '())))))))))
      (when (file-exists-p log-file)
        (delete-file log-file)))))

(ert-deftest futon3-apply-chatgpt-edn-tags-sigils-source ()
  "Sigils coming from ChatGPT should be marked with a :chatgpt origin."
  (futon3--with-chat-buffer
    (cl-letf (((symbol-function 'my-chatgpt-shell-persist-edn) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--validate-pattern-events) (lambda (&rest _) t))
              ((symbol-function 'my-chatgpt-shell--maybe-update-intent) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--refresh-context-hints) (lambda (&rest _) nil))
              ((symbol-function 'my-chatgpt-shell--maybe-render-context) (lambda (&rest _) nil)))
      (my-chatgpt-shell--apply-chatgpt-edn '(:intent "demo"
                                             :events []
                                             :intent-sigils ((:emoji "🧭" :hanzi "道"))))
      (should (eq (plist-get my-chatgpt-shell-last-edn :intent-sigils-source) :chatgpt))
      (should (equal (plist-get my-chatgpt-shell-last-edn :intent-sigils-intent) "demo")))))

(provide 'aob-chatgpt-test)

;;; aob-chatgpt-test.el ends here
