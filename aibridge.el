;;; aibridge-org.el --- AI Bridge chat in an Org buffer -*- lexical-binding: t; -*-
;; Minimal terminal-like chat UI in an org-mode buffer.

(require 'org)
(require 'button)
(require 'spinner)
(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'aibridge-codex)

(defgroup aibridge-org nil
  "Terminal-like AI chat in Org."
  :group 'tools)

(defcustom aibridge-org-prompt "\n* AI >  "
  "Prompt string (Org-style heading)."
  :type 'string)

(defcustom aibridge-org-path-regexp
  "\\([~/\\.[:alnum:]_\\-/$]+\\.[[:alnum:]_]+\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?"
  "Detect file paths with optional :line[:col]."
  :type 'regexp)

(defface aibridge-org-prompt-face    '((t :inherit shadow :slant italic))   "")
(defface aibridge-org-user-face      '((t :inherit org-level-2 :weight bold)) ""
  )
(defface aibridge-org-assistant-face '((t :inherit org-document-info))     "")
(defface aibridge-org-status-face    '((t :inherit shadow :slant italic))   "")
(defface aibridge-org-status-done    '((t :inherit success))                "")

(defvar aibridge-org-buffer-name "*AI Bridge Org*")

(defvar aibridge-org--buffers-by-conversation (make-hash-table :test 'equal)
  "Map conversation ids to their active AI Bridge buffers and clients.")

(defun aibridge-org--short-id (cid)
  "Return a shortened representation of CID for buffer names."
  (cond
   ((not (stringp cid)) "unknown")
   ((<= (length cid) 8) cid)
   (t (concat (substring cid 0 8) "…"))))

(defun aibridge-org--conversation-buffer-name (cid)
  "Return the canonical buffer name for CID."
  (if (and (stringp cid) (not (string-empty-p cid)))
      (format "%s: %s" aibridge-org-buffer-name (aibridge-org--short-id cid))
    (generate-new-buffer-name (format "%s: new" aibridge-org-buffer-name))))

(defun aibridge-org--lookup-buffer-for-cid (cid)
  "Return the buffer currently associated with CID, if any."
  (let ((entry (and cid (gethash cid aibridge-org--buffers-by-conversation))))
    (and entry (buffer-live-p (plist-get entry :buffer))
         (plist-get entry :buffer))))

(defun aibridge-org--register-buffer-for-cid (cid &optional target)
  "Register TARGET buffer (or current buffer) as owner of CID and rename it.

TARGET can be a buffer object or a buffer name string. Falls back to
`aibridge-org--target-buffer' or `current-buffer' when nil. This avoids
depending on whatever `current-buffer' happens to be when async events arrive."
  (when (and (stringp cid) (not (string-empty-p cid)))
    (let* ((buf (cond
                 ((bufferp target) target)
                 ((stringp target) (get-buffer target))
                 (aibridge-org--target-buffer aibridge-org--target-buffer)
                 (t (current-buffer))))
           (name (aibridge-org--conversation-buffer-name cid)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (unless (string= (buffer-name buf) name)
            (rename-buffer name t))
          (puthash cid (list :buffer buf :client aibridge-org--client)
                   aibridge-org--buffers-by-conversation)
          (setq aibridge-org--registered-cid cid)
          (setq aibridge-org--target-buffer buf)
          buf)))))

(defun aibridge-org--unregister-buffer-for-cid (cid buf)
  "Remove BUF's association with CID, if it matches the registry entry."
  (when (and cid buf)
    (let ((entry (gethash cid aibridge-org--buffers-by-conversation)))
      (when (eq (plist-get entry :buffer) buf)
        (remhash cid aibridge-org--buffers-by-conversation)))))

(defvar-local aibridge-org--conversation-id nil)
(defvar-local aibridge-org--conversation-opts nil)
(defvar-local aibridge-org--prompt-bol (point-min))
(defvar-local aibridge-org--spinner nil)
(defvar-local aibridge-org--spinner-label "")
(defvar-local aibridge-org--status-overlays (make-hash-table :test 'equal))
(defvar-local aibridge-org--status-anchor nil)
(defvar-local aibridge-org--status-start nil)
(defvar-local aibridge-org--append-marker nil)
(defvar-local aibridge-org--target-buffer nil)
;; Track and update the displayed title (with model)
(defvar-local aibridge-org--title-overlay nil)
(defvar-local aibridge-org--registered-cid nil)

;; Model selection UI -----------------------------------------------------
(defcustom aibridge-org-base-models '("gpt-5" "gpt-5-codex")
  "Base models available for selection."
  :type '(repeat string))

(defcustom aibridge-org-efforts '("low" "medium" "high")
  "Effort levels available for selection."
  :type '(repeat string))

(defcustom aibridge-org-sandbox-modes '("workspace-write" "read-only" "danger-full-access")
  "Sandbox modes available for selection."
  :type '(repeat string))

(defcustom aibridge-org-approval-policies '("never" "untrusted" "on-request" "on-failure")
  "Approval policies available for selection."
  :type '(repeat string))

(defcustom aibridge-org-reasoning-preview-max 160
  "Maximum characters to show for live reasoning previews."
  :type 'integer)

(defun aibridge-org--combine-model-efforts ()
  "Return list of display strings like gpt-5-medium, gpt-5-codex-high."
  (apply #'append
         (mapcar (lambda (base)
                   (mapcar (lambda (eff) (format "%s-%s" base eff))
                           aibridge-org-efforts))
                 aibridge-org-base-models)))

(defun aibridge-org--parse-model-display (s)
  "Parse display string S (e.g., gpt-5-high) into (model . effort)."
  (when (and (stringp s) (string-match "^\n?\t*\s-*\(.*\)$" s))
    (setq s (match-string 1 s)))
  (let* ((parts (and (stringp s) (split-string s "-" t)))
         (eff (car (last parts)))
         (model (mapconcat #'identity (butlast parts) "-")))
    (when (and (member eff aibridge-org-efforts)
               (member model aibridge-org-base-models))
      (cons model eff))))

(defun aibridge-org--current-model-display ()
  "Return combined display like gpt-5-medium from current opts."
  (let* ((opts (aibridge-org--current-turn-opts))
         (cwd (or (alist-get 'cwd opts) "None"))
         (model (or (alist-get 'model opts) "gpt-5"))
         (effort (or (alist-get 'effort opts) "medium"))
         (sandbox (or (alist-get 'sandbox opts) "workspace-write"))
         (approval (or (alist-get 'approval-policy opts) "never"))
         (cwd-abbrev (condition-case _
                         (abbreviate-file-name (or cwd ""))
                       (error cwd))))
    ;; Put status on its own line, and abbreviate cwd when possible.
    (format "\n[%s-%s] [cwd:%s] [sandbox:%s] [approval:%s]"
            model effort cwd-abbrev sandbox approval)))

(defun aibridge-org--refresh-title ()
  "Refresh the title line to include the current model."
  (when (and (overlayp aibridge-org--title-overlay)
             (buffer-live-p (overlay-buffer aibridge-org--title-overlay)))
    (let* ((beg (overlay-start aibridge-org--title-overlay))
           (end (overlay-end   aibridge-org--title-overlay))
           (inhibit-read-only t)
           (cur   (buffer-substring-no-properties beg end))
           (model (aibridge-org--current-model-display))
           (strip-re (rx (group (*? any))
                         (1+ (seq (+ space) "[" (*? (not ?])) "]"))
                     (* space) eol))
      (base (or (overlay-get aibridge-org--title-overlay 'aibridge-base)
                (if (string-match strip-re cur) (match-string 1 cur) cur)))
      (sep (if (and (stringp model) (string-prefix-p "\n" model)) "" "  "))
      (new  (format "%s%s%s" base sep model)))
    (overlay-put aibridge-org--title-overlay 'aibridge-base base)
    (unless (string= cur new)             ; avoid doubling if already correct
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert (propertize new 'face 'mode-line))
        (move-overlay aibridge-org--title-overlay beg (+ beg (length new))))))))

(defun aibridge-org-change-model ()
  "Change the model/effort for this conversation via a completing-read."
  (interactive)
  (let* ((choices (aibridge-org--combine-model-efforts))
         (default (aibridge-org--current-model-display))
         (sel (completing-read "Model (model-effort): " choices nil t nil nil default))
         (parsed (aibridge-org--parse-model-display sel)))
    (if (not parsed)
        (message "Unrecognized model selection: %s" sel)
      (let* ((model (car parsed))
             (effort (cdr parsed)))
        (setq aibridge-org--conversation-opts
              (aibridge-org--merge-opts aibridge-org--conversation-opts
                                        `((model . ,model)
                                          (effort . ,effort))))
        (aibridge-org--refresh-title)
        (message "Model set to %s (%s)" model effort)))))

(defun aibridge-org-change-sandbox ()
  "Change the sandbox policy (mode) for this conversation."
  (interactive)
  (let* ((opts (aibridge-org--current-turn-opts))
         (current (or (alist-get 'sandbox opts) "workspace-write"))
         (sel (completing-read "Sandbox policy: " aibridge-org-sandbox-modes nil t nil nil current)))
    (setq aibridge-org--conversation-opts
          (aibridge-org--merge-opts aibridge-org--conversation-opts
                                    `((sandbox . ,sel))))
    (aibridge-org--refresh-title)
    (message "Sandbox policy set to %s" sel)))

(defun aibridge-org-change-approval ()
  "Change the approval policy for this conversation."
  (interactive)
  (let* ((opts (aibridge-org--current-turn-opts))
         (current (or (alist-get 'approval-policy opts) "never"))
         (sel (completing-read "Approval policy: " aibridge-org-approval-policies nil t nil nil current)))
    (setq aibridge-org--conversation-opts
          (aibridge-org--merge-opts aibridge-org--conversation-opts
                                    `((approval-policy . ,sel))))
    (aibridge-org--refresh-title)
    (message "Approval policy set to %s" sel)))
;;;; aibridge-org.el — per-buffer client ///////////////////////////////////

(defvar-local aibridge-org--client nil)          ;; <— per-buffer Codex client
(defvar-local aibridge-org--rollout-path nil)    ;; if you want to archive by path

(defun aibridge-org--attach-client! ()
  "Create a dedicated Codex client for this buffer and register teardown."
  (unless (and aibridge-org--client
               (process-live-p (aibridge-mcp-proc
                                (aibridge-codex-client-mcp aibridge-org--client))))
    (setq aibridge-org--client
          (aibridge-codex-make aibridge-codex-command default-directory))
    ;; Don’t nag on Emacs exit.
    (set-process-query-on-exit-flag
     (aibridge-mcp-proc (aibridge-codex-client-mcp aibridge-org--client)) nil)
    (add-hook 'kill-buffer-hook #'aibridge-org--teardown nil t)))

(defun aibridge-org--teardown ()
  "Buffer is closing: attempt a graceful end and kill our dedicated MCP."
  (when aibridge-org--conversation-id
    (ignore-errors
      (aibridge-codex--sync-request*
       aibridge-org--client "interruptConversation"
       `(("conversationId" . ,aibridge-org--conversation-id)) 1.5))
    ;; If your server archives by path instead of id, use aibridge-org--rollout-path
    (when-let* ((rollout (aibridge-codex--normalize-rollout-path
                          aibridge-org--rollout-path)))
      (ignore-errors
        (aibridge-codex--sync-request*
         aibridge-org--client "archiveConversation"
         `(("path" . ,rollout)) 1.5))))
  (aibridge-org--unregister-buffer-for-cid aibridge-org--registered-cid (current-buffer))
  (setq aibridge-org--registered-cid nil)
  (aibridge-codex-stop-client aibridge-org--client)
  (setq aibridge-org--client nil))


;; ——— conversation discovery -----------------------------------------

(defconst aibridge-org--new-conversation-label "Create new conversation"
  "Label for spawning a fresh Codex conversation from the picker.")

(defun aibridge-org--json-get (alist key)
  "Look up KEY in ALIST regardless of symbol/string/keyword form."
  (let* ((name (symbol-name key))
         (candidates (list key
                           (intern (concat ":" name))
                           (intern name)
                           name)))
    (catch 'found
      (dolist (candidate candidates)
        (let ((val (cond
                    ((stringp candidate) (cdr (assoc candidate alist)))
                    (t (cdr (assoc candidate alist))))))
          (when (not (eq val nil))
            (throw 'found val))))
      nil)))

(defun aibridge-org--relative-time (ts)
  "Return a short human string like \"2h ago\" for timestamp TS.
TS can be anything `date-to-time` accepts (e.g., ISO 8601)."
  (condition-case _
      (let* ((t2   (date-to-time ts))
             (now  (current-time))
             (secs (float-time (time-subtract now t2))))
        (cond
         ((< secs 0)           "in the future")              ; clock skew
         ((< secs 60)          "just now")
         ((< secs 3600)        (format "%dm ago" (floor (/ secs 60))))
         ((< secs 86400)       (format "%dh ago" (floor (/ secs 3600))))
         ((< secs (* 7 86400)) (format "%dd ago" (floor (/ secs 86400))))
         (t ;; older than a week: fall back to a calendar date (omit year if same)
          (let* ((y-now (string-to-number (format-time-string "%Y" now)))
                 (y-ts  (string-to-number (format-time-string "%Y" t2))))
            (if (= y-now y-ts)
                (format-time-string "%b %e" t2)   ; e.g., "Sep 12"
              (format-time-string "%Y-%m-%d" t2))))))
    (error "?")))

(defun aibridge-org--conversation->label (conv abs-root)
  "Return a display label for CONV scoped to ABS-ROOT (ID omitted)."
  (let* ((preview (aibridge-org--json-get conv 'preview))
         (ts      (aibridge-org--json-get conv 'timestamp))
         (base    (string-trim (or preview "")))
         (ago     (and ts (aibridge-org--relative-time ts))))
    (format "%s  (%s)"
            (if (string-empty-p base)
                "[no preview]"
              (truncate-string-to-width base 80 nil nil "…"))
            (or ago "?"))))


(defun aibridge-org--conversation-rollout-path (conv)
  "Extract the rollout path from CONV if present."
  (when (listp conv)
    (or (aibridge-org--json-get conv 'path)
        (aibridge-org--json-get conv 'rolloutPath)
        (aibridge-org--json-get conv 'resumePath)
        (let ((rollout (aibridge-org--json-get conv 'rollout)))
          (when (listp rollout)
            (or (aibridge-org--json-get rollout 'path)
                (aibridge-org--json-get rollout 'rolloutPath)
                (aibridge-org--json-get rollout 'resumePath)))))))

(defun aibridge-org--collect-project-conversations (abs-root)
  "Return Codex conversations whose cwd resides inside ABS-ROOT.
If ABS-ROOT is nil, return every conversation."
  (let ((convs (aibridge-codex-list-conversations* aibridge-org--client)))
    (cl-loop for conv in convs
             for cwd = (aibridge-org--json-get conv 'cwd)
             ;; Fallback: when cwd is missing, don't filter it out.
             for abs-cwd = (and cwd (expand-file-name cwd (or abs-root default-directory)))
             when (or (not abs-root)
                      (not cwd)
                      (and abs-cwd
                           (or (ignore-errors (file-equal-p abs-cwd abs-root))
                               (ignore-errors (file-in-directory-p abs-cwd abs-root)))))
             collect conv)))

(defun aibridge-org--conv-seconds (conv)
  "Return CONV timestamp as seconds since epoch (0 if missing/unparseable)."
  (let ((ts (aibridge-org--json-get conv 'timestamp)))
    (condition-case nil
        (if ts (float-time (date-to-time ts)) 0)
      (error 0))))

(defun aibridge-org--pick-conversation ()
  "Interactively select or create a Codex conversation."
  (let* ((root (or (aibridge-org--project-root) default-directory))
         (abs-root (and root (file-name-as-directory (expand-file-name root))))
         (convs (cl-sort (copy-sequence (aibridge-org--collect-project-conversations abs-root))
                         #'>
                         :key #'aibridge-org--conv-seconds))
         (entries (cl-loop for conv in convs
                           for label = (aibridge-org--conversation->label conv abs-root)
                           collect (cons label conv)))
         (labels (cons aibridge-org--new-conversation-label (mapcar #'car entries)))
         ;; Wrap labels in a metadata function that disables re-sorting
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
                    (complete-with-action action labels string pred))))
         (prompt (if abs-root
                     (format "Codex conversation (%s): "
                             (file-name-nondirectory (directory-file-name abs-root)))
                   "Codex conversation: "))
         (selection (completing-read prompt table nil t nil nil
                                     aibridge-org--new-conversation-label))
         (conv (cdr (assoc selection entries))))
    (if conv
        (list :kind 'existing
              :label selection
              :conversation conv
              :cwd  (or (aibridge-org--json-get conv 'cwd) abs-root)
              :path (aibridge-org--conversation-rollout-path conv))
      (list :kind 'new
            :label (if abs-root
                       (format "New conversation (%s)\n"
                               (file-name-nondirectory (directory-file-name abs-root)))
                     "New conversation")
            :cwd abs-root))))

;; ——— utilities

(defun aibridge-org--project-root ()
  (when-let* ((pr (project-current))) (project-root pr)))

(defun aibridge-org--default-turn-opts ()
  `((cwd . ,(expand-file-name default-directory))
    (sandbox . "workspace-write")
    (approval-policy . "never")
    (model . "gpt-5")
    (summary . "auto")
    (effort . "medium")))

(defun aibridge-org--merge-opts (base overrides)
  (let ((result (copy-sequence (or base '()))))
    (dolist (pair (or overrides '()) result)
      (let ((key (car pair))
            (value (cdr pair)))
        (setf (alist-get key result nil nil #'eq) value)))))

(defun aibridge-org--turn-opts->resume-overrides (opts)
  "Convert OPTS alist into overrides payload for resumeConversation."
  (let (out)
    (let ((cwd (aibridge-org--json-get opts 'cwd)))
      (when (and cwd (or (not (stringp cwd)) (not (string-empty-p cwd))))
        (push (cons "cwd" cwd) out)))
    (let ((sandbox (aibridge-org--json-get opts 'sandbox)))
      (when (and sandbox (or (not (stringp sandbox)) (not (string-empty-p sandbox))))
        (push (cons "sandboxPolicy" (list (cons "mode" sandbox))) out)
        (push (cons "sandbox" sandbox) out)))
    (let ((approval (or (aibridge-org--json-get opts 'approval-policy)
                        (aibridge-org--json-get opts 'approvalPolicy))))
      (when (and approval (or (not (stringp approval)) (not (string-empty-p approval))))
        (push (cons "approvalPolicy" approval) out)))
    (let ((model (aibridge-org--json-get opts 'model)))
      (when (and model (or (not (stringp model)) (not (string-empty-p model))))
        (push (cons "model" model) out)))
    (let ((summary (aibridge-org--json-get opts 'summary)))
      (when (and summary (or (not (stringp summary)) (not (string-empty-p summary))))
        (push (cons "summary" summary) out)))
    (let ((effort (aibridge-org--json-get opts 'effort)))
      (when (and effort (or (not (stringp effort)) (not (string-empty-p effort))))
        (push (cons "effort" effort) out)))
    (nreverse out)))

(defun aibridge-org--opts-from-conversation (conv)
  (let* ((sandbox-policy (aibridge-org--json-get conv 'sandboxPolicy))
         (sandbox-mode (and (listp sandbox-policy)
                            (aibridge-org--json-get sandbox-policy 'mode)))
         (sandbox (or sandbox-mode (aibridge-org--json-get conv 'sandbox)))
         (approval (or (aibridge-org--json-get conv 'approvalPolicy)
                       (aibridge-org--json-get conv 'approval-policy))))
    (cl-remove-if-not
     (lambda (pair) (let ((val (cdr pair))) (and val (not (equal val "")))))
     `((cwd . ,(aibridge-org--json-get conv 'cwd))
       (sandbox . ,sandbox)
       (approval-policy . ,approval)
       (model . ,(aibridge-org--json-get conv 'model))
       (summary . ,(aibridge-org--json-get conv 'summary))
       (effort . ,(aibridge-org--json-get conv 'effort))))))

(defun aibridge-org--current-turn-opts ()
  (aibridge-org--merge-opts (aibridge-org--default-turn-opts)
                            aibridge-org--conversation-opts))

(defun aibridge-org--abs (path)
  (cond
   ((file-name-absolute-p path) path)
   ((string-prefix-p "~/" path) (expand-file-name path))
   (t (expand-file-name path (or (aibridge-org--project-root) default-directory)))))

(defun aibridge-org--region-ro (beg end)
  (let ((inhibit-read-only t))
    (add-text-properties beg end '(read-only t rear-nonsticky (read-only)))))

(defun aibridge-org--start-spinner (label)
  (aibridge-org--in-target
   (lambda ()
     (setq aibridge-org--spinner-label label)
     (unless (and (spinner-p aibridge-org--spinner) aibridge-org--spinner)
       (setq aibridge-org--spinner (spinner-create 'progress-bar t 10)))
     (spinner-start aibridge-org--spinner)
     (setq mode-line-process
           '(" " (:eval (spinner-print aibridge-org--spinner)) " "
             (:eval (propertize aibridge-org--spinner-label 'face 'aibridge-org-status-face))))
     (force-mode-line-update t))))

(defun aibridge-org--stop-spinner ()
  (aibridge-org--in-target
   (lambda ()
     (when (spinner-p aibridge-org--spinner) (spinner-stop aibridge-org--spinner))
     (setq mode-line-process nil)
     (force-mode-line-update t))))

;; ——— linkify plain paths (also works inside org-mode)
(defun aibridge-org--linkify-paths (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward aibridge-org-path-regexp end t)
      (let* ((line (ignore-errors (string-to-number (match-string 2))))
             (col  (ignore-errors (string-to-number (match-string 3))))
             (path (match-string 1))
             (full (aibridge-org--abs path)))
        (when (file-exists-p full)
          (let ((b (match-beginning 0)) (e (match-end 0)))
            (make-text-button
             b e
             'action (lambda (_)
                       (find-file full)
                       (when (and line (> line 0))
                         (goto-char (point-min)) (forward-line (1- line)))
                       (when (and col (> col 0))
                         (move-to-column (1- col) t))))
            (put-text-property b e 'help-echo (format "Open %s" full))))))))

;; ——— prompt & send

(defun aibridge-org--ensure-target ()
  (unless (and (buffer-live-p aibridge-org--target-buffer))
    (setq aibridge-org--target-buffer
          (or (aibridge-org--lookup-buffer-for-cid aibridge-org--registered-cid)
              (current-buffer)))))

(defun aibridge-org--ensure-marker ()
  (aibridge-org--ensure-target)
  (with-current-buffer aibridge-org--target-buffer
    (unless (and (markerp aibridge-org--append-marker)
                 (marker-buffer aibridge-org--append-marker))
      (setq aibridge-org--append-marker (copy-marker (point-max) t)))))

(defun aibridge-org--insert-prompt ()
  (aibridge-org--in-target
   (lambda ()
     (aibridge-org--ensure-marker)
     (let ((inhibit-read-only t))
       (goto-char aibridge-org--append-marker)
       (insert (propertize aibridge-org-prompt 'face 'aibridge-org-prompt-face))
       (setq aibridge-org--prompt-bol (point))
       (let ((here (point)))
         (setq aibridge-org--status-anchor (copy-marker here t))
         (set-marker aibridge-org--append-marker here))))))

(defun aibridge-org--insert-assistant (text)
  (aibridge-org--in-target
   (lambda ()
     (aibridge-org--ensure-marker)
     (let ((inhibit-read-only t))
       (goto-char aibridge-org--append-marker)
       (let ((beg (point)))
         ;; org content; your chunks can already include org lists/headings
         (insert "\n" text "\n")
         (font-lock-flush beg (point))
         (font-lock-ensure beg (point))
         (aibridge-org--linkify-paths beg (point))
         (aibridge-org--region-ro beg (point))
         (set-marker aibridge-org--append-marker (point)))))))

(defun aibridge-org--insert-title (title)
  (goto-char (point-max))
  (unless (> (buffer-size) 0)
    (let* ((model-text (aibridge-org--current-model-display))
           (sep (if (and (stringp model-text)
                         (string-prefix-p "\n" model-text)) "" "  "))
           (title-text (format "%s%s%s" title sep model-text))
           (row-beg nil)
           (row-end nil))
      (insert (make-string 60 ?—) "\n")
      (setq row-beg (point))
      (insert (propertize title-text 'face 'mode-line))
      (setq row-end (point))
      (setq aibridge-org--title-overlay (make-overlay row-beg row-end))
      (overlay-put aibridge-org--title-overlay 'aibridge-base title)
      (insert "\n" (make-string 60 ?—)))))

(defun aibridge-org--status (text &optional id donep)
  (aibridge-org--in-target
   (lambda ()
     (aibridge-org--ensure-marker)
     (let ((inhibit-read-only t))
       (unless (and (markerp aibridge-org--status-anchor)
                    (marker-buffer aibridge-org--status-anchor))
         (let* ((fallback (if (and (markerp aibridge-org--append-marker)
                                   (marker-buffer aibridge-org--append-marker))
                              (marker-position aibridge-org--append-marker)
                            (point)))
                (start-pos (if (and (markerp aibridge-org--status-start)
                                    (marker-buffer aibridge-org--status-start))
                               (marker-position aibridge-org--status-start)
                             fallback)))
           (setq aibridge-org--status-start (copy-marker start-pos nil))
           (setq aibridge-org--status-anchor (copy-marker start-pos t))))
       (let* ((line (concat (if donep "✓ " "• ") (or text "") "\n"))
              (face (if donep 'aibridge-org-status-done 'aibridge-org-status-face))
              (existing (and id (gethash id aibridge-org--status-overlays)))
              start end)
         (if (and (overlayp existing)
                  (overlay-buffer existing)
                  (overlay-start existing)
                  (overlay-end existing))
             (progn
               (setq start (overlay-start existing))
               (setq end   (overlay-end existing))
               (goto-char start)
               (delete-region start end)
               (setq start (point))
               (insert (propertize line 'face face))
               (setq end (point))
               (move-overlay existing start end))
           (goto-char (marker-position aibridge-org--status-anchor))
           (setq start (point))
           (insert (propertize line 'face face))
           (setq end (point))
           (when id
             (puthash id (make-overlay start end)
                      aibridge-org--status-overlays))))
       (when (and start end)
         (aibridge-org--region-ro start end)
         (let* ((anchor-pos (if (and (markerp aibridge-org--status-anchor)
                                     (marker-buffer aibridge-org--status-anchor))
                                (marker-position aibridge-org--status-anchor)
                              start))
                (new-anchor (if anchor-pos (max anchor-pos end) end)))
           (set-marker aibridge-org--status-anchor new-anchor))
         (when (and (markerp aibridge-org--append-marker)
                    (marker-buffer aibridge-org--append-marker))
           (let ((append-pos (marker-position aibridge-org--append-marker)))
             (set-marker aibridge-org--append-marker
                         (if append-pos (max append-pos end) end)))))))))

(defun aibridge-org--collect-mentions (text)
  (let (files)
    (with-temp-buffer
      (insert text) (goto-char (point-min))
      (while (re-search-forward "@\\([A-Za-z0-9_./\\-]+\\)" nil t)
        (let ((p (aibridge-org--abs (match-string 1))))
          (when (file-exists-p p) (push p files)))))
    (nreverse files)))

(defun aibridge-org-insert-file-mention ()
  "Insert a project file path (relative to project root) at point."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (files (mapcar (lambda (f) (file-relative-name f root))
                        (project-files project)))
         (file (completing-read "Insert file: " files)))
    (insert file)))

;;; helpers ---------------------------------------------------------------

(defcustom aibridge-org-src-lang "markdown"
  "Language used in the #+begin_src block for assistant output."
  :type 'string)

(defun aibridge-org--safe-in-target (fn)
  "Run FN inside the target chat buffer, recreating markers if needed."
  (aibridge-org--in-target
   (lambda ()
     (when (buffer-live-p (current-buffer))
       (aibridge-org--ensure-marker)
       (funcall fn)))))

(defun aibridge-org--emit-begin-src ()
  "Insert a #+begin_src block header at append marker."
  (let ((inhibit-read-only t))
    (goto-char aibridge-org--append-marker)
    (insert "\n#+begin_src " aibridge-org-src-lang "\n")
    (set-marker aibridge-org--append-marker (point))))

(defun aibridge-org--append-chunk-into-src (text)
  "Append TEXT inside the open src block at append marker."
  (let ((inhibit-read-only t))
    (goto-char aibridge-org--append-marker)
    (let ((beg (point)))
      (insert (or text ""))                 ;; chunks can carry their own newlines
      (aibridge-org--linkify-paths beg (point))
      (set-marker aibridge-org--append-marker (point)))))

(defun aibridge-org--emit-end-src ()
  "Insert a #+end_src footer at append marker."
  (let ((inhibit-read-only t))
    (goto-char aibridge-org--append-marker)
    (insert "\n#+end_src\n")
    (set-marker aibridge-org--append-marker (point))))

(defun aibridge-org--handle-status-ev (ev)
  "Render a :status event EV."
  (aibridge-org--status (or (plist-get ev :text) "")
                        (plist-get ev :id)
                        (plist-get ev :done)))

(defun aibridge-org--handle-reasoning-ev (ev)
  "Render a :reasoning event EV as a live preview line."
  (let* ((raw (or (plist-get ev :text) ""))
         (done (plist-get ev :done))
         (flat (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " raw)))
         (preview (truncate-string-to-width flat aibridge-org-reasoning-preview-max nil nil "…"))
         (body (if (string-empty-p preview) "…" preview))
         (line (if done
                   (if (string-empty-p flat)
                       "Reasoning complete"
                     (format "Reasoning complete: %s" body))
                 (format "Reasoning: %s" body))))
    (aibridge-org--status line "reasoning" done)))

(defun aibridge-org--handle-chunk-ev (ev opened)
  "Render a :chunk event EV. Opens src if OPENED is nil. Returns new OPENED."
  (aibridge-org--safe-in-target
   (lambda ()
     (unless opened
       (aibridge-org--emit-begin-src)
       (setq opened t))
     (aibridge-org--append-chunk-into-src (plist-get ev :text))))
  opened)

(defun aibridge-org--handle-unknown-ev (ev opened)
  "Render unknown payloads as content. Returns new OPENED."
  (aibridge-org--safe-in-target
   (lambda ()
     (unless opened
       (aibridge-org--emit-begin-src)
       (setq opened t))
     (aibridge-org--append-chunk-into-src (format "%s" ev))))
  opened)

(defun aibridge-org--handle-done-ev (opened)
  "Finish the stream. Closes src (if OPENED) and inserts next prompt."
  (aibridge-org--stop-spinner)
  (aibridge-org--safe-in-target
   (lambda ()
     (when opened (aibridge-org--emit-end-src))
     (aibridge-org--insert-prompt))))

;;; aibridge-org-send (refactored) ---------------------------------------

(defun aibridge-org-send ()
  (interactive)
  (catch 'aibridge-org--abort-send
    (let* ((inhibit-read-only t)
           ;; Remember the buffer where this send originated so async events
           ;; don’t depend on whichever buffer is focused later.
           (origin-buf (current-buffer))
           (bol aibridge-org--prompt-bol)
           (eol (line-end-position)))
      ;; Empty line: insert a fresh prompt and abort synchronously.
      (when (<= eol bol)
        (goto-char (point-max))
        (insert "\n")
        (aibridge-org--insert-prompt)
        (throw 'aibridge-org--abort-send :empty))

      ;; Freeze the user line
      (add-text-properties bol eol
                           '(face aibridge-org-user-face read-only t rear-nonsticky (read-only)))
      (goto-char eol)
      (insert "\n")
      (aibridge-org--region-ro (max (point-min) (1- bol)) (point))

      ;; Reset per-turn UI anchors
      (setq aibridge-org--status-start (copy-marker (point) nil))
      (setq aibridge-org--status-anchor (copy-marker (point) t))
      (set-marker aibridge-org--append-marker (point))
      (clrhash aibridge-org--status-overlays)

      (let* ((text   (buffer-substring-no-properties bol eol))
             (opened nil))
        (aibridge-org--start-spinner "Working...")

        ;; START THE STREAM HERE:
        (aibridge-org--backend-send
         text nil
         (lambda (ev)
           ;; Route all async UI updates to the originating buffer, if alive.
           (when (buffer-live-p origin-buf)
             (with-current-buffer origin-buf
               ;; Never allow async paths to "return" from aibridge-org-send.
               (condition-case err
                   (pcase (plist-get ev :type)
                     ;; Capture Codex session id once on first turn
                     ('session
                      (let ((cid (plist-get ev :conversation-id)))
                        (when (and cid (stringp cid))
                          (aibridge-org--safe-in-target
                           (lambda ()
                             (setq aibridge-org--conversation-id cid)
                             (aibridge-org--register-buffer-for-cid cid (current-buffer))))
                          (aibridge-org--handle-status-ev
                           (list :id "session" :text (format "session %s" cid) :done t)))))

                  ('status (aibridge-org--handle-status-ev ev))
                  ('reasoning (aibridge-org--handle-reasoning-ev ev))
                  ('chunk  (setq opened (aibridge-org--handle-chunk-ev ev opened)))
                     ('done   (aibridge-org--handle-done-ev opened))
                     (_       (setq opened (aibridge-org--handle-unknown-ev ev opened))))
                 (no-catch
                  ;; Swallow stale `return`/`cl-return` aimed at the send’s block.
                  (when (and (consp err)
                             (eq (car err) 'no-catch)
                             ;; Tag from cl-return inside aibridge-org-send
                             (equal (cadr err) '--cl-block-aibridge-org-send--))
                    ;; silently ignore
                    nil))
                 (error
                  ;; Don’t break the UI on unexpected errors
                  (message "[aibridge] send callback error: %S" err)))))))))))


(defun aibridge-org-newline ()
  "Insert a newline inside the prompt."
  (interactive) (insert "\n"))

;; ——— simple backend demo with streaming/status
(defun aibridge-org--deliver (cb ev) (funcall cb ev))

(defun aibridge-org--ensure-conversation (&optional opts)
  "Ensure this buffer has an active Codex conversation.
Returns the conversation id when successful, otherwise nil."
  (if (and (stringp aibridge-org--conversation-id)
           (not (string-empty-p aibridge-org--conversation-id)))
      aibridge-org--conversation-id
    (let* ((opts (or opts (aibridge-org--current-turn-opts)))
           (res  (aibridge-codex-new-conversation* aibridge-org--client opts)))
      (pcase res
        (`(:ok ,obj)
         (let* ((cid (aibridge-org--json-get obj 'conversationId))
                (rollout (aibridge-org--conversation-rollout-path obj))
                (conv-opts (aibridge-org--opts-from-conversation obj))
                (merged (aibridge-org--merge-opts conv-opts opts))
                (normalized (when rollout (aibridge-codex--normalize-rollout-path rollout))))
           (setq aibridge-org--conversation-id cid)
           (setq aibridge-org--conversation-opts merged)
           (setq aibridge-org--rollout-path (or normalized rollout))
           (aibridge-org--register-buffer-for-cid cid (current-buffer))
           (aibridge-org--refresh-title)
           cid))
        (`(:error ,err)
         (aibridge-org--status
          (format "New conversation failed: %s"
                  (or (alist-get 'message err) err))
          "conversation")
         nil)
        (_ nil)))))

(defun aibridge-org--backend-send (text _files cb)
  "Send TEXT to Codex; requires an existing conversation id.
Does NOT create conversations. Creation must happen earlier (e.g.,
in `aibridge-org`)." 
  (funcall cb (list :type 'status :id "started" :text "Contacting Codex..."))
  (let* ((turn-opts (aibridge-org--current-turn-opts))
         (cid aibridge-org--conversation-id))
    (if (and (stringp cid) (not (string-empty-p cid)))
        (aibridge-codex-continue* aibridge-org--client cid text cb turn-opts)
      (funcall cb (list :type 'status
                        :id "conversation"
                        :text "No active Codex conversation (open or resume first)"))
      (funcall cb (list :type 'done)))))

(defun aibridge-org--in-target (fn &rest args)
  (aibridge-org--ensure-target)
  (when (buffer-live-p aibridge-org--target-buffer)
    (with-current-buffer aibridge-org--target-buffer
      (apply fn args))))

;; ——— major mode

(defvar aibridge-org-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")    #'aibridge-org-send)     ;; send
    (define-key m (kbd "S-RET")  #'aibridge-org-newline)  ;; newline within prompt
    (define-key m (kbd "C-c C-c")#'aibridge-org-send)
    (define-key m (kbd "@")      #'aibridge-org-insert-file-mention)
    (define-key m (kbd "C-c m")  #'aibridge-org-change-model) ;; change model
    (define-key m (kbd "C-c s")  #'aibridge-org-change-sandbox) ;; change sandbox policy
    (define-key m (kbd "C-c p")  #'aibridge-org-change-approval) ;; change approval policy
    ;; Org's standard link opener:
    (define-key m (kbd "C-c C-o") #'org-open-at-point)
    m)
  "Keymap for `aibridge-org-mode'.")

(define-derived-mode aibridge-org-mode org-mode "AI-Bridge-Org"
  "Chat-style Org buffer."
  (setq-local header-line-format nil)
  (setq-local electric-indent-inhibit t)
  (setq-local truncate-lines nil)
  (font-lock-mode 1)
  (setq-local font-lock-multiline t)
  (read-only-mode -1)
  ;; (goto-char (point-max))
  ;; (unless (> (buffer-size) 0)
  ;;   (insert (propertize "AI Bridge (Org)\n" 'face 'mode-line))
  ;;   (insert (make-string 60 ?—) "\n\n"))
  (setq aibridge-org--conversation-id nil)
  (setq aibridge-org--conversation-opts nil)
  (setq aibridge-org--spinner nil)
  (setq aibridge-org--spinner-label "")
  (setq aibridge-org--status-overlays (make-hash-table :test 'equal))
  (setq aibridge-org--status-anchor nil)
  (setq aibridge-org--status-start nil)
  (setq-local default-directory
              (or (when-let ((pr (project-current))) (project-root pr))
                  default-directory))
  (setq aibridge-org--target-buffer (current-buffer))
  (setq aibridge-org--append-marker (copy-marker (point-max) t))
  (setq aibridge-org--title-overlay nil)
  (setq aibridge-org--registered-cid nil)
  (aibridge-org--attach-client!)
  )

;;;###autoload
;;;###autoload
(defun aibridge-org--maybe-switch-to-existing (choice temp-buffer)
  "If CHOICE targets an already-open conversation, reuse its buffer.
Clean up TEMP-BUFFER (and its client) before switching. Returns the reused buffer or nil."
  (when (eq (plist-get choice :kind) 'existing)
    (let* ((conv (plist-get choice :conversation))
           (cid  (aibridge-org--json-get conv 'conversationId))
           (existing (aibridge-org--lookup-buffer-for-cid cid)))
      (when (buffer-live-p existing)
        (with-current-buffer temp-buffer
          (aibridge-org--teardown))
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer))
        (pop-to-buffer existing)
        (aibridge-org--refresh-title)
        (message "Reusing AI Bridge conversation buffer %s" (buffer-name existing))
        existing))))

;;;###autoload
;;;###autoload
(defun aibridge-org ()
  "Open the AI Bridge Org chat."
  (interactive)
  (catch 'aibridge-org-done
    (let* ((buf (generate-new-buffer aibridge-org-buffer-name)))
      (pop-to-buffer buf)
      (aibridge-org-mode)                   ;; ← creates per-buffer client
      ;; now it's safe to query Codex
      (let* ((choice (aibridge-org--pick-conversation)))
        (when-let ((existing (aibridge-org--maybe-switch-to-existing choice buf)))
          (throw 'aibridge-org-done existing))
        (when-let ((cwd (plist-get choice :cwd)))
          (setq-local default-directory (file-name-as-directory (expand-file-name cwd))))
        (pcase (plist-get choice :kind)
          ('existing
           (let* ((conv (plist-get choice :conversation))
                  (cid  (aibridge-org--json-get conv 'conversationId))
                  (path (plist-get choice :path))
                  (turn-opts (aibridge-org--current-turn-opts))
                  (overrides (aibridge-org--turn-opts->resume-overrides turn-opts)))
             (pcase (aibridge-codex-resume* aibridge-org--client cid path overrides)
               (`(:ok ,res)
                (setq aibridge-org--conversation-id cid)
                (setq aibridge-org--conversation-opts
                      (let* ((conv-opts (aibridge-org--opts-from-conversation conv))
                             (res-opts  (aibridge-org--opts-from-conversation res))
                             (merged    (aibridge-org--merge-opts conv-opts res-opts)))
                        (aibridge-org--merge-opts merged turn-opts)))
                (setq aibridge-org--rollout-path
                      (or (aibridge-codex--normalize-rollout-path path) path))
                (aibridge-org--register-buffer-for-cid cid (current-buffer))
                (aibridge-org--insert-title (plist-get choice :label))
                (aibridge-org--insert-prompt))
               (`(:error ,err)
                (aibridge-org--status
                 (format "Resume failed: %s" (or (alist-get 'message err) err))
                 "conversation")))))
          ('new
           (setq aibridge-org--conversation-id nil)
           (setq aibridge-org--conversation-opts nil)
           (setq aibridge-org--rollout-path nil)
           (setq aibridge-org--registered-cid nil)
           (let ((turn-opts (aibridge-org--current-turn-opts)))
             (aibridge-org--ensure-conversation turn-opts))
           (aibridge-org--insert-title (plist-get choice :label))
           (aibridge-org--insert-prompt))
          (_ (message "Unknown conversation choice: %S" choice)))))))

(provide 'aibridge-org)
;;; aibridge-org.el ends here
