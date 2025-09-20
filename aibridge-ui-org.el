;;; aibridge-ui-org.el --- AI Bridge chat in an Org buffer (UI module) -*- lexical-binding: t; -*-
;; Thin UI entry point. For now, reuse the existing Org UI.

;; Shared UI state/defs extracted from legacy file ------------------------

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
(defface aibridge-org-user-face      '((t :inherit org-level-2 :weight bold)) "")
(defface aibridge-org-assistant-face '((t :inherit org-document-info))     "")
(defface aibridge-org-status-face    '((t :inherit shadow :slant italic))   "")
(defface aibridge-org-status-done    '((t :inherit success))                "")

(defvar aibridge-org-buffer-name "*AI Bridge Org*")

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
    (format "\n[%s-%s] [cwd:%s] [sandbox:%s] [approval:%s]"
            model effort cwd-abbrev sandbox approval)))

(defvar-local aibridge-org--title-overlay nil)

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
    (unless (string= cur new)
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

;;;###autoload
(defun aibridge-chat ()
  "Open the AI Bridge chat UI (Org-based)."
  (interactive)
  (require 'aibridge) ;; legacy loader still defines `aibridge-org'
  (call-interactively #'aibridge-org))

(provide 'aibridge-ui-org)
;;; aibridge-ui-org.el ends here
