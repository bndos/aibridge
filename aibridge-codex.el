;;; aibridge-codex.el --- Codex MCP binding -*- lexical-binding: t; -*-

(require 'aibridge-mcp)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defgroup aibridge-codex nil
  "Codex MCP client."
  :group 'tools)

(defcustom aibridge-codex-command '("codex" "mcp")
  "How to launch Codex MCP server."
  :type '(repeat string))

(defcustom aibridge-codex-approval-default 'denied
  "Default `ReviewDecision` when no user handler is installed."
  :type '(choice (const approved)
          (const approved_for_session)
          (const denied)
          (const abort)))

(defvar aibridge-codex--client nil)
(defvar aibridge-codex--pending-approvals (make-hash-table :test 'equal))
(defvar aibridge-org--registered-cid nil)
(defvar aibridge-org--conversation-id nil)
(defvar aibridge-org-mode nil)

;; Active streams keyed by requestId (and optionally by session/conversation id)
(defvar aibridge-codex--streams-by-rid (make-hash-table :test 'equal))
(defvar aibridge-codex--streams-by-cid (make-hash-table :test 'equal))
;; each entry value is a plist: (:cb <fn> :cid <string>)

(defconst aibridge-codex--exec-status-id "exec:command")

(defun aibridge-codex--key-variants (key)
  "Return a list of equivalent representations for KEY."
  (let ((variants (list key)))
    (cond
     ((symbolp key)
      (push (symbol-name key) variants)
      (when (keywordp key)
        (let* ((name (substring (symbol-name key) 1))
               (sym  (intern name)))
          (push name variants)
          (push sym variants))))
     ((stringp key)
      (let ((sym (ignore-errors (intern key)))
            (kw  (ignore-errors (intern (concat ":" key)))))
        (when sym (push sym variants))
        (when kw  (push kw variants)))))
    (cl-delete-duplicates (delq nil variants) :test #'equal)))

(defun aibridge-codex--aget (table &rest keys)
  "Return the first value for KEYS within TABLE (alist or hash-table)."
  (catch 'found
    (dolist (key keys)
      (dolist (variant (aibridge-codex--key-variants key))
        (cond
         ((hash-table-p table)
          (let* ((marker (make-symbol "aibridge-codex--missing"))
                 (val (gethash variant table marker)))
            (unless (eq val marker)
              (throw 'found val))))
         ((listp table)
          (let ((cell (assoc variant table)))
            (when cell (throw 'found (cdr cell))))))))
    nil))
(defun aibridge-codex--windows-absolute-path-p (path)
  (and (stringp path)
       (string-match-p "\\`[A-Za-z]:[\\\\/]" path)))

(defun aibridge-codex--unc-path-p (path)
  (and (stringp path)
       (string-prefix-p "\\\\" path)))

(defun aibridge-codex--absolute-rollout-path-p (path)
  (and (stringp path)
       (or (file-name-absolute-p path)
           (aibridge-codex--windows-absolute-path-p path)
           (aibridge-codex--unc-path-p path))))

(defun aibridge-codex--normalize-rollout-path (path)
  (cond
   ((not (stringp path)) nil)
   ((aibridge-codex--windows-absolute-path-p path) path)
   ((aibridge-codex--unc-path-p path) path)
   ((file-name-absolute-p path) (expand-file-name path))
   (t (expand-file-name path))))
;; --- Approval minor-mode ------------------------------------------------

(defvar-local aibridge-codex-approval-call-id nil)
(defvar-local aibridge-codex-approval-kind   nil) ;; 'apply-patch or 'exec-command
;;; --- Anchor-follow, persp-agnostic pop policy -------------------------
;; ---- Easier anchoring --------------------------------------------------

(defcustom aibridge-codex-anchor-buffer-regexp
  "^\\*+\\s-*AI Bridge\\s-+Org\\(?:[^\\n]*\\)?\\*+\\'"
  "Match AI Bridge buffers like \"*AI Bridge Org*\" (with optional suffix)."
  :type 'regexp)

;; Track the most recently-selected anchor buffer (by object, not name).
(defvar aibridge-codex--last-anchor-buf nil)

(cl-defstruct aibridge-codex-client
  mcp ; the aibridge-mcp struct
  )

(defun aibridge-codex--ensure-listener* (client cid)
  "Always ask the server to attach a listener for CID.
Treat 'already attached' errors as success. No local cache."
  (when (stringp cid)
    (pcase (aibridge-codex--sync-request* client
                                          "addConversationListener"
                                          `(("conversationId" . ,cid))
                                          10.0)
      (`(:ok ,_)
       (message "[codex] listener attached for %s" cid)
       t)
      (`(:error ,err)
       nil))))

(defun aibridge-codex--ensure-client (client)
  (unless (and client (aibridge-codex-client-p client))
    (error "Codex: expected per-buffer client, got %S" client))
  client)

(defun aibridge-codex--buffer-conversation-id (buf)
  "Return the conversation id associated with BUF, if any."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (cond
       ((and (boundp 'aibridge-org--registered-cid)
             (stringp aibridge-org--registered-cid)
             (not (string-empty-p aibridge-org--registered-cid)))
        aibridge-org--registered-cid)
       ((and (boundp 'aibridge-org--conversation-id)
             (stringp aibridge-org--conversation-id)
             (not (string-empty-p aibridge-org--conversation-id)))
        aibridge-org--conversation-id)
       (t nil)))))

(defun aibridge-codex--anchor-buffer-p (buf)
  (and (buffer-live-p buf)
       (or (string-match-p aibridge-codex-anchor-buffer-regexp (buffer-name buf))
           (with-current-buffer buf (derived-mode-p 'aibridge-org-mode))
           (aibridge-codex--buffer-conversation-id buf))))

(defun aibridge-codex--note-anchor! ()
  "If current buffer is an anchor, remember it."
  (when (aibridge-codex--anchor-buffer-p (current-buffer))
    (setq aibridge-codex--last-anchor-buf (current-buffer))
    (unless aibridge-codex-anchor-follow-mode
      (aibridge-codex-anchor-follow-mode 1))))

;; Keep your existing auto-enable flag:
(defcustom aibridge-codex-auto-anchor-default t
  "Auto-enable follow mode in AI Bridge buffers and remember them as anchors."
  :type 'boolean)

(defun aibridge-codex--global-anchor-tick ()
  (when aibridge-codex-auto-anchor-default
    (aibridge-codex--note-anchor!)))

(add-hook 'buffer-list-update-hook #'aibridge-codex--global-anchor-tick)
(add-hook 'window-selection-change-functions
          (lambda (_frame) (aibridge-codex--global-anchor-tick)))

(defun aibridge-codex--pending-unshown-bufs (&optional conversation-id)
  "Return approval buffers that exist but aren’t visible.
When CONVERSATION-ID is provided, restrict to entries for that id."
  (let (out)
    (maphash
     (lambda (_call-id entry)
       (let ((b (plist-get entry :buf))
             (cid (plist-get entry :conversation-id)))
         (when (and (buffer-live-p b)
                    (not (get-buffer-window b t)))
           (when (or (not conversation-id)
                     (not cid)
                     (string= cid conversation-id))
             (push b out)))))
     aibridge-codex--pending-approvals)
    (nreverse out)))

(defun aibridge-codex--pop-approvals-here ()
  "If we’re in an anchor buffer, pop any pending approval buffers in this frame."
  (when (aibridge-codex--anchor-buffer-p (current-buffer))
    (let ((cid (aibridge-codex--buffer-conversation-id (current-buffer))))
      (dolist (b (aibridge-codex--pending-unshown-bufs cid))
        ;; polite: reuse a window in this frame; don’t switch frames
        (let ((display-buffer-overriding-action
               '((display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (inhibit-switch-frame . t))))
          (display-buffer b))))))

(define-minor-mode aibridge-codex-anchor-follow-mode
  "When enabled in your AI Bridge buffer, auto-pop Codex approvals here only."
  :lighter " Codex-Follow"
  (if aibridge-codex-anchor-follow-mode
      ;; Buffer-local hook: runs only while you’re in this buffer
      (add-hook 'post-command-hook #'aibridge-codex--pop-approvals-here nil t)
    (remove-hook 'post-command-hook #'aibridge-codex--pop-approvals-here t)))

(defun aibridge-codex-approval--send (decision)
  "Send DECISION (\"approved\"/\"denied\"/...) for this buffer's call-id."
  (let* ((cid   aibridge-codex-approval-call-id)
         (entry (and cid (gethash cid aibridge-codex--pending-approvals)))
         (reply (plist-get entry :reply)))
    (if (not (and cid reply))
        (message "[codex] no pending approval/reply for call %S" cid)
      (funcall reply `(("decision" . ,decision)))
      (remhash cid aibridge-codex--pending-approvals)
      (message "[codex] %s: %s"
               (or (symbol-name aibridge-codex-approval-kind) "approval")
               decision)
      (quit-window 'kill))))

(defun aibridge-codex-approval-approve () (interactive)
       (aibridge-codex-approval--send "approved"))

(defun aibridge-codex-approval-deny () (interactive)
       (aibridge-codex-approval--send "denied"))

(defvar aibridge-codex-approval-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'aibridge-codex-approval-approve) ;; approve
    (define-key m (kbd "C-c C-k") #'aibridge-codex-approval-deny)    ;; deny
    m)
  "Keys for `aibridge-codex-approval-mode'.")

(define-minor-mode aibridge-codex-approval-mode
  "Inline approval: C-c C-c approve, C-c C-k deny."
  :lighter " Codex-Approve"
  :keymap aibridge-codex-approval-mode-map)

(defun aibridge-codex--register-codex-events (mcp)
  (dolist (meth '("codex/event" ;; keep if some builds still use it
                  "codex/event/session_configured"
                  "codex/event/task_started"
                  "codex/event/task_complete"
                  "codex/event/agent_message_delta"
                  "codex/event/agent_message"
                  "codex/event/agent_reasoning_delta"
                  "codex/event/agent_reasoning"
                  "codex/event/agent_reasoning_section_break"
                  "codex/event/exec_command_begin"
                  "codex/event/exec_command_end"
                  "codex/event/exec_command_output_delta"
                  "codex/event/exec_command_output"
                  "codex/event/apply_patch_approval_request"
                  "codex/event/exec_approval_request"
                  "codex/event/token_count"))
    (aibridge-mcp-register-notify mcp meth #'aibridge-codex--on-event)))

(defun aibridge-codex-make (&optional command default-directory)
  (let* ((mcp (aibridge-mcp-start (or command aibridge-codex-command)
                                  default-directory))
         (client (make-aibridge-codex-client :mcp mcp)))
    ;; Wire handlers on *this* MCP only:
    (aibridge-codex--register-codex-events mcp)

    ;; requests: server -> client (NEW)
    (aibridge-mcp-register-request mcp "execCommandApproval"
                                   #'aibridge-codex--on-exec-command-approval)
    (aibridge-mcp-register-request mcp "applyPatchApproval"
                                   #'aibridge-codex--on-apply-patch-approval)

    ;; keep compatibility path
    (aibridge-mcp-register-request mcp "elicitation/create"
                                   #'aibridge-codex--on-elicitation-create)

    ;; optional auth notifies, sentinel… (unchanged)
    (aibridge-mcp-register-notify mcp "loginChatGptComplete"
                                  (lambda (p) (run-hook-with-args 'aibridge-codex-login-complete-hook p)))
    (aibridge-mcp-register-notify mcp "authStatusChange"
                                  (lambda (p) (run-hook-with-args 'aibridge-codex-auth-change-hook p)))
    (set-process-sentinel
     (aibridge-mcp-proc mcp)
     (lambda (_proc ev) (message "[codex] (per-buffer) %s" (string-trim ev))))
    client))


(defun aibridge-codex-stop-client (client)
  "Hard stop and cleanup for a per-buffer client."
  (when (and client (aibridge-codex-client-p client))
    (let ((mcp (aibridge-codex-client-mcp client)))
      (when (and mcp (process-live-p (aibridge-mcp-proc mcp)))
        (aibridge-mcp-stop mcp)))))

;; Small helpers that accept explicit CLIENT (no globals).
(defun aibridge-codex--sync-request* (client method &optional params timeout)
  (setq client (aibridge-codex--ensure-client client))
  (message "aibridge-codex--sync-request %s %s" method params)
  (let* ((mcp (aibridge-codex-client-mcp client))
         (proc (and mcp (aibridge-mcp-proc mcp)))
         (timeout (or timeout 8.0))
         (deadline (+ (float-time) timeout))
         result done id)
    (unless (and proc (process-live-p proc))
      (error "Codex MCP process is not running"))
    (setq id (aibridge-mcp-request
              mcp method (or params '())
              (lambda (res) (setq result res done t))))
    (unwind-protect
        (progn
          (while (and (not done) (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.1))
          (unless done
            (setq result `(:error ((code . timeout)
                                   (message . ,(format "Timed out waiting for %s" method)))))
            (message "[codex] %s" (alist-get 'message (cadr result)))))
      (unless done (remhash id (aibridge-mcp-pending mcp))))
    result))

(defun aibridge-codex-list-conversations* (client &optional params)
  (pcase (aibridge-codex--sync-request* client "listConversations" params)
    (`(:ok ,obj)
     (let* ((items (or (alist-get 'items obj nil nil #'equal)
                       (alist-get 'conversations obj nil nil #'equal)
                       obj)))
       (aibridge-codex--as-list items)))
    (`(:error ,err)
     (message "[codex] listConversations failed: %s"
              (or (alist-get 'message err) err))
     nil)))

(defun aibridge-codex-resume* (client cid path &optional overrides)
  "Resume a conversation from PATH, return `(:ok OBJ)` or `(:error ERR)`.
On success, also ensure a listener is attached for CID."
  (setq client (aibridge-codex--ensure-client client))
  (setq path (and path (expand-file-name path)))
  (unless (and (stringp path) (file-name-absolute-p path))
    (user-error "resumeConversation needs an absolute rollout path (got %S)" path))
  (let* ((ov (cond
              ((null overrides) nil)
              ((hash-table-p overrides) overrides)
              ((and (listp overrides) (null overrides)) (make-hash-table :test 'equal))
              ((listp overrides) overrides)
              (t overrides)))
         (res (aibridge-codex--sync-request* client "resumeConversation"
                                             (append `(("path" . ,path))
                                                     (when ov `(("overrides" . ,ov))))
                                             20.0)))
    (pcase res
      (`(:ok ,_)
       (aibridge-codex--ensure-listener* client cid))
      (_ nil))
    res))

(defun aibridge-codex-new-conversation* (client opts)
  "Start a fresh Codex conversation using OPTS.
Returns `(:ok OBJ)` with the created conversation descriptor or
`(:error ERR)` if the request failed."
  (setq client (aibridge-codex--ensure-client client))
  (let* ((opts (or opts '()))
         (model (alist-get 'model opts nil nil #'eq))
         (profile (alist-get 'profile opts nil nil #'eq))
         (cwd (alist-get 'cwd opts nil nil #'eq))
         (approval (alist-get 'approval-policy opts nil nil #'eq))
         (sandbox (alist-get 'sandbox opts nil nil #'eq))
         (config (alist-get 'config opts nil nil #'eq))
         (base (alist-get 'base-instructions opts nil nil #'eq))
         (plan (alist-get 'include-plan-tool opts nil nil #'eq))
         (apply (alist-get 'include-apply-patch-tool opts nil nil #'eq))
         (params (seq-filter
                  #'identity
                  (list
                   (when model                           (cons "model" model))
                   (when profile                         (cons "profile" profile))
                   (when cwd                             (cons "cwd" cwd))
                   (when approval                        (cons "approvalPolicy" approval))
                   (when sandbox                         (cons "sandbox" sandbox))
                   (when config                          (cons "config" config))
                   (when base                            (cons "baseInstructions" base))
                   (when (not (eq plan nil))             (cons "includePlanTool" plan))
                   (when (not (eq apply nil))            (cons "includeApplyPatchTool" apply))))))
    (let ((res (aibridge-codex--sync-request* client "newConversation" params 20.0)))
      (pcase res
        (`(:ok ,obj)
         (when-let ((cid (aibridge-codex--aget obj 'conversationId 'id 'conversation_id)))
           (aibridge-codex--ensure-listener* client cid)))
        (_ nil))
      res)))

(defun aibridge-codex--opt (opts &rest keys)
  (catch 'found
    (dolist (key keys)
      (let ((val (cond
                  ((symbolp key) (alist-get key opts nil nil #'eq))
                  ((stringp key) (cdr (assoc key opts)))
                  (t nil))))
        (when (not (eq val nil))
          (throw 'found val))))
    nil))

(defun aibridge-codex--stringify (value)
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((null value) nil)
   (t (format "%s" value))))

(defun aibridge-codex--normalize-items (items)
  (cond
   ((null items) nil)
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t (user-error "Codex items must be a list or vector (got %S)" items))))

(defun aibridge-codex--turn-items (prompt opts)
  (let ((items (aibridge-codex--normalize-items
                (aibridge-codex--opt opts 'items :items "items"))))
    (cond
     (items items)
     ((stringp prompt)
      (list `(("type" . "text") ("data" . (("text" . ,prompt))))))
     ((vectorp prompt)
      (aibridge-codex--normalize-items prompt))
     ((listp prompt)
      prompt)
     (t (user-error "Codex prompt must be a string or item list (got %S)" prompt)))))

(defun aibridge-codex-continue* (client conversation-id prompt cb &optional opts)
  (setq client (aibridge-codex--ensure-client client))
  (unless (and (stringp conversation-id)
               (not (string-empty-p conversation-id)))
    (user-error "Codex sendUserTurn needs a conversation id"))
  (let* ((opts (or opts (list)))
         (cwd (or (aibridge-codex--opt opts 'cwd :cwd "cwd") default-directory))
         (cwd (or (and cwd (expand-file-name cwd))
                  (user-error "Codex sendUserTurn needs cwd")))
         (sandbox (aibridge-codex--stringify
                   (or (aibridge-codex--opt opts 'sandbox :sandbox 'mode
                                            "sandbox" "sandboxPolicy" 'sandbox-policy)
                       "workspace-write")))
         (approval (aibridge-codex--stringify
                    (or (aibridge-codex--opt opts 'approvalPolicy 'approval-policy
                                             :approvalPolicy :approval-policy
                                             "approvalPolicy" "approval-policy")
                        "untrusted")))
         (model (aibridge-codex--stringify
                 (or (aibridge-codex--opt opts 'model :model "model")
                     "gpt-5")))
         (summary (aibridge-codex--stringify
                   (or (aibridge-codex--opt opts 'summary :summary "summary")
                       "auto")))
         (effort (aibridge-codex--stringify
                  (or (aibridge-codex--opt opts 'effort :effort "effort")
                      "medium")))
         (items (aibridge-codex--turn-items prompt opts)))
    (unless items (user-error "Codex sendUserTurn needs items"))
    (let* ((params `(("conversationId" . ,conversation-id)
                     ("items" . ,items)
                     ("cwd" . ,cwd)
                     ("approvalPolicy" . ,approval)
                     ("model" . ,model)
                     ("summary" . ,summary)
                     ("effort" . ,effort)))
           (params (if sandbox
                       (append params
                               (list (cons "sandboxPolicy"
                                           (list (cons "mode" sandbox)))))
                     params))
           (rid (aibridge-mcp-request (aibridge-codex-client-mcp client)
                                      "sendUserTurn" params
                                      (lambda (res) (message "[codex] sendUserTurn -> %S" res)))))
      (puthash rid (list :cb cb :cid conversation-id) aibridge-codex--streams-by-rid)
      (puthash conversation-id (list :cb cb :cid conversation-id) aibridge-codex--streams-by-cid)
      rid)))

(defun aibridge-codex-stop () (interactive)
       (when aibridge-codex--client
         (aibridge-mcp-stop aibridge-codex--client)
         (setq aibridge-codex--client nil))
       (clrhash aibridge-codex--streams-by-rid)
       (clrhash aibridge-codex--streams-by-cid))

(defun aibridge-codex--as-list (v)
  (cond ((null v) nil)
        ((vectorp v) (append v nil))
        ((listp v) v)
        (t (list v))))


;;; Tools API -------------------------------------------------------------

;;; Event mapping ---------------------------------------------------------

(defun aibridge-codex--extract-cid (params)
  (let ((msg (alist-get 'msg params)))
    (or
     ;; Preferred: envelope-level identifiers
     (alist-get 'conversationId params)             ;; camelCase
     (alist-get 'id params)                         ;; most builds
     (alist-get 'conversation_id params)            ;; snake_case

     ;; Fallbacks: occasionally carried inside msg
     (and (listp msg) (alist-get 'conversationId msg))
     (and (listp msg) (alist-get 'conversation_id msg))
     ;; NOTE: Avoid using msg.session_id unless your server
     ;;       *guarantees* it equals the conversation id.
     )))

(defun aibridge-codex--stream-for (params)
  "Find stream via _meta.requestId first, else via resolved conversation id."
  (let* ((meta (alist-get '_meta params))
         (rid  (alist-get 'requestId meta))
         (cid  (aibridge-codex--extract-cid params))
         (s-by-rid (and rid (gethash rid aibridge-codex--streams-by-rid)))
         (s-by-cid (and cid (gethash cid aibridge-codex--streams-by-cid))))
    (or s-by-rid s-by-cid)))

(defun aibridge-codex--maybe-alias-rid->cid (params)
  "If this is the first event with a rid and we know the cid, alias both maps."
  (let* ((meta (alist-get '_meta params))
         (rid  (alist-get 'requestId meta))
         (cid  (alist-get 'id params)))
    (when rid
      (let* ((s (gethash rid aibridge-codex--streams-by-rid)))
        (when (and s cid)
          (puthash cid s aibridge-codex--streams-by-cid))))))

(defun aibridge-codex--flatten-command (value)
  "Normalise VALUE into a list of command components."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   ((stringp value) (list value))
   (t (list (format "%S" value)))))

(defun aibridge-codex--fmt-arg (arg)
  "Return a display string for ARG, quoting when needed."
  (cond
   ((stringp arg)
    (if (string-match-p "[[:space:]\"]\\|'" arg)
        (shell-quote-argument arg)
      arg))
   ((numberp arg) (number-to-string arg))
   (t (format "%S" arg))))

(defun aibridge-codex--fmt-cmd (command &optional args)
  "Pretty-print COMMAND (plus optional ARGS) as a shell-like string."
  (let ((parts (append (aibridge-codex--flatten-command command)
                       (aibridge-codex--flatten-command args))))
    (mapconcat #'aibridge-codex--fmt-arg parts " ")))
(defun aibridge-codex--shell-preview (cmd cwd reason)
  "Build a sh-mode preview string for CMD (vector/list/string).
Include CWD and REASON as comments."
  (let* ((cmdline (aibridge-codex--fmt-cmd cmd))  ;; reuses your formatter
         (lines  (list "#!/usr/bin/env bash"
                       "set -euo pipefail"
                       ""
                       (when (and cwd (not (string-empty-p cwd)))
                         (format "# cwd: %s" cwd))
                       (when (and reason (not (string-empty-p reason)))
                         (format "# reason: %s" reason))
                       ""
                       (if (and cwd (not (string-empty-p cwd)))
                           (format "cd %s" (shell-quote-argument cwd))
                         "# cd <cwd>  # (not provided)")
                       cmdline
                       "")))
    (mapconcat #'identity (delq nil lines) "\n")))

(defun aibridge-codex--prompt-and-reply (call-id kind)
  "Ask y/n for CALL-ID and send the decision via the stored :reply fn.
KIND is a short human string like \"apply patch\" or \"run command\"."
  (let* ((entry (gethash call-id aibridge-codex--pending-approvals))
         (reply (plist-get entry :reply)))
    ;; If we don't yet have the reply fn (race), try again a tick later.
    (if (not reply)
        (run-at-time 0.05 nil #'aibridge-codex--prompt-and-reply call-id kind)
      (run-at-time
       0 nil
       (lambda ()
         (let* ((prompt (format "Allow Codex to %s (call %s)? " kind call-id))
                (decision (condition-case _
                              (if (y-or-n-p prompt) "allow" "deny")
                            (quit "deny"))))
           (funcall reply `(("decision" . ,decision)))
           (remhash call-id aibridge-codex--pending-approvals)))))))

(defun aibridge-codex--on-event (params)
  "Handle `codex/event` notifications."
  (aibridge-codex--maybe-alias-rid->cid params)
  (let* ((meta   (alist-get '_meta params))
         (rid    (alist-get 'requestId meta))
         (stream (aibridge-codex--stream-for params))
         (cb     (plist-get stream :cb))
         (cid    (plist-get stream :cid))
         (msg    (alist-get 'msg params))
         (type   (alist-get 'type msg)))
    (when cb
      (cond
       ;; session id (first turn)
       ((string= type "session_configured")
        (let ((cid (aibridge-codex--extract-cid params)))
          (when (and cid (stringp cid) (not (string-empty-p cid)))
            ;; alias stream by cid for future turns
            (puthash cid stream aibridge-codex--streams-by-cid)
            ;; tell the frontend
            (funcall cb (list :type 'session :conversation-id cid)))))

       ;; streaming assistant deltas (flip :saw-delta so we can skip final full message)
       ((string= type "agent_message_delta")
        (let ((delta (alist-get 'delta msg)))
          (when (and delta (not (string-empty-p delta)))
            (setq stream (plist-put stream :saw-delta t))
            (when rid (puthash rid stream aibridge-codex--streams-by-rid))
            (when cid (puthash cid stream aibridge-codex--streams-by-cid))
            (funcall cb (list :type 'chunk :text delta)))))

       ;; turn lifecycle
       ((string= type "task_started")
        (funcall cb (list :type 'status :id "started" :text "Started")))

       ((string= type "task_complete")
        (funcall cb (list :type 'done))
        ;; clean only the rid mapping; keep conversation alias for future turns
        (when rid (remhash rid aibridge-codex--streams-by-rid)))

       ;; exec_command_begin
       ((string= type "exec_command_begin")
        (let* ((call-id   (or (alist-get 'call_id msg) rid))
               (cmd       (aibridge-codex--fmt-cmd (alist-get 'command msg)
                                                   (alist-get 'args msg)))
               (cwd       (alist-get 'cwd msg))
               (status-id aibridge-codex--exec-status-id) ;; <— constant ID
               (cwd-text  (if (and cwd (not (string-empty-p cwd)))
                              (format "  (cwd %s)" cwd) ""))
               (text      (format "▶ %s%s" cmd cwd-text))
               (execs     (plist-get stream :exec)))
          ;; keep cache by call-id for the END event
          (when call-id
            (setq execs (assoc-delete-all call-id execs))
            (push (cons call-id cmd) execs)
            (setq stream (plist-put stream :exec execs))
            (when rid (puthash rid stream aibridge-codex--streams-by-rid))
            (when cid (puthash cid stream aibridge-codex--streams-by-cid)))
          (funcall cb (list :type 'status :id status-id :text text))))

       ;; exec_command_end
       ((string= type "exec_command_end")
        (let* ((call-id   (or (alist-get 'call_id msg) rid))
               (exit      (alist-get 'exit_code msg))
               (cwd       (alist-get 'cwd msg))
               (execs     (plist-get stream :exec))
               (cached    (and call-id (cdr (assoc call-id execs))))
               (cmd       (or cached
                              (aibridge-codex--fmt-cmd (alist-get 'command msg)
                                                       (alist-get 'args msg))))
               ;; drop from cache, but status id stays constant
               (execs     (and call-id (assoc-delete-all call-id execs)))
               (_         (setq stream (plist-put stream :exec execs)))
               (status-id aibridge-codex--exec-status-id) ;; <— constant ID
               (cwd-text  (if (and cwd (not (string-empty-p cwd)))
                              (format "  (cwd %s)" cwd) ""))
               ;; include exit code so the single line “flips” from ▶ to ✓/✗
               (prefix    (if (and exit (zerop exit)) "✓" "✗"))
               (text      (format "%s %s%s (exit %s)" prefix cmd cwd-text (or exit "?"))))
          (funcall cb (list :type 'status :id status-id :text text))))


       ((string= type "apply_patch_approval_request")
        (let* ((call-id (or (alist-get 'call_id msg) "unknown"))
               (count   (length (alist-get 'changes msg))))
          (funcall cb
                   (list :type 'status
                         :id   (format "approval:%s" call-id)
                         :text (format "Patch approval requested (%d file%s)"
                                       count (if (= count 1) "" "s"))))))

       ((string= type "exec_approval_request")
        (let* ((call-id (or (alist-get 'call_id msg) "unknown")))
          (funcall cb
                   (list :type 'status
                         :id   (format "approval:%s" call-id)
                         :text "Command approval requested"))))


       ;; ignore command output payloads (you only asked for the command line in status)
       ((or (string= type "exec_command_output_delta")
            (string= type "exec_command_output"))
        nil)

       ;; quiet reasoning noise (uncomment to surface)
       ((or (string= type "agent_reasoning_delta")
            (string= type "agent_reasoning")
            (string= type "agent_reasoning_section_break"))
        nil)

       ;; fallback: log to *Messages* (keeps UI clean)
       (t
        (message "[codex] unhandled event type: %s | params=%S" type params))))))

;;; Diff formatting -------------------------------------------------------
;;; --- helpers -----------------------------------------------------------


;;; --- request handlers: server -> client --------------------------------

(defun aibridge-codex--on-exec-command-approval (params reply)
  "Handle server RPC execCommandApproval."
  (let* ((call-id (or (aibridge-codex--aget params 'call_id 'callId :call_id :callId "call_id" "callId")
                      "unknown"))
         (argv    (or (aibridge-codex--aget params 'command :command "command")
                      (aibridge-codex--aget params 'cmd :cmd "cmd")))
         (cwd     (or (aibridge-codex--aget params 'cwd :cwd "cwd")
                      (aibridge-codex--aget params 'workingDir :workingDir "workingDir")))
         (reason  (aibridge-codex--aget params 'reason :reason "reason")))
    ;; Show preview buffer
    (aibridge-codex--show-exec-approval-buffer call-id argv cwd reason)
    ;; Stash wrapped reply so C-c C-c / C-c C-k work
    (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
      (setq entry (plist-put entry :reply (aibridge-codex--wrap-reply-normalize reply)))
      (setq entry (plist-put entry :command argv))
      (setq entry (plist-put entry :cwd cwd))
      (setq entry (plist-put entry :reason reason))
      (setq entry (plist-put entry :call-id call-id))
      (setq entry (plist-put entry :conversation-id
                             (aibridge-codex--stringify
                              (aibridge-codex--aget params 'conversationId 'conversation_id
                                                    :conversationId :conversation_id "conversationId" "conversation_id"))))
      (setq entry (plist-put entry :kind 'exec-command))
      (puthash call-id entry aibridge-codex--pending-approvals))))

(defun aibridge-codex--on-apply-patch-approval (params reply)
  "Handle server RPC applyPatchApproval."
  (let* ((call-id (or (aibridge-codex--aget params 'call_id 'callId :call_id :callId "call_id" "callId")
                      "unknown"))
         ;; different builds: fileChanges / changes / file_changes
         (changes (aibridge-codex--aget params 'fileChanges 'file_changes :fileChanges :file_changes "fileChanges" "file_changes" 'changes :changes "changes"))
         (norm    (aibridge-codex--normalize-change-table changes))
         (cwd     (aibridge-codex--aget params 'grantRoot 'grant_root :grantRoot :grant_root "grantRoot" "grant_root" 'cwd :cwd "cwd"))
         (reason  (aibridge-codex--aget params 'reason :reason "reason")))
    ;; Show diff buffer
    (aibridge-codex--show-approval-diff call-id norm cwd)
    ;; Stash wrapped reply
    (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
      (setq entry (plist-put entry :reply (aibridge-codex--wrap-reply-normalize reply)))
      (setq entry (plist-put entry :changes norm))
      (setq entry (plist-put entry :cwd cwd))
      (setq entry (plist-put entry :reason reason))
      (setq entry (plist-put entry :call-id call-id))
      (setq entry (plist-put entry :conversation-id
                             (aibridge-codex--stringify
                              (aibridge-codex--aget params 'conversationId 'conversation_id
                                                    :conversationId :conversation_id "conversationId" "conversation_id"))))
      (setq entry (plist-put entry :kind 'apply-patch))
      (puthash call-id entry aibridge-codex--pending-approvals))))

(defun aibridge-codex--normalize-change-table (changes)
  "Return CHANGES as an alist of (path . spec)."
  (cond
   ((null changes) nil)
   ((hash-table-p changes)
    (let (out)
      (maphash (lambda (k v) (push (cons k v) out)) changes)
      (nreverse out)))
   ((listp changes) changes)
   ((vectorp changes) (append changes nil))
   (t (list changes))))

(defun aibridge-codex--changes->diff (changes &optional cwd)
  "Build a unified diff from Codex CHANGES.
When CWD is provided, file paths are made relative when possible."
  (mapconcat
   (lambda (entry)
     (let* ((file (car entry))
            (spec (cdr entry))
            (upd  (aibridge-codex--aget spec 'update :update "update"))
            (mv   (or (aibridge-codex--aget spec 'move_path :move_path "move_path")
                     (and upd (aibridge-codex--aget upd 'move_path :move_path "move_path"))))
            (ud   (or (and upd (aibridge-codex--aget upd 'unified_diff :unified_diff "unified_diff"))
                     (aibridge-codex--aget spec 'unified_diff :unified_diff "unified_diff")))
            (rel  (condition-case _
                      (if (and (stringp cwd) (stringp file) (file-name-absolute-p file))
                          (file-relative-name file cwd)
                        file)
                   (error file)))
            (dst  (or mv rel)))
       (cond
        (ud
         ;; If server only gives hunks (starting with @@), prepend minimal headers.
         (let ((hdr (format "--- a/%s\n+++ b/%s\n" rel (or dst rel))))
           (concat hdr ud (unless (string-suffix-p "\n" ud) "\n"))))
        (mv
         (format "--- a/%s\n+++ b/%s\n@@\n# move/rename only\n" rel dst))
        (t
         (format "### Unknown change for %s ###\n" rel)))))
   changes
   "\n"))

(defun aibridge-codex--show-exec-approval-buffer (call-id cmd cwd reason)
  "Create/update (silently) the exec approval buffer; pop only from anchor."
  (let* ((bufname (format "*Codex Exec Approval: %s*" call-id))
         (buf (get-buffer-create bufname))
         (preview (aibridge-codex--shell-preview cmd cwd reason)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert preview)
        (sh-mode)
        ;; inline-approval UX
        (setq-local aibridge-codex-approval-call-id call-id)
        (setq-local aibridge-codex-approval-kind 'exec-command)
        (setq header-line-format "Approve: C-c C-c    Deny: C-c C-k")
        (aibridge-codex-approval-mode 1)
        (goto-char (point-min))
        (read-only-mode 1)))
    ;; remember buffer in pending entry so the anchor can pop it later
    (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
      (setq entry (plist-put entry :buf buf))
      (puthash call-id entry aibridge-codex--pending-approvals))
    ;; do NOT display here; anchor-follow mode will pop when appropriate
    buf))

(defun aibridge-codex--show-approval-diff (call-id changes cwd)
  "Create/update (silently) the patch approval buffer; pop only from anchor."
  (let* ((bufname (format "*Codex Patch: %s*" call-id))
         (buf (get-buffer-create bufname))
         (norm (aibridge-codex--normalize-change-table changes))
         (diff (aibridge-codex--changes->diff norm cwd)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# Codex patch request (call %s)\n" call-id))
        (when (and cwd (not (string-empty-p cwd)))
          (insert (format "# cwd: %s\n" cwd)))
        (insert "\n")
        (if (string-empty-p diff)
            (insert "# (no diff provided by server)\n")
          (insert diff))
        (diff-mode)
        ;; inline-approval UX
        (setq-local aibridge-codex-approval-call-id call-id)
        (setq-local aibridge-codex-approval-kind 'apply-patch)
        (setq header-line-format "Approve: C-c C-c    Deny: C-c C-k")
        (aibridge-codex-approval-mode 1)
        (read-only-mode 1)))
    (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
      (setq entry (plist-put entry :buf buf))
      (puthash call-id entry aibridge-codex--pending-approvals))
    ;; keep it quiet; anchor-follow will pop when you’re in the AI buffer
    buf))
;;; Approvals -------------------------------------------------------------

(defconst aibridge-codex--review-decision-values
  '("approved" "approved_for_session" "denied" "abort")
  "Allowed `ReviewDecision` values serialized as strings.")

(defun aibridge-codex--normalize-review-decision (decision)
  "Normalize DECISION into one of the supported `ReviewDecision` values."
  (let* ((s (cond
             ((symbolp decision) (downcase (symbol-name decision)))
             ((stringp decision) (downcase decision))
             ((eq decision t) "approved")
             ((null decision) "denied")
             (t (format "%s" decision)))))
    (cond
     ((member s aibridge-codex--review-decision-values) s)
     ((string-match-p "\\bapprove_for_session\\b" s) "approved_for_session")
     ((string-match-p "\\bapprove\\|allow\\b" s) "approved")
     ((string-match-p "\\babort\\b" s) "abort")
     (t "denied"))))

(defun aibridge-codex--wrap-reply-normalize (reply)
  "Wrap REPLY so that approval decisions are normalized before sending."
  (lambda (alist)
    (let* ((d (or (cdr (assoc "decision" alist))
                  (cdr (assoc 'decision alist))))
           (norm (aibridge-codex--normalize-review-decision d)))
      (funcall reply `(("decision" . ,norm))))))

(defun aibridge-codex--approval-kind-label (kind)
  "Return a human string describing KIND."
  (pcase kind
    ((or 'apply-patch :apply-patch) "apply patch")
    ((or 'exec-command :exec-command) "run command")
    (_ (format "perform %s" kind))))

(defvar aibridge-codex-login-complete-hook nil)
(defvar aibridge-codex-auth-change-hook nil)

(defvar aibridge-codex-approval-prompt-function
  #'aibridge-codex--default-approval-prompt
  "Function `(lambda (kind payload))` -> ReviewDecision.
Return a symbol or string matching `ReviewDecision` enum (`approved`,
`approved_for_session`, `denied`, `abort`).")

;;; --- NEW: single handler for both patch/exec approvals via elicitation/create ---
(defun aibridge-codex--on-elicitation-create (params reply)
  "Handle Codex approvals via MCP `elicitation/create` request.
`params` carries Codex-specific fields:
  - codex_elicitation: \"patch-approval\" | \"exec-approval\"
  - codex_call_id: unique call id
  - For patch: codex_changes (map of file -> {update:{unified_diff}, move_path})
  - For exec: codex_command (argv), codex_cwd (str|nil), codex_reason (str|nil)
We open an approval buffer and save REPLY to be triggered by C-c C-c / C-c C-k."
  (let* ((kind   (aibridge-codex--aget params 'codex_elicitation :codex_elicitation "codex_elicitation"))
         (call-id (or (aibridge-codex--aget params 'codex_call_id :codex_call_id "codex_call_id") "unknown")))
    (pcase kind
      ("patch-approval"
        (let* ((changes (aibridge-codex--normalize-change-table
                          (aibridge-codex--aget params 'codex_changes :codex_changes "codex_changes")))
                (cwd     (or (aibridge-codex--aget params 'codex_grant_root :codex_grant_root "codex_grant_root")
                             (aibridge-codex--aget params 'codex_cwd :codex_cwd "codex_cwd"))))
         ;; show diff buffer with minor-mode that binds C-c C-c / C-c C-k
         (aibridge-codex--show-approval-diff call-id changes cwd)
         ;; stash reply so keybindings can answer
         (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
            (setq entry (plist-put entry :reply (aibridge-codex--wrap-reply-normalize reply)))
            (setq entry (plist-put entry :changes changes))
           (setq entry (plist-put entry :cwd cwd))
           (setq entry (plist-put entry :call-id call-id))
           (setq entry (plist-put entry :kind 'apply-patch))
           (puthash call-id entry aibridge-codex--pending-approvals))))

      ("exec-approval"
       (let* ((argv   (aibridge-codex--aget params 'codex_command :codex_command "codex_command"))
              (cwd    (aibridge-codex--aget params 'codex_cwd :codex_cwd "codex_cwd"))
              (reason (aibridge-codex--aget params 'codex_reason :codex_reason "codex_reason")))
         ;; show sh-mode preview buffer with approval minor-mode
         (aibridge-codex--show-exec-approval-buffer call-id argv cwd reason)
         ;; stash reply so keybindings can answer
         (let* ((entry (or (gethash call-id aibridge-codex--pending-approvals) (list))))
           (setq entry (plist-put entry :reply (aibridge-codex--wrap-reply-normalize reply)))
           (setq entry (plist-put entry :command argv))
           (setq entry (plist-put entry :cwd cwd))
           (setq entry (plist-put entry :reason reason))
           (setq entry (plist-put entry :call-id call-id))
           (setq entry (plist-put entry :kind 'exec-command))
           (puthash call-id entry aibridge-codex--pending-approvals))))

      (_
       ;; Unknown elicitation kind: deny by default (or you can prompt)
       (message "[codex] unknown elicitation kind: %S (call %s)" kind call-id)
       (funcall reply '(("decision" . "denied")))))))

(provide 'aibridge-codex)
