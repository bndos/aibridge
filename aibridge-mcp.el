;;; aibridge-mcp.el --- Minimal MCP (JSON-RPC over stdio, line-delimited) -*- lexical-binding: t; -*-
;; Author: AI Bridge
;; SPDX-License-Identifier: MIT
;;
;; Transport-only client for MCP-style servers (Codex, etc.).
;; - One JSON object per line (no Content-Length framing).
;; - Generic JSON-RPC 2.0 request/response + notifications + server->client requests.
;;
;; Public API:
;;   (aibridge-mcp-start COMMAND) -> CLIENT
;;   (aibridge-mcp-stop CLIENT)
;;   (aibridge-mcp-request CLIENT METHOD PARAMS CB)   ; CB gets (:ok result) or (:error err)
;;   (aibridge-mcp-notify  CLIENT METHOD PARAMS)
;;   (aibridge-mcp-register-notify  CLIENT METHOD FN) ; notifications
;;   (aibridge-mcp-register-request CLIENT METHOD FN) ; server->client requests
;;   (aibridge-mcp-unregister-notify  CLIENT METHOD)
;;   (aibridge-mcp-unregister-request CLIENT METHOD)
;;
;; See also: aibridge-codex.el for Codex-specific bindings.

(require 'json)
(require 'cl-lib)

(defgroup aibridge-mcp nil
  "Minimal MCP JSON-RPC transport over line-delimited stdio."
  :group 'tools)

(defcustom aibridge-mcp-log-messages t
  "When non-nil, echo raw lines and decoded messages to *Messages* for debugging."
  :type 'boolean)

(cl-defstruct aibridge-mcp
  "Opaque client handle for an MCP/JSON-RPC stdio process."
  proc                 ;; process object
  buf                  ;; stdout buffer (for debugging)
  errbuf               ;; stderr buffer (for debugging)
  rxbuf                ;; partial line accumulator (string)
  next-id              ;; numeric request id counter
  pending              ;; hash: id (eql) -> callback
  notify-handlers      ;; hash: method (string) -> fn(params)
  request-handlers)    ;; hash: method (string) -> fn(params reply-fn)

;;;; Process lifecycle ----------------------------------------------------
;;; --- Add these near the top (after defcustoms) -------------------------

(defvar aibridge-mcp-request-hook nil
  "Hook run for every server->client JSON-RPC request.
Each function is called as (FN CLIENT METHOD PARAMS ID).")

(defvar aibridge-mcp-notify-hook nil
  "Hook run for every server notification.
Each function is called as (FN CLIENT METHOD PARAMS).")

(defvar aibridge-mcp-response-hook nil
  "Hook run for every server response to *our* requests.
Each function is called as (FN CLIENT ID RESULT ERROR RAW-MSG).")

;;; Optional: a quick logger you can enable with (aibridge-mcp-install-logger)
(defun aibridge-mcp-install-logger ()
  "Install a lightweight logger that prints all MCP traffic."
  (interactive)
  (add-hook 'aibridge-mcp-request-hook
            (lambda (_client method params id)
              (message "[mcp ▶ req] id=%s method=%s params=%S" id method params)))
  (add-hook 'aibridge-mcp-notify-hook
            (lambda (_client method params)
              (message "[mcp ▶ not] method=%s params=%S" method params)))
  (add-hook 'aibridge-mcp-response-hook
            (lambda (_client id result error _raw)
              (message "[mcp ◀ res] id=%s %s=%S"
                       id (if error "error" "result") (or error result)))))

(defun aibridge-mcp-start (command &optional default-directory)
  "Start an MCP process COMMAND (list of strings).
Returns a CLIENT struct to pass to other functions."
  (unless (and (listp command) (stringp (car command)))
    (user-error "COMMAND must be a list of program and args"))
  (let* ((buf   (generate-new-buffer "*aibridge-mcp*"))
         (err   (generate-new-buffer "*aibridge-mcp-stderr*"))
         (client (make-aibridge-mcp
                  :buf buf :errbuf err :rxbuf "" :next-id 0
                  :pending (make-hash-table :test 'eql)
                  :notify-handlers (make-hash-table :test 'equal)
                  :request-handlers (make-hash-table :test 'equal))))
    (setf (aibridge-mcp-proc client)
          (apply #'make-process
                 :name "aibridge-mcp"
                 :buffer buf
                 :command command
                 :coding 'utf-8
                 :connection-type 'pipe
                 :stderr err
                 :noquery t
                 :filter (lambda (_p chunk) (aibridge-mcp--filter client chunk))
                 :sentinel (lambda (_ ev) (aibridge-mcp--sentinel client ev))
                 nil))
    (when default-directory
      (with-current-buffer buf
        (setq-local default-directory default-directory)))
    client))

(defun aibridge-mcp-stop (client)
  "Stop CLIENT process (if alive)."
  (when (and client (process-live-p (aibridge-mcp-proc client)))
    (delete-process (aibridge-mcp-proc client))))

;;;; Registration API -----------------------------------------------------

(defun aibridge-mcp-register-notify (client method fn)
  "Register notification handler FN for METHOD (string)."
  (puthash method fn (aibridge-mcp-notify-handlers client)))

(defun aibridge-mcp-register-request (client method fn)
  "Register server->client request handler FN for METHOD.
FN is called as (FN PARAMS REPLY), where (REPLY RESULT &optional IS-ERROR)
must be called exactly once."
  (puthash method fn (aibridge-mcp-request-handlers client)))

(defun aibridge-mcp-unregister-notify (client method)
  "Remove notification handler for METHOD."
  (remhash method (aibridge-mcp-notify-handlers client)))

(defun aibridge-mcp-unregister-request (client method)
  "Remove server->client request handler for METHOD."
  (remhash method (aibridge-mcp-request-handlers client)))

;;;; JSON-RPC send --------------------------------------------------------

(defun aibridge-mcp--send (client obj)
  "Encode OBJ as JSON and send it as a single line."
  (interactive)
  (message "aibridge-mcp--send %s" obj)
  (let ((txt (json-encode obj)))
    (when aibridge-mcp-log-messages
      (message "[mcp->] %s" txt))
    (process-send-string (aibridge-mcp-proc client) (concat txt "\n"))))

(defun aibridge-mcp--normalize-params (params)
  "Ensure empty params encode as {} (not []). Keep arrays/lists when explicit."
  (cond
   ;; already an object → keep
   ((hash-table-p params) params)
   ;; explicit empty → make {}
   ((null params) (make-hash-table :test 'equal))
   ;; empty list is ambiguous in JSON-RPC; default to {} for MCP servers
   ((and (listp params) (null (cdr params)) (null (car params)))
    (make-hash-table :test 'equal))
   ;; otherwise pass through (non-empty alist, plist, vector, or positional list)
   (t params)))

(defun aibridge-mcp-request (client method params cb)
  "Send JSON-RPC request METHOD with PARAMS (alist/plist/hash/array/list).
CB is called with the server's response object. Returns the numeric id."
  (message "aibridge-mcp-request %s %s" method params)
  (let* ((id (cl-incf (aibridge-mcp-next-id client)))
         (obj `(("jsonrpc" . "2.0")
                ("id" . ,id)
                ("method" . ,method)
                ("params" . ,(aibridge-mcp--normalize-params params)))))
    (puthash id cb (aibridge-mcp-pending client))
    (aibridge-mcp--send client obj)
    id))

(defun aibridge-mcp-notify (client method params)
  "Send JSON-RPC notification METHOD with PARAMS."
  (aibridge-mcp--send client
                      `(("jsonrpc" . "2.0")
                        ("method" . ,method)
                        ("params" . ,(or params (list))))))

;;;; Incoming data / dispatch --------------------------------------------

(defun aibridge-mcp--filter (client chunk)
  "Line-delimited reader: accumulate into rxbuf; dispatch full JSON lines."
  (setf (aibridge-mcp-rxbuf client) (concat (aibridge-mcp-rxbuf client) chunk))
  (let ((buf (aibridge-mcp-rxbuf client))
        (start 0)
        (len (length (aibridge-mcp-rxbuf client))))
    (cl-loop for nl = (cl-position ?\n buf :start start)
             while nl
             do (let ((line (substring buf start nl)))
                  (setq start (1+ nl))
                  (unless (string-blank-p line)
                    (when aibridge-mcp-log-messages
                      (message "[mcp<-] %s" line))
                    (condition-case err
                        (aibridge-mcp--dispatch
                         client
                         (json-parse-string line :object-type 'alist :null-object nil))
                      (error (message "[mcp] parse error: %S | line=%S" err line)))))
             finally (setf (aibridge-mcp-rxbuf client)
                           (if (< start len) (substring buf start) "")))))

(defun aibridge-mcp--sentinel (_client ev)
  (message "[mcp] %s" (string-trim ev)))

(defun aibridge-mcp--dispatch (client msg)
  "Dispatch a decoded JSON-RPC object MSG."
  (let* ((id     (alist-get 'id msg))
         (method (alist-get 'method msg))
         (result (alist-get 'result msg))
         (error  (alist-get 'error msg))
         (params (alist-get 'params msg)))
    (cond
     ;; JSON-RPC REQUEST (server -> client)
     ((and method id (not result) (not error))
      ;; Tap logger: does not consume the message
      (run-hook-with-args 'aibridge-mcp-request-hook client method params id)
      (let* ((handler (gethash method (aibridge-mcp-request-handlers client))))
        (if (not (functionp handler))
            (aibridge-mcp--send client
                                `(("jsonrpc" . "2.0")
                                  ("id" . ,id)
                                  ("error" . (("code" . -32601)
                                              ("message" . ,(format "Method not found: %s" method))))))
          (funcall handler params
                   (lambda (res &optional is-error)
                     (aibridge-mcp--send
                      client
                      (if is-error
                          `(("jsonrpc" . "2.0") ("id" . ,id) ("error" . ,(or res (list))))
                        `(("jsonrpc" . "2.0") ("id" . ,id) ("result" . ,(or res (list)))))))))))

     ;; JSON-RPC NOTIFICATION
     ((and method (not id))
      (run-hook-with-args 'aibridge-mcp-notify-hook client method params)
      (let ((handler (gethash method (aibridge-mcp-notify-handlers client))))
        (when (functionp handler)
          (funcall handler params))))

     ;; JSON-RPC RESPONSE (to our earlier request)
     ((and id (not method))
      (run-hook-with-args 'aibridge-mcp-response-hook client id result error msg)
      (let ((cb (gethash id (aibridge-mcp-pending client))))
        (remhash id (aibridge-mcp-pending client))
        (when (functionp cb)
          (funcall cb (if error (list :error error) (list :ok result))))))

     (t
      (message "[mcp] ignoring message: %S" msg)))))

(provide 'aibridge-mcp)
;;; aibridge-mcp.el ends here
