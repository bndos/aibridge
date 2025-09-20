(require 'avy)
(require 'project)

(defvar aibridge-path-regexp
  "\\([~/\\.[:alnum:]_@#$/-]+\\.[[:alnum:]_]+\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?"
  "Matches file paths with optional :LINE[:COL]. 1=path 2=line 3=col.")

(defun aibridge--project-root ()
  (or (when-let ((pr (project-current))) (project-root pr))
      default-directory))

(defun aibridge--open-path-at (pos)
  "Open path at POS and jump to optional line/col."
  (save-excursion
    (goto-char pos)
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (when (re-search-forward aibridge-path-regexp eol t)
        (let* ((path (match-string 1))
               (line (and (match-string 2) (string-to-number (match-string 2))))
               (col  (and (match-string 3) (string-to-number (match-string 3))))
               (root (aibridge--project-root))
               (abs  (if (file-name-absolute-p path)
                         path
                       (expand-file-name path root))))
          (find-file abs)
          (when (and line (> line 0))
            (goto-char (point-min))
            (forward-line (1- line)))
          (when (and col (> col 0))
            (move-to-column (1- col) t))
          t)))))

(defun aibridge-avy-open-path ()
  "Use avy to select a path like foo/bar.py:12:4 and open it at that spot."
  (interactive)
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward aibridge-path-regexp nil t)
        ;; Use the match beginning for a stable hint position
        (push (match-beginning 0) positions)))
    (setq positions (nreverse positions))
    (cond
     ((null positions)
      (user-error "No file paths found"))
     (t
      (let ((pos (avy-with aibridge-avy-open-path
                   (avy-process positions))))
        (unless (aibridge--open-path-at pos)
          (user-error "Failed to open path here")))))))

;; Optional: convenient bindings
;; Doom users: bind in your chat/markdown modes (adjust keys to taste)
(with-eval-after-load 'org-mode
  (define-key org-mode-map (kbd "C-c l") #'aibridge-avy-open-path))

;; If you have a custom aibridge mode map, also add:
;; (define-key aibridge-term-mode-map (kbd "C-c C-l") #'aibridge-avy-open-path)
