(require 'acrepl-send)
(require 'acrepl-guess)
(require 'acrepl-interaction)

(defvar acrepl-miracle-save-contexts (make-hash-table :test 'equal))

(defun acrepl-find-miracle-save
    ()
  "Looks backwards from point and tries to find `save` or `save-do` calls. Sadly it's pretty stupid and will accept any form that looks like this, regardless of wether it's correct or not."
  (interactive)
  (setq-local found nil)
  (save-excursion
    (let ((min-point (save-excursion
                       (beginning-of-defun)
                       (point))))
      (while (and (equal found nil)
                  (> (point) min-point))
        (paredit-backward)
        (save-excursion
          (paredit-forward-down)
          (let ((start (point)))
            (paredit-forward)
            (let* ((end (point))
                   (text (buffer-substring start end)))
              (when (or (equal text "save-do")
                        (equal text "save"))
                (paredit-forward)
                (let ((other-end (point)))
                  (paredit-backward)
                  (let ((other-start (point)))
                    
                    (setq found (buffer-substring other-start other-end)))))))))
      (if found
          found
        (progn (message "Couldn't find any saves.")
               nil)))))

(defun acrepl-miracle-send-expr (expr id pos)
  "Evaluate `EXPR` in miracle.save in miracle.save context specified by `ID` and `POS`."
  (interactive "sMiracle save id: ")
  (let ((to-send (format "(miracle.save/eval-in-context '(do %s) %s %s)" expr id pos)))
    (acrepl-send-code-with-callback
     to-send
     (lambda (res)
       (acrepl-send-hidden-code res)))))

(defun acrepl-miracle-send-expr-current-context (expr)
  "Evaluate `EXPR` in miracle.save in current miracle.save context."
  (interactive "sMiracle save id: ")
  (if-let (id-pos (gethash (acrepl-guess-repl-buffer) acrepl-miracle-save-contexts))
      (acrepl-miracle-send-expr expr (first id-pos) (second id-pos))
    (message "No miracle context set. Run `acrepl-set-miracle-save-context` first")))

(defun acrepl-miracle-send-expr-at-point (id pos)
  "Evaluate expression at pointer in miracle.save context specified by `ID` and `POS`."
  (interactive "sMiracle save id: ")
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (let ((expr (buffer-substring start end)))
        (acrepl-miracle-send-expr expr id pos)))))

(defun acrepl-miracle-send-expr-at-point-current-context ()
  "Evaluate expression at pointer in the current miracle.save context."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (let ((expr (buffer-substring start end)))
        (acrepl-miracle-send-expr-current-context expr)))))

(defun acrepl-miracle-send-wrapping-sexp-current-context ()
  "Evaluate the expression wrapping the pointer in the current miracle.save context."
  (interactive)
  (let* ((pt (point))
         (prev (start-of-sexp pt)))
    (when prev
      (let ((next (end-of-sexp prev)))
        (when next
	  (acrepl-miracle-send-expr-current-context (buffer-substring prev next)))))))

(defun acrepl-miracle-set-context
    (id pos &optional data display-data)
  (puthash (acrepl-guess-repl-buffer)
           (list id pos display-data)
           acrepl-miracle-save-contexts)                 
  (message "Miracle context set to %s / %s - Data: %s" id pos
           (when data
             (replace-regexp-in-string "\n" "" data))))

;; (replace-regexp-in-string "\#" "\"\"" "hej#")

(defun acrepl-set-miracle-save-context-latest ()
  (interactive)
  (lexical-let ((save-id (acrepl-find-miracle-save)))
    (if save-id
        (progn (acrepl-send-code-with-callback
                (format "(miracle.save/get-last %s)" save-id)
                (lambda (res)
                  (message "HAHA")
                  (let* ((res (replace-regexp-in-string
                               "\#" "\"\"" res))
                         (parsed (edn-read res))
                         (pos (aref parsed 0))
                         (data (aref parsed 1)))
                    (acrepl-miracle-set-context save-id pos (edn-read (replace-regexp-in-string "\#" "\"\"" data)) data)))))
      (message "Couldn't find a save id."))))

(defun acrepl-set-miracle-save-context
    ()
  (interactive)
  (lexical-let ((save-id (acrepl-find-miracle-save)))
    (if save-id
        (progn
          (acrepl-send-code-with-callback
           (format "(do (require '[miracle.save]) (miracle.save/get-last-nof %s 10))" save-id)
           (lambda (res)
             (message "omguh")
             (let* ((parsed (edn-read res))
                    (cands (map 'list (lambda (x) (format "%s\n%s" (aref x 0)
                                                          (replace-regexp-in-string
                                                           "\\\""
                                                           ""
                                                           (replace-regexp-in-string
                                                            "\\\\\""
                                                            "\""
                                                            (replace-regexp-in-string
                                                             "\\\\n"
                                                             "\n"
                                                             (aref x 1)))))) parsed)))
               (setq lol parsed)
               (message "%s" (second parsed))
               
               (when-let* ((chosen (helm :sources (helm-build-sync-source "test"
                                                    :candidates cands
                                                    :multiline 500)
                                         :buffer "*helm my command*"))
                           (pos-to-split (string-match " \\|\\\n" chosen))
                           (pos (substring chosen 0 pos-to-split))
                           (data (substring chosen (+ 1 pos-to-split))))
                 (acrepl-miracle-set-context save-id pos data chosen))))))
      (message "Couldn't find a save id."))))

(defun acrepl-display-miracle-context-wrapper (where-fn)
  (interactive)
  (if-let ((context (gethash (acrepl-guess-repl-buffer) acrepl-miracle-save-contexts)))
      (let ((data (third context))
            (display-buffer (get-buffer-create
                             (generate-new-buffer
                              (format-time-string
                               "acrepl-miracle-%Y-%m-%d_%H:%M:%S")))))
        (funcall where-fn display-buffer)
        (insert (format "%s" data))
        (run-hooks 'acrepl-display-last-output-hook))
    (message "No miracle context set. Run `acrepl-set-miracle-save-context` first")))

(defun acrepl-display-miracle-context-in-window ()
  "Display miracle context in current window."
  (interactive)
  (acrepl-display-miracle-context-wrapper
   (lambda (buffer)
     (pop-to-buffer-same-window buffer))))

(defun acrepl-display-miracle-context-in-other-window ()
  "Display miracle context in other window."
  (interactive)
  (acrepl-display-miracle-context-wrapper
   (lambda (buffer)
     (switch-to-buffer-other-window buffer))))

(defun acrepl-display-miracle-context-in-frame ()
  "Display miracle context in a frame."
  (interactive)
  (acrepl-display-miracle-context-wrapper
   (lambda (buffer)
     (select-frame (make-frame))
     (pop-to-buffer buffer)
     (delete-other-windows))))

(define-key acrepl-interaction-mode-map "\C-c\C-o\C-s" 'acrepl-set-miracle-save-context)
(define-key acrepl-interaction-mode-map "\C-c\C-o\C-w" 'acrepl-miracle-send-wrapping-sexp-current-context)
(define-key acrepl-interaction-mode-map "\C-c\C-o\C-e" 'acrepl-miracle-send-expr-at-point-current-context)
(define-key acrepl-interaction-mode-map "\C-c\C-o\C-d" 'acrepl-display-miracle-context-in-other-window)
(define-key acrepl-interaction-mode-map "\C-c\C-o\C-l" 'acrepl-set-miracle-save-context-latest)


(provide 'acrepl-miracle)

;;; acrepl-miracle.el ends here
