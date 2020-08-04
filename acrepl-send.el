;;; acrepl-send.el --- sending -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-bounds)
(require 'acrepl-guess)

(require 'comint)

(defun acrepl-send-code (code-str)
  "Send CODE-STR.
CODE-STR should be a Clojure form."
  (interactive "sCode: ")
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (let ((here (point))
            (original-buffer (current-buffer)))
        ;; switch to acrepl buffer to prepare for appending
        (set-buffer repl-buffer)
        (goto-char (point-max))
        ;; (comint-send-string
        ;;  (get-buffer-process (current-buffer))
        ;;  (format "%s" code-str))
        (goto-char (point-max))
        (insert code-str)
        (comint-send-input)
        (set-buffer original-buffer)
        (if (eq original-buffer repl-buffer)
            (goto-char (point-max))
          (goto-char here))))))

(defun acrepl-send-hidden-code (code-str)
  "Send CODE-STR.
CODE-STR should be a Clojure form."
  (interactive "sCode: ")
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (process-send-string repl-buffer code-str))))

(defvar acrepl-listeners (make-hash-table :test 'equal))

(defun acrepl-send-code-with-callback (code-str callback)
  "Send CODE-STR.
CODE-STR should be a Clojure form."
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (let ((here (point))
            (original-buffer (current-buffer)))
        ;; switch to acrepl buffer to prepare for appending
        (set-buffer repl-buffer)
        (goto-char (point-max))
        ;; (comint-send-string
        ;;  (get-buffer-process (current-buffer))
        ;;  (format "%s" code-str))
        (goto-char (point-max))
        (let ((id (symbol-name (cl-gensym "acrepl-eval"))))
          (puthash id (list original-buffer callback) acrepl-listeners)
          (process-send-string repl-buffer (format "(try {:tag :ret :id \"%s\" :result (pr-str (do %s))} (catch #?(:cljs js/Error :clj Exception) e {:id \"%s\" :error (pr-str e)}))
" id code-str id))
          ;;          (insert (format "(try {:tag :ret :id \"%s\" :result (pr-str (do %s))} (catch js/Error e {:id \"%s\" :error (pr-str e)}))" id code-str id))
          )
        (set-buffer original-buffer)
        (if (eq original-buffer repl-buffer)
            (goto-char (point-max))
          (goto-char here))))))

(defun acrepl-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (buffer-substring-no-properties start end)))

(defun acrepl-send-expr-at-point ()
  "Send expression at point.
Optional arg IGNORE-UNEVAL, if non-nil, does not send a leading uneval (#_)."
  (interactive "P")
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-send-region
       start
       end))))

(defun acrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (acrepl-send-region (point-min) (point-max)))

(defun acrepl-send-expr-at-point (&optional ignore-uneval)
  "Send expression at point.
Optional arg IGNORE-UNEVAL, if non-nil, does not send a leading uneval (#_)."
  (interactive "P")
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-send-region
       (if (and ignore-uneval
                (string-equal "#_"
                              (buffer-substring-no-properties start
                                                              (+ start 2))))
           (+ 2 start)
         start)
       end))))

;; Saikyun's additions
(defun acrepl-send-defun ()
  "Figure out top-level expression and send it to evaluation."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (acrepl-send-region (point) end))))

(defun acrepl-send-wrapping-sexp ()
  "Sends the expression wrapping the point."
  (interactive)
  (let* ((pt (point))
         (prev (start-of-sexp pt)))
    (when prev
      (let ((next (end-of-sexp prev)))
        (when next
          (acrepl-send-region prev next))))))

(provide 'acrepl-send)

;;; acrepl-send.el ends here
