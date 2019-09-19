;;; acrepl-send.el --- sending -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-bounds)

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
        (insert code-str)
        (comint-send-input)
        (set-buffer original-buffer)
        (if (eq original-buffer repl-buffer)
            (goto-char (point-max))
          (goto-char here))))))

(defun acrepl-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (buffer-substring-no-properties start end)))

(defun acrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (acrepl-send-region (point-min) (point-max)))

(defun acrepl-send-expr-at-point ()
  "Send expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-send-region start end))))

(provide 'acrepl-send)

;;; acrepl-send.el ends here