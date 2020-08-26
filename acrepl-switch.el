;;; acrepl-switch.el --- switch -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-guess)

(defun acrepl-switch-to-repl ()
  "Try to switch to a relevant repl buffer from code buffer."
  (interactive)
  (let* ((repl-buffer (acrepl-guess-repl-buffer)))
    (when (not repl-buffer)
      (error "Did not find repl buffer.  May be no connection?"))
    (switch-to-buffer-other-window repl-buffer)))

(provide 'acrepl-switch)

;;; acrepl-switch.el ends here
