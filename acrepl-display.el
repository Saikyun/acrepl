(require 'comint)

(defun acrepl-display-last-output-in-frame ()
  "Display last output in a frame.

Idea for use: use `zprint' interactively on resulting buffer content."
  (interactive)
  (let ((repl-buffer (acrepl-guess-repl-buffer))
        output)
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      ;; see comint.el
      (save-excursion
        (with-current-buffer repl-buffer
          (goto-char (process-mark (get-buffer-process repl-buffer)))
          (forward-line 0)
          (setq output
                (buffer-substring-no-properties comint-last-input-end
                                                (point)))))
      (select-frame (make-frame))
      (let ((display-buffer (get-buffer-create
                             (generate-new-buffer
                              (format-time-string
                               "acrepl-%Y-%m-%d_%H:%M:%S")))))
        (pop-to-buffer display-buffer)
        (delete-other-windows)
        (insert output)))))

(provide 'acrepl-display)
