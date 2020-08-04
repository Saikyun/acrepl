(defun acrepl-doc-expr-at-point ()
  "Evaluate clojure.repl/doc on current expression."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (let* ((expr (buffer-substring start end))
	    (to-send (format "(do (require 'clojure.repl) (clojure.repl/doc %s))" expr)))
        (acrepl-send-code to-send)))))

(provide 'acrepl-repl-util)

;;; acrepl-repl-util.el ends here
