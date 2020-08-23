(defun acrepl-doc-expr-at-point ()
  "Evaluate clojure.repl/doc on current expression."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (let* ((expr (buffer-substring start end))
	    (to-send (format "(do (require 'clojure.repl) (clojure.repl/doc %s))" expr)))
        (acrepl-send-code to-send)))))

(defun acrepl-bean-expr-at-point ()
  "Evaluate cljs-bean.core/bean on current expression."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (let* ((expr (buffer-substring start end))
	    (to-send (format "(do (require 'cljs-bean.core) (require 'clojure.pprint) (clojure.pprint/pprint (cljs-bean.core/bean %s)))" expr)))
        (acrepl-send-code to-send)))))

(defun acrepl-pprint-last-result ()
  (interactive)
  (acrepl-send-code
   "(do (require 'clojure.pprint) (clojure.pprint/pp))"))

(provide 'acrepl-repl-util)

;;; acrepl-repl-util.el ends here
