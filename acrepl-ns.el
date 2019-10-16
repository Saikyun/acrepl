(require 'rx)
(require 'acrepl-send)

(defconst acrepl-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n" ""))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n" "")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))


(defun acrepl-find-ns ()
  "Return the namespace of the current Clojure buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer.

This has been taken from `clojure-mode` and modified to handle the
mixed newlines of the clojure core packages."
  (let ((ns (save-excursion
              (save-restriction
                (widen)
                
                ;; Move to top-level to avoid searching from inside ns
                (ignore-errors (while t (up-list nil t t)))
                
                ;; The closest ns form above point.
                (when (or (re-search-backward acrepl-namespace-name-regex nil t)
                          ;; Or any form at all.
                          (and (goto-char (point-min))
                               (re-search-forward acrepl-namespace-name-regex nil t)))
                  (match-string-no-properties 4))))))
    ns))

(defun acrepl-set-ns ()
  "Tries to evaluate Clojure ns form. It does this by matching first
expression at the beginning of the file and evaluating it. Not something
that is 100% accurate, but Clojure practice is to keep ns forms always
at the top of the file."
  (interactive)
  (when-let (ns (acrepl-find-ns))
    (acrepl-send-code (format "(in-ns '%s)" ns))    
    ;; (save-excursion
    ;;   (goto-char (match-beginning 0))
    ;;   (acrepl-send-ascertained-region))
    ))

(provide 'acrepl-ns)
