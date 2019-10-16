(defvar acrepl-shadow-cljs-in-repl nil)

(defun acrepl-shadow-cljs-start-repl
    ()
  (interactive)
  (progn (acrepl-send-code "(shadow/repl :app)")
	 (setq acrepl-shadow-cljs-in-repl t)))

(defun acrepl-shadow-cljs-exit-repl
    ()
  (interactive)
  (if acrepl-shadow-cljs-in-repl
      (progn (acrepl-send-code ":cljs/quit")
	     (setq acrepl-shadow-cljs-in-repl nil))
    (message "Not in cljs-repl.")))

(defun acrepl-shadow-cljs-stop-worker
    ()
  (interactive)
  (acrepl-send-code "(shadow.cljs.devtools.api/stop-worker :app)"))

(defun acrepl-shadow-cljs-watch-without-autobuild
    ()
  (interactive)
  (acrepl-send-code "(shadow.cljs.devtools.api/watch :app {:autobuild false})"))

(defun acrepl-shadow-cljs-watch-compile
    ()
  (interactive)
  (if acrepl-shadow-cljs-in-repl
      (progn (acrepl-shadow-cljs-exit-repl)
	     (acrepl-send-code "(shadow.cljs.devtools.api/watch-compile! :app)")
	     (acrepl-shadow-cljs-start-repl))
    (acrepl-send-code "(shadow.cljs.devtools.api/watch-compile! :app)")))


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


(defvar acrepl-project-dir)
(defvar acrepl-shadow-cljs-server-buffer-name "*acrepl-shadow-cljs-server*")
(defvar acrepl-project-file "shadow-cljs.edn")

(defun acrepl-set-project-dir ()
  "Sets `acrepl-project-dir` to the project directory."
  (interactive)
  (setq acrepl-project-dir
        (locate-dominating-file default-directory
                                acrepl-project-file)))

(defun acrepl-start-shadow-cljs-server ()
  (interactive)
  (if (get-buffer acrepl-shadow-cljs-server-buffer-name)
      (message "`shadow-cljs server` is already running.")
    (let ((default-directory (or acrepl-project-dir default-directory)))
      (async-shell-command "shadow-cljs server" acrepl-shadow-cljs-server-buffer-name)
      (message "Started `shadow-cljs server` in dir %s" default-directory))))

(declare acrepl-start-shadow-cljs-server-and-connect)


    (define-key map "\C-c\C-l" 'acrepl-load-file)
    (define-key map "\C-c\C-n" 'acrepl-set-ns)

    (define-key map "\C-c\C-g\C-c" 'acrepl-shadow-cljs-watch-compile)
(define-key map "\C-c\C-g\C-s" 'acrepl-start-shadow-cljs-server-and-connect)
(define-key map "\C-c\C-g\C-w\C-w" 'acrepl-shadow-cljs-watch-without-autobuild)

	"--"
	["Eval buffer's ns-form" acrepl-set-ns t]


    (define-key map "\C-c\C-i" 'acrepl-load-file)
    (easy-menu-define acrepl-mode-map map
      "A Clojure REPL Mode Menu"
      '("ACRepl"
        ["Load file" acrepl-load-file t]))

   (setq-local comment-start ";")

(declare acrepl)
(defun acrepl-repeat-connect (&optional done times)
  (let ((times (or times 60))
	(endpoint (acrepl-guess-endpoint)))
    (if (<= times 0)
        (message "Couldn't connect to `shadow-cljs server` after 60 seconds.")
      (if endpoint
	  (condition-case nil
              (progn (acrepl endpoint)
		     (when done
		       (funcall done)))
            (error (progn (message "Connecting to `shadow-cljs server`...")
			  (run-at-time "1 sec" nil 'acrepl-repeat-connect done (- times 1)))))
	(progn (message "Connecting to `shadow-cljs server`...")
               (run-at-time "1 sec" nil 'acrepl-repeat-connect done (- times 1)))))))

(defun acrepl-start-shadow-cljs-server-and-connect
    ()
  (interactive)
  (acrepl-set-project-dir)
  (acrepl-start-shadow-cljs-server)
  (acrepl-repeat-connect (lambda ()
			   (acrepl-shadow-cljs-watch-without-autobuild)
			   (acrepl-shadow-cljs-start-repl)
			   (async-shell-command "yarn start --web" "*yarn start --web*"))))

(defun acrepl-stop-and-start-worker
    ()
  (interactive)
  (acrepl-shadow-cljs-exit-repl)
  (acrepl-shadow-cljs-stop-worker)
  (acrepl-shadow-cljs-watch-without-autobuild)
  (acrepl-shadow-cljs-start-repl))


  (unless
      					;(ignore-errors ;; XXX: uncomment at some point...
      (let* ((ep (split-string endpoint ":"))
             (host (car ep))
             (port (string-to-number (cadr ep))))
        (message "Connecting to socket REPL on '%s:%d'..." host port)
	(acrepl-interaction-mode t)
        (with-current-buffer (get-buffer-create acrepl-repl-buffer-name)
          (prog1
              (make-comint-in-buffer "acrepl" acrepl-repl-buffer-name
                                     (cons host port))
            (goto-char (point-max))
            (acrepl-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max)))))
     (message "Failed to connect to %s" endpoint)))
