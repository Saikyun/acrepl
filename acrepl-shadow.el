;;; acrepl-shadow.el --- shadow-cljs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Usage:
;;
;;  Convenient connection to socket repl for shadow-cljs, and optionally
;;  auto-reconnect upon shadow-cljs restart:
;;
;;    (require 'acrepl-shadow)
;;    ;; nil or don't set if you don't want auto reconnection
;;    (setq acrepl-shadow-auto-reconnect t)
;;
;;    M-x acrepl-shadow-connect
;;
;;  For auto-detection of project for use with M-x acrepl, try:
;;
;;    (add-hook 'acrepl-project-type-hook
;;              'acrepl-shadow-cljs-type-p)
;;
;;  For single connection per project behavior, try:
;;
;;    (add-hook 'acrepl-guess-repl-hook
;;              'acrepl-shadow-reuse-same-project-repl)
;;
;;  To disable if already enabled, try:
;;
;;    (remove-hook 'acrepl-guess-repl-hook
;;                 'acrepl-shadow-reuse-same-project-repl)

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(require 'filenotify)
(require 'subr-x)

(defvar acrepl-shadow-auto-reconnect nil
  "Attempt reconnection if shadow-cljs restarts.")

(defun acrepl-shadow-cljs-dir (code-path)
  "Determine shadow-cljs directory for CODE-PATH."
  (file-truename (locate-dominating-file code-path "shadow-cljs.edn")))

(defun acrepl-shadow-cljs-dir (code-path)
  "Determine shadow-cljs directory for CODE-PATH."
  (when-let ((guess-path (locate-dominating-file code-path "shadow-cljs.edn")))
    (file-truename guess-path)))

(defun acrepl-shadow-cljs-project-p ()
  "Determine whether some containing directory is a shadow-cljs project."
  (when-let ((closest-dir-with-sc-edn
              (locate-dominating-file default-directory "shadow-cljs.edn")))
    closest-dir-with-sc-edn))

(defun acrepl-shadow-cljs-type-p ()
  "Record if current source file is part of a shadow-cljs project.
One use would be via `add-hook' with `acrepl-project-type-hook'."
  (when-let ((path (acrepl-shadow-cljs-project-p)))
    (setq acrepl-project-types
	  (plist-put acrepl-project-types :shadow-cljs
		     (list
		      :path path
		      :connect #'acrepl-shadow-connect)))))

(defun acrepl-shadow-find-dot-dir ()
  "Find .shadow-cljs directory."
  (when-let ((shadow-project-dir (acrepl-shadow-cljs-project-p)))
    (let ((dot-shadow-cljs-dir (concat shadow-project-dir ".shadow-cljs")))
      (when (file-exists-p dot-shadow-cljs-dir)
        dot-shadow-cljs-dir))))

(defun acrepl-shadow-auto-reconnect-setup (dot-dir conn-name)
  "Arrange for auto-reconnect on shadow-cljs restart.
Monitor DOT-DIR for appropriate changes to trigger reconnection.
Only handle CONN-NAME's reconnection though."
  (let ((port-file (concat dot-dir
			   "/socket-repl.port")))
    (when (not (file-exists-p port-file))
      (error "Socket repl port file for shadow-cljs not found"))
    (file-notify-add-watch dot-dir (list 'change)
			   (lambda (event)
			     ;; actions: created, changed, deleted, stopped
			     (let ((action (nth 1 event))
				   (file (nth 2 event)))
			       (when (and (string-equal (file-truename (expand-file-name file))
							(file-truename (expand-file-name port-file)))
					  (equal action 'changed))
				 (let ((port (acrepl-number-from-file file)))
				   (when (not (> port 0))
				     (error "Unexpected socket repl file content"))
				   (when-let ((repl-buffer (get-buffer conn-name))) ; XXX: wrap?
				     (acrepl-connect conn-name)))))))))

(defun acrepl-net-filter (process str)
  "Called when a new message is recieved."
  (with-current-buffer (process-buffer process)
    (let ((res (condition-case nil
		   (edn-read (replace-regexp-in-string "\n.*=>" "" str ""))
		 (error (message "Error :( %s" str)))))
      (cond
       ((not (hash-table-p res))
	(comint-output-filter process str))
       ((eq :tap (gethash :tag res))
        (message "tapped!"))
       ((eq :ret (gethash :tag res))
        (let* ((val (gethash :result res))
	       (id (gethash :id res))
	       (listener (gethash id acrepl-listeners)))
          (if listener
	      (with-current-buffer
		  (first listener)
		(funcall (second listener) val))
	    (message "no listener found"))))
       ('t
	(comint-output-filter process str))))))

(defun acrepl-auto-complete-dotdot-form
    ()
  (interactive)
  (let* ((pt (point))
         (prev (start-of-sexp pt)))
    (when prev
      (let ((next (end-of-sexp prev)))
        (when next
	  (let* ((expr (edn-read (buffer-substring prev next)))
		 (sym (cond
		       ((= (length expr) 2)
                        (list "" (second expr)))
		       ((and (= (length expr) 3)
			     (eq (third expr) '-))
                        (list "" (second expr)))

		       ((and (= (length expr) 3)
			     (equal (substring (symbol-name (car (last expr))) 0 1) "-"))
                        (list (substring (symbol-name (car (last expr))) 1) (second expr)))
		       
		       ((eq (car (last expr)) '-)
                        (list "" (butlast expr)))
		       ((equal (substring (symbol-name (car (last expr))) 0 1) "-")
                        (list (substring (symbol-name (car (last expr))) 1) (butlast expr)))
		       ('t nil))))
	    (when sym
	      (acrepl-send-code-with-callback
	       (format "(map name (keys (bean %s)))" (second sym))
	       (lambda (res)
		 (message "type %s" (type-of res))
		 (let ((sources (edn-read res)))
		   (if (car sources)
		       (let ((chosen (helm :sources (helm-build-sync-source "test"
						      :candidates sources)
					   :input (first sym)
					   :buffer "*helm my command*")))
			 (when chosen
			   (let ((end (point)))
			     (re-search-backward "-")
			     (delete-region (point) end)
			     (insert "-")
			     (insert chosen))))
		     (message "No completions found."))))))))))))

(defun acrepl-shadow-connect (&optional callback)
  "Start acrepl for a file in a shadow-cljs project."
  (interactive)
  (when (not (buffer-file-name)) ; XXX: loose
    (user-error "Please invoke when visiting a Clojure file"))
  (let ((dot-dir (acrepl-shadow-find-dot-dir)))
    (when (not dot-dir)
      (error "Failed to find .shadow-cljs directory"))
    (let ((port-file (concat dot-dir
			     "/socket-repl.port")))
      (when (not (file-exists-p port-file))
        (error "Socket repl port file for shadow-cljs not found"))
      (let ((port (acrepl-number-from-file port-file)))
        (when (not (> port 0))
          (error "Unexpected socket repl file content"))
        (let* ((host "localhost") ; XXX: ever be remote?
               (file-buffer (current-buffer))
               (file-path (buffer-file-name))
               (repl-buffer (get-buffer-create
                             (acrepl-make-repl-buffer-name file-path port)))
               (repl-buffer-name (buffer-name repl-buffer))
               (conn-name repl-buffer-name))
          ;; need this before acrepl-connect can work
          (acrepl-set-endpoint! conn-name host port)
          (with-current-buffer repl-buffer
            (let ((res-buffer (acrepl-connect conn-name)))
              (if (not res-buffer)
                  (progn
                    (acrepl-remove-endpoint! conn-name) ; XXX: failed, remove?
                    (error "Failed to start acrepl"))
		(when acrepl-shadow-auto-reconnect
                  ;; XXX: if nil, indicate to user not successful?
                  (when (not (acrepl-shadow-auto-reconnect-setup dot-dir
								 conn-name))
                    (message "Warning: failed to setup auto-reconnect.")))
		(acrepl-update-conn-path! conn-name file-path)
		(acrepl-mode)
		(pop-to-buffer (current-buffer))
		(goto-char (point-max))
		(pop-to-buffer file-buffer)
		(set-process-filter (get-buffer-process res-buffer) 'acrepl-net-filter)

		(when callback
		  (funcall callback))))))))))

;; Saikyun's additions
(require 'acrepl-send)

(defvar acrepl-shadow-cljs-in-repl nil)
(defvar acrepl-shadow-cljs-server-buffer-name "*acrepl-shadow-cljs-server*")

(defun acrepl-shadow-start-server ()
  (interactive)
  (if (get-buffer acrepl-shadow-cljs-server-buffer-name)
      (message "`shadow-cljs server` is already running.")
    (let ((default-directory (acrepl-shadow-cljs-project-p)))
      (async-shell-command "shadow-cljs server" acrepl-shadow-cljs-server-buffer-name)
      (message "Started `shadow-cljs server` in dir %s" default-directory))))

(defun acrepl-shadow-cljs-start-repl ()
  (interactive)
  (progn (acrepl-send-code "(shadow/repl :app)")
	 (setq acrepl-shadow-cljs-in-repl t)))

(defun acrepl-shadow-cljs-exit-repl ()
  (interactive)
  (acrepl-send-code ":cljs/quit")
  (setq acrepl-shadow-cljs-in-repl nil))

(defun acrepl-shadow-cljs-stop-worker ()
  (interactive)
  (acrepl-send-code "(shadow.cljs.devtools.api/stop-worker :app)"))

(defun acrepl-shadow-stop-and-start-worker
    ()
  (interactive)
  (acrepl-shadow-cljs-exit-repl)
  (acrepl-shadow-cljs-stop-worker)
  (acrepl-shadow-cljs-watch-without-autobuild)
  (acrepl-shadow-cljs-start-repl))

(defun acrepl-shadow-cljs-watch-without-autobuild ()
  (interactive)
  (acrepl-send-code "(shadow.cljs.devtools.api/watch :app {:autobuild false})"))

(defun acrepl-shadow-cljs-watch-compile ()
  (interactive)
  (if acrepl-shadow-cljs-in-repl
      (progn (acrepl-shadow-cljs-exit-repl)
	     (acrepl-send-code "(shadow.cljs.devtools.api/watch-compile! :app)")
	     (acrepl-shadow-cljs-start-repl))
    (acrepl-send-code "(shadow.cljs.devtools.api/watch-compile! :app)")))

(defun acrepl-shadow-start-watch-and-yarn ()
  (acrepl-shadow-cljs-watch-without-autobuild)
  (acrepl-shadow-cljs-start-repl)
  (async-shell-command "yarn start --web" "*yarn start --web*"))

(defun acrepl-shadow-connect-yarn ()
  (interactive)
  (acrepl-shadow-connect 'acrepl-shadow-start-watch-and-yarn))

(defun acrepl-shadow-connect-cljs-repl ()
  (interactive)
  (acrepl-shadow-connect (lambda ()
			   (acrepl-shadow-cljs-start-repl)
			   (acrepl-set-ns))))

(defun acrepl-shadow-same-project-p (file-a-path file-b-path)
  "Guess if FILE-A-PATH and FILE-B-PATH are of the same shadow-cljs project."
  (let* ((shadow-a-parent
          (file-truename (locate-dominating-file file-a-path ".shadow-cljs")))
         (shadow-b-parent
          (file-truename (locate-dominating-file file-b-path ".shadow-cljs"))))
    (string-equal shadow-a-parent shadow-b-parent)))

(defun acrepl-shadow-conns-for-project ()
  "Try to find all conns for current shadow-cljs project."
  (let ((code-path (buffer-file-name)))
    (when-let ((sc-dir (acrepl-shadow-cljs-dir code-path)))
      ;; XXX: somehow put appropriate bits in acrepl-state.el
      (let ((path-to-conn (acrepl-get-path-to-conn))
            matching-conns)
        ;; collect conns for any files in same shadow-cljs project
        (maphash (lambda (path conn)
                   (let ((a-sc-dir (acrepl-shadow-cljs-dir path)))
                     (when (string-equal sc-dir a-sc-dir)
                       (push conn matching-conns))))
                 path-to-conn)
        matching-conns))))

(defun acrepl-shadow-reuse-same-project-repl ()
  "Try to find an existing connection in same project.
Searches existing connections for a matching one and if successful,
attempts to set it for the current code buffer.
Enable use via `add-hook' and `acrepl-guess-repl-hook'."
  (let ((code-path (buffer-file-name)))
    (when-let ((sc-dir (acrepl-shadow-cljs-dir code-path)))
      ;; XXX: somehow put appropriate bits in acrepl-state.el
      (let ((path-to-conn (acrepl-get-path-to-conn))
            candidates)
        ;; collect any files in same shadow-cljs project
        (maphash (lambda (path conn)
                   (let ((a-sc-dir (acrepl-shadow-cljs-dir path)))
                     (when (string-equal sc-dir a-sc-dir)
                       (push (cons path conn) candidates))))
                 path-to-conn)
        ;; XXX: customize choice policy?
        ;; for now just choose first match, if any
        (when-let ((match (car candidates)))
          (let ((path (car match))
                (conn (cdr match)))
            (acrepl-set-conn! code-path conn)
            (acrepl-add-conn-user! conn code-path)))))))

;; XXX: enable
;; (advice-add 'acrepl-current-conn :before
;;             #'acrepl-shadow-find-matching-conn);

(add-hook 'acrepl-guess-repl-hook
	  'acrepl-shadow-reuse-same-project-repl)



;; XXX: for testing
;;(add-hook 'acrepl-guess-repl-hook
;;          'acrepl-shadow-reuse-same-project-repl)

;; XXX: for testing
;;(add-hook 'acrepl-project-type-hook
;;          'acrepl-shadow-cljs-type-p)

(provide 'acrepl-shadow)

;;; acrepl-shadow.el ends here
