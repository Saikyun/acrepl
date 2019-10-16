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

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(require 'filenotify)
(require 'subr-x)

(defvar acrepl-shadow-auto-reconnect nil
  "Attempt reconnection if shadow-cljs restarts.")

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

(defun acrepl-shadow-auto-reconnect-setup (dot-dir file-buffer)
  "Arrange for auto-reconnect on shadow-cljs restart.
Monitor DOT-DIR for appropriate changes to trigger reconnection.
Only handle FILE-BUFFER's reconnection though."
  (let ((port-file (concat dot-dir
                     "/socket-repl.port")))
    (when (not (file-exists-p port-file))
      (error "Socket repl port file for shadow-cljs not found"))
    (file-notify-add-watch dot-dir (list 'change)
      (lambda (event)
        ;; actions: created, changed, deleted, stopped
        (let ((action (nth 1 event))
              (file (nth 2 event)))
          (if (and (string-equal (expand-file-name file)
                     (expand-file-name port-file))
                (equal action 'changed))
            (let ((port (acrepl-number-from-file file)))
              (when (not (> port 0))
                (error "Unexpected socket repl file content"))
              (when (not (buffer-live-p file-buffer))
                (error "Missing buffer for: %s" file-buffer))
              (with-current-buffer file-buffer
                (when-let ((conn-name acrepl-current-conn-name))
                  (when-let ((conn (acrepl-lookup-conn conn-name)))
                    (let ((host (alist-get :host conn))
                          (port (alist-get :port conn)))
                      ;; XXX: may need more tweaking
                      (when (or (string-equal host "localhost")
                              (string-equal host "127.0.0.1")
                              (string-equal host "::1"))
                        (acrepl-connect conn)))))))))))))

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
               (conn-name repl-buffer-name)
               (conn-desc
                 (acrepl-make-conn-desc conn-name host port file-path
                   (format-time-string "%Y-%m-%d_%H:%M:%S")
                   repl-buffer)))
          (setq acrepl-current-conn-name conn-name)
          (with-current-buffer repl-buffer
            (let ((res-buffer (acrepl-connect conn-desc)))
              (when (not res-buffer)
                (error "Failed to start acrepl"))
              (when acrepl-shadow-auto-reconnect
                ;; XXX: if nil, indicate to user not successful?
                (when (not (acrepl-shadow-auto-reconnect-setup dot-dir
                             file-buffer))
                  (message "Warning: failed to setup auto-reconnect.")))
              (acrepl-remember-conn conn-name conn-desc)
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max))
              (pop-to-buffer file-buffer)
	      (when callback
		(funcall callback)))))))))

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

(defun acrepl-shadow-find-matching-conn ()
  "Advice to try to arrange for use of existing connection in same project.
Searches existing connections for a matching one and if successful,
attempts to set it for the current code buffer.
Use `advice-add' with `acrepl-current-conn' and :before to enable."
  (when (not acrepl-current-conn-name)
    (let ((conn-names (acrepl-conn-names))
          target-conn-name)
      (while (and conn-names
                  (not target-conn-name))
        (let ((conn-name (car conn-names)))
          (setq conn-names (cdr conn-names))
          (let* ((conn-desc (acrepl-lookup-conn conn-name))
                 (path (alist-get :path conn-desc))
                 (file-name (buffer-file-name)))
            (when (acrepl-shadow-same-project-p path file-name)
              (setq target-conn-name conn-name)))))
      (when target-conn-name
        (setq acrepl-current-conn-name target-conn-name)))))

;; XXX: enable
(advice-add 'acrepl-current-conn :before
            #'acrepl-shadow-find-matching-conn)

(provide 'acrepl-shadow)

;;; acrepl-shadow.el ends here
