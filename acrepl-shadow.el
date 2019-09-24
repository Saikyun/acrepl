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

(defun acrepl-shadow-connect ()
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
              (pop-to-buffer file-buffer))))))))
  
(provide 'acrepl-shadow)

;;; acrepl-shadow.el ends here