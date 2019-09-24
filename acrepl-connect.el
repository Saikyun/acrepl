;;; acrepl-connect.el --- connection -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'rx)

(defcustom acrepl-default-endpoint "localhost:23579"
  "Default host and port to connect to.
Host and port should be delimited with ':'."
  :type 'string
  :group 'acrepl)

(defvar acrepl-connections
  (make-hash-table :test #'equal)
  "Hash table of acrepl connections.")

(defvar-local acrepl-connection-name nil
  "Current connection name.")

(defvar-local acrepl-reconnect-try-number 0
  "Current reconnect try number.")

(defun acrepl-make-conn-desc (name host port path ts repl-buffer)
  "Create conn descriptor from NAME, HOST, PORT, PATH, TS, and REPL-BUFFER."
  (list
   (cons :name name)
   (cons :host host)
   (cons :port port)
   (cons :path path)
   (cons :ts ts)
   (cons :repl-buffer repl-buffer)))

(defun acrepl-get-connection (name)
  "Get connection named NAME."
  (gethash name acrepl-connections))

(defun acrepl-connection-names ()
  "Return list of connection names."
  (let ((names '()))
    (maphash (lambda (k _)
               (push k names))
             acrepl-connections)
    names))

(defun acrepl-set-connection (name)
  "Set current connection to the one named NAME."
  (interactive
   (let ((input (completing-read "Connection: "
                                 (acrepl-connection-names)
                                 nil
                                 "confirm")))
     (if (equal input "")
       (user-error "No connection specified")
       (list input))))
  (let ((conn (gethash name acrepl-connections)))
    (when conn
      (setq acrepl-connection-name name)
      conn)))

(defun acrepl-current-connection ()
  "Return current connection, if any."
  (acrepl-get-connection acrepl-connection-name))

(defun acrepl-remember-connection (name connection)
  "Remember CONNECTION named NAME."
  (puthash name connection acrepl-connections))

(defvar acrepl-conn-counter 0
  "Number of connections made so far.")

(defun acrepl-make-repl-buffer-name (path port)
  "Create a unique-ish repl buffer name using PATH, PORT and other info."
  ;; sample: src/augistints/core.cljc -> (1) augistints (2) core.cljc
  (let ((re-parent-and-file (rx (0+ anything)
                                "/"
                                (group-n 1 (1+ (not (any "/")))) ; parent
                                "/"
                                (group-n 2 (1+ (not (any "/")))) ; file
                                eol)))
    (if (not (string-match re-parent-and-file path))
        (error "Failed to parse: %s" path)
      ;; XXX: checking?
      (let ((dir-name (match-string 1 path))
            (file-name (match-string 2 path)))
        (setq acrepl-conn-counter (1+ acrepl-conn-counter))
        (format "[%s]*%s/%s*[%s]"
                acrepl-conn-counter
                (substring dir-name 0 (min 3 (length dir-name)))
                file-name
                port)))))

(defun acrepl-repl-buffer-name? (buffer-name)
  "Check if `acrepl-make-repl-buffer-name' could have made BUFFER-NAME."
  ;; sample: [1]*aug/core.cljc*[23579]
  (let ((re-rbn (rx bol
                    "[" (1+ digit) "]" ; conn counter
                    "*" (1+ (not (any "/"))) "/" (1+ (not (any "/"))) "*"
                    "[" (1+ digit) "]" ; port
                    eol)))
  (string-match re-rbn buffer-name)))

(defun acrepl-arrange-retry (repl-buffer conn-name sentinel retry)
  "May be arrange for a reconnection attempt for REPL-BUFFER.
Try to reconnect with the connection associated with CONN-NAME,
using SENTINEL, influenced by the description of RETRY.
`acrepl-reconnect-try-number' is used to track number of connection attempts."
  (let ((wait-secs (plist-get retry :wait))
        (max-tries (plist-get retry :max-tries)))
    (run-at-time wait-secs nil
      (lambda ()
        (when (not repl-buffer)
          (error "Repl-buffer is no longer: %S" repl-buffer))
        (with-current-buffer repl-buffer
          (if (>= acrepl-reconnect-try-number max-tries)
            (progn
              (setq acrepl-reconnect-try-number 0)
              (message "Giving up after %S attempts." max-tries))
            ;; try again...
            (setq acrepl-reconnect-try-number
              (1+ acrepl-reconnect-try-number))
            ;; XXX: why acrepl-reconnect and not acrepl-connect?
            (acrepl-reconnect conn-name sentinel retry)))))))

(defun acrepl-connect (conn-desc &optional sentinel retry)
  "Try to connect using CONN-DESC.
Optional argument SENTINEL is a process sentinel.
Optional argument RETRY is a plist describing the retrying."
  (let* ((name (alist-get :name conn-desc))
         (host (alist-get :host conn-desc))
         (port (alist-get :port conn-desc))
         (repl-buffer (alist-get :repl-buffer conn-desc))
         (repl-buffer-name (buffer-name repl-buffer))
         (repl-process-name repl-buffer-name))
    (message "Connecting to socket REPL on '%s:%d'..." host port)
    (when (not (buffer-live-p repl-buffer))
      (error "Buffer not alive? %S" name))
    (condition-case nil
      (let ((buffer (make-comint-in-buffer repl-process-name repl-buffer-name
                      (cons host port))))
        (when (not buffer)
          (error "Failed to connect to %s:%d" host port))
        (when sentinel
          (let ((process (get-process repl-process-name)))
            (when (not process)
              (error "Failed to acquire repl process"))
            (set-process-sentinel process sentinel)))
        buffer)
      (file-error ; handling connection refused
        (when retry
          (acrepl-arrange-retry repl-buffer name sentinel retry))
        nil))))

(defun acrepl-reconnect (name &optional sentinel retry)
  "Try to connect to connection named NAME.
Tries to guess a reasonable default.
If `acrepl-connection-name' is set, assumes current buffer is a file
with Clojure code, and uses the value of the variable as the default.
Otherwise, if the current buffer name looks like an acrepl repl buffer
name, uses that as a default.
If neither of those things works out, just uses an empty string as the
default.
Optional argument SENTINEL should be a process sentinel.
Optional argument RETRY is a plist describing the retrying."
  (interactive
   (let* ((default (or acrepl-connection-name
                       (let ((buffer-name (buffer-name (current-buffer))))
                         (when (acrepl-repl-buffer-name? buffer-name)
                           buffer-name))
                       ""))
          (input (completing-read "Connection: "
                                 (acrepl-connection-names)
                                 nil
                                 "confirm"
                                 default)))
     (if (equal input "")
       (user-error "No connection specified")
       (list input))))
  (let ((conn-desc (gethash name acrepl-connections)))
    (when conn-desc ; XXX: errors?
      (acrepl-connect conn-desc sentinel retry))))

(provide 'acrepl-connect)

;;; acrepl-connect.el ends here
