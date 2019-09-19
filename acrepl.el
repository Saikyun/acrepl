;;; acrepl.el --- A Clojure REPL -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20190907
;; Package-Requires: ((clojure-mode "5.11.0") (emacs "26.2"))
;; Keywords: clojure, repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Clojure REPL - simple socket repl interaction and editor functionality

;;;; Installation

;;;;; Manual

;; Ensure this file and the following dependencies (and their
;; dependencies) are in your load-path:
;;
;;   ab.el -- should be included in the same repository
;;   clojure-mode
;;
;;  and put this in your relevant init file:
;;
;;    (require 'acrepl)
;;
;;  Optionally, add:
;;
;;    (add-hook 'clojure-mode-hook
;;              #'acrepl-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  Clojure code in it.

;;;;; Automatic

;; TODO :)

;;;;; Usage

;; 0. Start up a clojure project (JVM, shadow-cljs, Arcadia, CLR) with a
;;    socket repl and note the host and port
;;
;;    N.B. for shadow-cljs', the info may be autodetected

;; 1. Connect to the socket repl by:
;;
;;      M-x acrepl
;;
;;    and at the prompt, specify a host and port like:
;;
;;      localhost:23579
;;
;;    i.e. a host or ip address followed by a colon and port number
;;
;;    A buffer for interaction with the socket repl should appear.

;; 2. For editor features, in a relevant buffer with a clojure source file:
;;
;;      M-x acrepl-interaction-mode
;;
;;    There should be a ACRepl menu containing some convenience commands:
;;
;;      Send ascertained region
;;      Send buffer
;;      Send expression at point
;;      Send region
;;      tap> expression at point
;;
;;      Load buffer file
;;      Load file
;;
;;      Switch to REPL

;;;;; Acknowledgments

;; Thanks to those involved in:
;;
;;   cider
;;   clojure-mode
;;   inf-clojure
;;   miracle
;;   monroe
;;   replique
;;   sesman
;;
;; and transitively involved folks too ;)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'ab)
(require 'clojure-mode)
(require 'comint)
(require 'subr-x)

;;;; The Rest

(defgroup acrepl nil
  "A Clojure REPL"
  :prefix "acrepl-"
  :group 'applications)

;; clojure repls (NOT unrepl nor prepl):
;;
;;   my.namespace=>
;;
;; shadow-cljs:
;;
;;   [1:1] my.namespace=>
;;
;; note that each has a trailing space
(defcustom acrepl-prompt-regexp "^[^> \n]*=> *"
  "Regexp to recognize prompts in acrepl-mode."
  :type 'regexp
  :group 'acrepl)

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

(defun acrepl-make-conn-desc (host port path ts repl-buffer)
  "Create connection descriptor from HOST, PORT, PATH, TS, and REPL-BUFFER."
  (list
   (cons 'host host)
   (cons 'port port)
   (cons 'path path)
   (cons 'ts ts)
   (cons 'repl-buffer repl-buffer)))

(defun acrepl-get-connection (name)
  "Get connection named NAME."
  (gethash name acrepl-connections))

(defun acrepl-connection-names ()
  "Return list of connection names."
  (let ((names '()))
    (maphash (lambda (k v)
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

(defun acrepl-guess-repl-buffer ()
  "Return a relevant repl buffer."
  (let ((conn (acrepl-current-connection)))
    (when conn
      (alist-get 'repl-buffer conn)))) ; XXX: checking?

(defun acrepl-remember-connection (name connection)
  "Remember CONNECTION named NAME."
  (puthash name connection acrepl-connections))

(defun acrepl-switch-to-repl ()
  "Switch to a repl buffer."
  (interactive)
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (pop-to-buffer repl-buffer))))

(defun acrepl-send-code (code-str)
  "Send CODE-STR.
CODE-STR should be a Clojure form."
  (interactive "sCode: ")
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (let ((here (point))
            (original-buffer (current-buffer)))
        ;; switch to acrepl buffer to prepare for appending
        (set-buffer repl-buffer)
        (goto-char (point-max))
        (insert code-str)
        (comint-send-input)
        (set-buffer original-buffer)
        (if (eq original-buffer repl-buffer)
            (goto-char (point-max))
          (goto-char here))))))

(defun acrepl-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (buffer-substring-no-properties start end)))

(defun acrepl-tap-region (start end)
  "Apply tap> to a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (concat "(tap> "
                            (buffer-substring-no-properties start end)
                            ")")))

(defvar acrepl-ascertain-forms
  (list 'def
        'defn
        'defn-
        'defmacro
        'ns
        'require)
  "List of symbols used by `acrepl-send-ascertained-region'.")

(defun acrepl-send-ascertained-region ()
  "Send a region ascertained around point.
Determination is based on `acrepl-ascertain-forms'."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds acrepl-ascertain-forms)
        (acrepl-send-region start end))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

(defun acrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (acrepl-send-region (point-min) (point-max)))

;; XXX: experimental
(defun acrepl-detect-clojure-sexp-bounds ()
  "Return the bounds of the Clojure sexp at point."
  (let ((here (point)))
    (backward-sexp)
    (let ((start (if (looking-back "\\(#[^#)]*\\)")
                     (match-beginning 1)
                   (point))))
      (forward-sexp)
      (let ((end (point)))
        (goto-char here)
        (list start end)))))

(defun acrepl-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-sexp-bounds)
    (when (and start end)
      (acrepl-send-region start end))))

(defun acrepl-tap-expression-at-point ()
  "Apply tap> to expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-sexp-bounds)
    (when (and start end)
      (acrepl-tap-region start end))))

(defun acrepl-load-file (filename)
  "Send the `load-file` form with full path of FILENAME."
  (interactive "fFile name: ")
  (acrepl-send-code (format "(load-file \"%s\")"
                            (expand-file-name filename))))

(defun acrepl-load-buffer-file ()
  "Send the `load-file` form with buffer's full path."
  (interactive)
  (acrepl-load-file (buffer-file-name)))

(defvar acrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'acrepl-send-ascertained-region)
    (define-key map "\C-c\C-b" 'acrepl-send-buffer)
    (define-key map "\C-c\C-e" 'acrepl-send-expression-at-point)
    (define-key map "\C-c\C-i" 'acrepl-load-file)
    (define-key map "\C-c\C-l" 'acrepl-load-buffer-file)
    (define-key map "\C-c\C-r" 'acrepl-send-region)
    (define-key map "\C-c\C-t" 'acrepl-tap-expression-at-point)
    (define-key map "\C-c\C-x" 'acrepl-set-connection)
    (define-key map "\C-c\C-y" 'acrepl)
    (define-key map "\C-c\C-z" 'acrepl-switch-to-repl)
    (easy-menu-define acrepl-interaction-mode-map map
      "A Clojure REPL Interaction Mode Menu"
      '("ACRepl"
        ["Send ascertained region" acrepl-send-ascertained-region t]
        ["Send buffer" acrepl-send-buffer t]
        ["Send expression at point" acrepl-send-expression-at-point t]
        ["Send region" acrepl-send-region t]
        ["tap> expression at point" acrepl-tap-expression-at-point t]
        "--"
        ["Load buffer file" acrepl-load-buffer-file t]
        ["Load file" acrepl-load-file t]
        "--"
        ["New Connection" acrepl t]
        ["Set Connection" acrepl-set-connection t]
        "--"
        ["Switch to REPL" acrepl-switch-to-repl t]))
    map)
  "ACRepl interaction mode map.")

(defvar acrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (define-key map "\C-c\C-i" 'acrepl-load-file)
        (easy-menu-define acrepl-mode-map map
          "A Clojure REPL Mode Menu"
          '("ACRepl"
            ["Load file" acrepl-load-file t]))
    map)
  "ACRepl mode map.")

(define-derived-mode acrepl-mode comint-mode "A Clojure REPL"
  "Major mode for acrepl.

\\{acrepl-mode-map}"
  
  :syntax-table clojure-mode-syntax-table
  (setq comint-prompt-regexp acrepl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  ;; XXX: can use setq-local instead?
  (set (make-local-variable 'font-lock-defaults)
       '(clojure-font-lock-keywords t)))

;;;###autoload
(define-minor-mode acrepl-interaction-mode
  "Minor mode for acrepl interaction from a Clojure buffer.
The following keys are available in `acrepl-interaction-mode`:
\\{acrepl-interaction-mode}"

  nil " acrepl" acrepl-interaction-mode-map)

;;; XXX: git-specific and works only for shadow-cljs
(defun acrepl-guess-endpoint ()
  "Guess an endpoint."
  (let ((closest-dot-git-parent
         (locate-dominating-file default-directory ".git")))
    (when closest-dot-git-parent
      (let ((socket-repl-port-file (concat closest-dot-git-parent
                                           ".shadow-cljs/socket-repl.port")))
        (when (file-exists-p socket-repl-port-file)
          (let ((port (string-to-number
                       (with-temp-buffer
                         (insert-file-contents socket-repl-port-file)
                         (buffer-string)))))
            (when (> port 0)
              (format "localhost:%s" port))))))))

(defun acrepl-make-connection-name (host port)
  "Create a unique-ish connection name using HOST, PORT and other information."
  (format "%s:%s:%s:%s"
          host
          port
          (cdr (project-current))
          (format-time-string "%Y-%m-%d_%H:%M:%S")))

(defvar acrepl-conn-counter 0
  "Number of connections made so far.")

(defun acrepl-make-buffer-name (path port)
  "Create a unique-ish repl buffer name using PATH, PORT and other info."
  (string-match ".*/\\([^/]+\\)/\\([^/]+\\)$" path)
  ;; XXX: checking?
  (let ((dir-name (match-string 1 path))
        (file-name (match-string 2 path)))
    (setq acrepl-conn-counter (1+ acrepl-conn-counter))
    (format "[%s]*%s/%s*[%s]"
            acrepl-conn-counter
            (substring dir-name 0 (min 3 (length dir-name)))
            file-name
            port)))

;;;###autoload
(defun acrepl (endpoint)
  "Start acrepl.
Query user for ENDPOINT which specifies the Clojure socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (if (not (buffer-file-name)) ; XXX: loose
       (user-error "Please invoke when visiting a Clojure file")
     (let ((endpoint (or (acrepl-guess-endpoint)
                         acrepl-default-endpoint)))
       (list
        (read-string (format "REPL endpoint (default '%s'): " endpoint)
                     endpoint nil endpoint)))))
  (unless
      ;;(ignore-errors ;; XXX: uncomment at some point...
      (let* ((ep (split-string endpoint ":"))
             (host (car ep))
             (port (string-to-number (cadr ep)))
             (file-buffer (current-buffer))
             (file-path (buffer-file-name))
             (repl-buffer (get-buffer-create
                           (acrepl-make-buffer-name file-path port)))
             (repl-buffer-name (buffer-name repl-buffer))
             (conn-name repl-buffer-name)
             (conn-desc
              (acrepl-make-conn-desc host port file-path
                                     (format-time-string "%Y-%m-%d_%H:%M:%S")
                                     repl-buffer)))
        (message "Connecting to socket REPL on '%s:%d'..." host port)
        (setq acrepl-connection-name conn-name)
        (with-current-buffer repl-buffer
          (prog1
              (make-comint-in-buffer "acrepl" repl-buffer-name
                                     (cons host port))
            (acrepl-remember-connection conn-name conn-desc)
            (acrepl-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            (pop-to-buffer file-buffer))))
    (message "Failed to connect to %s" endpoint)))

(provide 'acrepl)

;;; acrepl.el ends here
