;;; acrepl-interaction.el --- interaction -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-ascertain)
(require 'acrepl-display)
(require 'acrepl-ns)
(require 'acrepl-load)
(require 'acrepl-send)
(require 'acrepl-switch)
(require 'acrepl-tap)
(require 'acrepl-repl-util)

(defvar acrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-e" 'acrepl-send-expr-at-point)
    (define-key map "\C-c\C-l" 'acrepl-load-buffer-file)
    (define-key map "\C-c\C-r" 'acrepl-send-region)
    (define-key map "\C-c\C-x" 'acrepl-set-current-conn)
    (define-key map "\C-c\C-y" 'acrepl)
    (define-key map "\C-c\C-z" 'acrepl-switch-to-repl)
    ;; XXX: may be not so important?
    (define-key map "\C-c\C-a" 'acrepl-send-ascertained-region)
    (define-key map "\C-c\C-b" 'acrepl-send-buffer)
    (define-key map "\C-c\C-i" 'acrepl-load-file)
    (define-key map "\C-c\C-t" 'acrepl-tap-expr-at-point)

    ;; Saikyun's additions
    (define-key map "\C-c\C-c" 'acrepl-send-defun)
    (define-key map "\C-c\C-w" 'acrepl-send-wrapping-sexp)
    (define-key map "\C-c\C-d" 'acrepl-doc-expr-at-point)
    (define-key map "\C-c\C-n" 'acrepl-set-ns)
    (define-key map "\C-c\C-g\C-y" 'acrepl-shadow-connect)
    (define-key map "\C-c\C-g\C-r" 'acrepl-shadow-connect-cljs-repl)
    (define-key map "\C-c\C-g\C-c" 'acrepl-shadow-cljs-watch-compile)
    (define-key map "\C-c\C-g\C-c" 'acrepl-shadow-cljs-watch-compile)
    (define-key map "\C-c\C-g\C-s" 'acrepl-start-shadow-cljs-server-and-connect)
    (define-key map "\C-c\C-g\C-w\C-w" 'acrepl-shadow-cljs-watch-without-autobuild)
    ;; End of Saikyun's additions

    (easy-menu-define acrepl-interaction-mode-menu map
      "A Clojure REPL Interaction Mode Menu"
      '("ACRepl"
        ["Send expression at point" acrepl-send-expr-at-point t]
        ["Send region" acrepl-send-region t]
        "--"
        ["Load buffer file" acrepl-load-buffer-file t]
        "--"
        ["New Connection" acrepl t]
        ["Set Connection" acrepl-set-current-conn t]
        ["Reconnect" acrepl-reconnect t]
        "--"
        ["Switch to REPL" acrepl-switch-to-repl t]
        "--"
        ("Extras"
         ["Send ascertained region" acrepl-send-ascertained-region t]
         ["Send buffer" acrepl-send-buffer t]
         "--"
         ["tap> expression at point" acrepl-tap-expr-at-point t]
         ["tap> region" acrepl-tap-region t]
         "--"
         ["Load file" acrepl-load-file t]
         "--"
         ["Last output in window" acrepl-last-output-in-window t]
         ["Last output in other window" acrepl-last-output-in-other-window t]
         ["Last output in frame" acrepl-last-output-in-frame t])))
    map)
  "Keymap for acrepl interaction mode.")

;;;###autoload
(define-minor-mode acrepl-interaction-mode
  "Minor mode for acrepl interaction from a Clojure buffer.
The following keys are available in `acrepl-interaction-mode`:
\\{acrepl-interaction-mode}"
  nil " acrepl" acrepl-interaction-mode-map)

(provide 'acrepl-interaction)

;;; acrepl-interaction.el ends here
