;; GUI and terminal have some different UI setting

;; Start up dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs.")
  )

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run every time after we make a new frame."
  (select-frame frame)
  
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defun fix-up-terminal-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "A" [up])
    (define-key map "B" [down])
    (define-key map "C" [left])
    (define-key map "D" [right])))

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (fix-up-terminal-arrows)
            ; Mouse in a terminal (Use shift to paste with middle button)
            (xterm-mouse-mode 1)
            ;; Enable wheelmouse support by default
            (cond (window-system
                   (mwheel-install)))
            ;; Menu bar
            (menu-bar-mode -1)
            ;; Show time in console
            (use-package time
              :config
              (setq display-time-24hr-format t
                    display-time-default-load-average nil)
              (display-time-mode))
            ))

(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (menu-bar-mode 1)
))

(provide 'init-frame-hooks)
