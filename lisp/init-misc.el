;; Remove trailing whitespaces
(setq show-trailing-whitespace t)
(use-package ws-butler
  :ensure
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode)
  (add-hook 'proof-mode-hook #'ws-butler-mode)
  (add-hook 'bibtex-mode-hook #'ws-butler-mode)
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t))

(if (not *win64*)
    (use-package flyspell-mode
      :defer t
      :config
      (progn
        (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
        (customize-set-variable 'ispell-program-name "aspell")
        (customize-set-variable 'ispell-list-command "--list") ;; run flyspell with aspell, not ispell
        )
      ))

(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (electric-pair-mode t)
  (flyspell-prog-mode)
  (display-line-numbers-mode)
  (setq tab-always-indent 'complete)
  (unless (string= major-mode "web-mode")
    (display-fill-column-indicator-mode)))

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

;;  Standard ML mode
(use-package sml-mode :ensure t :defer t)

;; Recent files
(use-package recentf
  :init
  (run-at-time "10 min" (* 10 60) 'recentf-save-list)
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 50)
  (recentf-exclude `("/ssh:"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'"
                     "~/.emacs.d/session.*")))

;; Back up
;; {{
(customize-set-variable 'backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Save a lot
(customize-set-variable 'delete-old-versions -1)
(customize-set-variable 'version-control t)
(customize-set-variable 'vc-make-backup-files t)
(customize-set-variable 'auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; }}

;; Do not create lock file
(customize-set-variable 'create-lockfiles nil)

 ;; Less prompts
 (defalias 'yes-or-no-p 'y-or-n-p)
 (customize-set-variable 'confirm-nonexistent-file-or-buffer nil)
 (customize-set-variable 'kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
             kill-buffer-query-functions))

 ;; File delete to trash can
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'recentf-auto-cleanup 'never)

(use-package exec-path-from-shell
  :ensure
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Better terminal
(if (not *win64*)
    (use-package vterm
      :ensure t
      :defer 30
      :bind (:map vterm-mode-map
                  ("C-w" . nil)
                  ("C-v" . vterm-yank)
                  ("M-i" . vterm-send-up)
                  ("M-k" . vterm-send-down)
                  ("M-j" . vterm-send-left)
                  ("M-l" . vterm-send-right))
      :custom
      (vterm-max-scrollback 100000)))

;; diminish a bunch of standard minor-modes
(use-package simple
  :diminish visual-line-mode auto-fill-mode
  :hook (after-init . size-indication-mode) ;; Show size of a file in modeline
  )

(use-package eldoc :diminish)
(use-package checkdoc :diminish checkdoc-minor-mode)
(use-package with-editor :diminish with-editor-mode)

;; Auto Revert Mode
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

;; Winner mode can be used to undo/redo changes on window
(use-package winner-mode
  :hook (after-init . winner-mode))

;; Minor mode to save the curse place for a file after last visit
(use-package saveplace
  :hook (after-init . save-place-mode))

;; Workaround for inefficiency that may caused by long lines
;; For example, when opening an minified JS file
(use-package so-long
  :config (global-so-long-mode 1))

(use-package abbrev
  :diminish abbrev-mode)

;; Let WSL emacs open windows browser
(setq-default sysTypeSpecific  system-type) ;; get the system-type value

(cond
 ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
 ((eq sysTypeSpecific 'gnu/linux)
  (when (string-match "Linux.*Microsoft.*Linux"
                      (shell-command-to-string "uname -a"))

    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
     cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic))))

;; Chinese input method
(use-package rime
  :ensure
  :custom
  (default-input-method "rime"))

(provide 'init-misc)
