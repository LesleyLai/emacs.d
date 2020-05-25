(use-package idle-highlight-mode :ensure t)

(use-package fill-column-indicator
  :ensure t
  :config
  ;; See https://github.com/alpaker/Fill-Column-Indicator/issues/67
  (defvar-local eos/fci-disabled nil)
  ;; Add a hook that disables fci if enabled when the window changes
  ;; and it isn't wide enough to display it.
  (defun eos/maybe-disable-fci ()
    (interactive)
    ;; Disable FCI if necessary
    (when (and fci-mode
               (< (window-width) (or fci-rule-column fill-column)))
      (fci-mode -1)
      (setq-local eos/fci-disabled t))
    ;; Enable FCI if necessary
    (when (and eos/fci-disabled
               (eq fci-mode nil)
               (> (window-width) (or fci-rule-column fill-column)))
      (fci-mode 1)
      (setq-local eos/fci-disabled nil))))

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


(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (idle-highlight-mode t)
  (electric-pair-mode t)
  (setq tab-always-indent 'complete)
  (unless (string= major-mode "web-mode")
    (progn (fci-mode t)  ; 80 column
           (add-hook 'window-configuration-change-hook
                     #'eos/maybe-disable-fci)
           )))

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

;;  Standard ML mode
(use-package sml-mode :ensure t :defer t)

;; Recent files
(use-package recentf
  ;; Loads after 1 second of idle time.
  :init
  :config
  (customize-set-variable 'recentf-max-saved-items 50)
  (recentf-mode +1)
  )

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

;; History
;;  From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(use-package savehist
  :init
  (customize-set-variable 'savehist-file "~/.emacs.d/history")
  (savehist-mode 1)
  :config
  (customize-set-variable 'history-length t)
  (customize-set-variable 'history-delete-duplicates t)
  (customize-set-variable 'savehist-save-minibuffer-history 1)
  (customize-set-variable 'savehist-additional-variables
        '(search
          kill-ring
          search-ring
          regexp-search-ring)))

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
    (exec-path-from-shell-initialize))
  )

(provide 'init-misc)
