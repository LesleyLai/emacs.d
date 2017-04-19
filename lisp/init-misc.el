;; Hook for general programming
(use-package idle-highlight-mode :ensure t)

(use-package fill-column-indicator
  :ensure t
  :config
  ;; See https://github.com/alpaker/Fill-Column-Indicator/issues/67
  (defvar eos/fci-disabled nil)
  (make-variable-buffer-local 'eos/fci-disabled)
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


(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (idle-highlight-mode t)
  (flyspell-prog-mode) ; Spell checking in comment
  (unless (string= major-mode "web-mode")
    (progn (fci-mode t)  ; 80 column
           (add-hook 'window-configuration-change-hook
                     #'eos/maybe-disable-fci)
           )))

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

;;  Standard ML mode
(use-package sml-mode :ensure t)

;; Recent files
(recentf-mode 1)

;; Back up
;; {{
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Save a lot
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; }}

;; History
;;  From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(use-package savehist
  :init
  (setq savehist-file "~/.emacs.d/history")
  (savehist-mode 1)
  :config
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(search
          kill-ring
          search-ring
          regexp-search-ring)))

;; Save cursor position
;; remember cursor position
(if (version< emacs-version "25.0")
     (progn
       (require 'saveplace)
       (setq-default save-place t))
   (save-place-mode 1))

 ;; Less prompts
 (defalias 'yes-or-no-p 'y-or-n-p)
 (setq confirm-nonexistent-file-or-buffer nil)
 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
             kill-buffer-query-functions))

 ;; File delete to trash can
(setq delete-by-moving-to-trash t)

(provide 'init-misc)
