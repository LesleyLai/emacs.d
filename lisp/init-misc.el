;; Hook for general programming
(use-package fill-column-indicator :ensure t)
(use-package idle-highlight-mode :ensure t)

(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (idle-highlight-mode t)
  (fci-mode t) ; 80 column
  )

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

;;  Standard ML mode
(use-package sml-mode :ensure t)

;; Recent files
(recentf-mode 1)

;; Many ibuffer default
(defalias 'list-buffers 'ibuffer) ; make ibuffer default

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
;; {{
(setq savehist-file "~/.emacs.d/history")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
;; }}

;; Save cursor position
;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; Sudo edit
(use-package sudo-edit :ensure t)

;; Less prompts
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(defalias 'term 'ansi-term)

;; File delete to trash can
(setq delete-by-moving-to-trash t)

;; Show the bookmark page instead of scratch page at start up
(let ((scratch "*scratch*")
      (bookmark "*Bookmark List*"))
  (if (buffer-live-p (get-buffer scratch))
      (progn
         (kill-buffer scratch)
         (require 'bookmark)
         (bookmark-bmenu-list)
         (switch-to-buffer bookmark))))

(provide 'init-misc)
