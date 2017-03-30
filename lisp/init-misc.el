(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
                  
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Hook for programming
(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (idle-highlight-mode t))

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

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


;; Show the bookmark page instead of scratch page at start up
(kill-buffer "*scratch*")
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

(provide 'init-misc)
