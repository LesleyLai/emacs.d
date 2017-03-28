;; Initializes the UI of emacs that fit my habits

;; Clears unsless menu stuffs
;; {{
(global-unset-key (kbd "C-h t"))
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(define-key menu-bar-help-menu [emacs-tutorial-language-specific] nil)

(global-unset-key (kbd "C-h C-f"))
(define-key menu-bar-help-menu [emacs-faq] nil)

(global-unset-key (kbd "C-h C-p"))
(define-key menu-bar-help-menu [view-emacs-problems] nil)

(global-unset-key (kbd "C-h g"))
(define-key menu-bar-help-menu [about-gnu-project] nil)

(global-unset-key (kbd "C-h C-c"))
(define-key menu-bar-help-menu [describe-copying] nil)

(global-unset-key (kbd "C-h C-w"))
(define-key menu-bar-help-menu [describe-no-warranty] nil)

(global-unset-key (kbd "C-h C-o"))
(define-key menu-bar-help-menu [getting-new-versions] nil)

(define-key menu-bar-help-menu [emacs-known-problems] nil)

(define-key menu-bar-tools-menu [games] nil)
;; }}

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; No startup screen
(setq inhibit-startup-screen t)

(provide 'init-ui)
