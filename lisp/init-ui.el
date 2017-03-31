;; Initializes the UI of emacs that fit my habits

;; I-beam instead block cursor
(setq-default cursor-type 'bar)

;; Clears unsless menu stuffs
;; {{
(define-key menu-bar-edit-menu [search search-forward] nil)
(define-key menu-bar-edit-menu [search search-backward] nil)
(define-key menu-bar-edit-menu [search re-search-forward] nil)
(define-key menu-bar-edit-menu [search re-search-backward] nil)
(define-key menu-bar-edit-menu [search separator-repeat-search] nil)
(define-key menu-bar-edit-menu [search repeat-search-fwd] nil)
(define-key menu-bar-edit-menu [search repeat-search-back] nil)
(define-key menu-bar-edit-menu [search separator-tag-search] nil)


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

;; Smart Mode line
;; {{
(require-package 'smart-mode-line)
(require-package 'smart-mode-line-powerline-theme)

(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Short cuts
(add-to-list 'sml/replacer-regexp-list '("^~/dev/" ":DEV:"))
(add-to-list 'sml/replacer-regexp-list '("^~/dev/web/" ":WEB:"))
(add-to-list 'sml/replacer-regexp-list '("^~/dev/Graphics/" ":CG:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Downloads/" ":D/L:"))
(add-to-list 'sml/replacer-regexp-list '(".+/C\\+\\+/" ":C++:"))
(add-to-list 'sml/replacer-regexp-list '(".+/blog/" ":BLOG:"))
(add-to-list 'sml/replacer-regexp-list '(".+/wiki/" ":WIKI:"))

;; Block minor mode from show on mode line
(setq rm-blacklist (quote (" hl-p" " company" " Undo-Tree" " yas")))
;; }}

;; No startup screen
(setq inhibit-startup-screen t)

(provide 'init-ui)
