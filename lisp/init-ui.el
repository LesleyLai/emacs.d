;; ==============================================
;; Initializes the UI of emacs that fit my habits
;; ==============================================

(setq-default cursor-type 'bar) ; I-beam instad of block cursor

;; Window/frame titles
(setq frame-title-format (list "%b " "%[ - GNU %F " emacs-version)
      icon-title-format (list "%b " " - GNU %F " emacs-version))

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

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
(use-package smart-mode-line-powerline-theme :ensure t)
(use-package smart-mode-line
             :ensure t
             :config
             (add-to-list 'sml/replacer-regexp-list '("^~/dev/" ":DEV:"))
             (add-to-list 'sml/replacer-regexp-list '("^~/dev/web/" ":WEB:"))
             (add-to-list 'sml/replacer-regexp-list '("^~/dev/Graphics/" ":CG:"))
             (add-to-list 'sml/replacer-regexp-list '("^~/Downloads/" ":D/L:"))
             (add-to-list 'sml/replacer-regexp-list '(".+/C\\+\\+/" ":C++:"))
             (add-to-list 'sml/replacer-regexp-list '(".+/blog/" ":BLOG:"))
             (add-to-list 'sml/replacer-regexp-list '(".+/wiki/" ":WIKI:"))
             
             ;; Block minor mode from show on mode line
             (setq rm-blacklist (quote (" hl-p" " company"
                                        " Undo-Tree" " yas")))
             :init
             (setq sml/no-confirm-load-theme t)
             (sml/setup)
             )
;; }}

;; No useless GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))


;; No startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


(provide 'init-ui)
