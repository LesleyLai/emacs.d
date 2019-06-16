;; ==============================================
;; Initializes the UI of emacs that fit my habits
;; ==============================================

;; Window/frame titles
(customize-set-variable 'frame-title-format (list "%b " "%[ - GNU %F " emacs-version))
(customize-set-variable 'icon-title-format (list "%b " " - GNU %F " emacs-version))

(show-paren-mode 1)

(customize-set-variable 'indent-tabs-mode nil)

(setq select-enable-clipboard t
      select-enable-primary t
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

;; Line number
(use-package display-line-numbers
  :if (version<= "26.0.50" emacs-version)
  :init
  (global-display-line-numbers-mode)
  )

;; Git gutter
(use-package git-gutter
  :ensure t
  :defer 2
  :init
  (if *use-GUI*
      (use-package git-gutter-fringe :ensure t :defer 2))
  :config
  (global-git-gutter-mode)
  (customize-set-variable 'git-gutter:handled-backends '(svn hg git)))


;; Smart Mode line
;; {{
(use-package smart-mode-line
  :ensure t
  :init
  (use-package smart-mode-line-powerline-theme
    :ensure t
    :defer t)
  (setq sml/theme 'light-powerline)
  (setq sml/no-confirm-load-theme t)
  
  :config  
  (add-to-list 'sml/replacer-regexp-list '(".+/C\\+\\+/" ":C++:"))
  (add-to-list 'sml/replacer-regexp-list '(".+/blog/" ":BLOG:"))
  (add-to-list 'sml/replacer-regexp-list '(".+/wiki/" ":WIKI:"))
  
  ;; Block minor mode from show on mode line
  (setq rm-blacklist (quote (" hl-p" " company"
                             " Undo-Tree" " yas"
                             " GitGutter" " ARev"      
                             " Paredit" " Undo-Tree"
                             "∑flykeys" " WK")))

  (sml/setup)
  )
;; }}

;; Emacs which-key showes displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :defer t
  :config
  (which-key-mode))

;; Cursor Type
(setq-default cursor-type 'bar)

;; No useless GUI features
(customize-set-variable 'use-file-dialog nil)
(customize-set-variable 'use-dialog-box nil)
(push '(tool-bar-lines . 0) default-frame-alist)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; No startup screen
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-echo-area-message t)

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (customize-set-variable 'line-spacing 0)))

(use-package paradox
  :ensure t
  :defer 1
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print)
)



(provide 'init-ui)
