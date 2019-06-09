;; Misc load later

;; Sudo edit
(use-package sudo-edit :ensure t)

;;  Startup benchmark
(use-package esup :ensure t)

;; better terminal
(defalias 'term 'ansi-term)

(use-package switch-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  )

;; Set exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :init
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))

;; For org-mode html exportation
(use-package htmlize
  :ensure t)

;; Update quelpa
(quelpa-self-upgrade)

;;  menu bar stuffs
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

(provide 'init-misc-lazy)
