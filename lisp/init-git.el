;; ============================================
;; ui for git
;; ============================================
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  )

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(provide 'init-git)
