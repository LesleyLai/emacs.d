;; ============================================
;; ui for git
;; ============================================
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (if *win64*
      (progn (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
             (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
             (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
             (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
             (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
             (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))))

(use-package gitattributes-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)

(provide 'init-git)
