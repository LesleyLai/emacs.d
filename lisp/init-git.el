;; ============================================
;; ui for git
;; ============================================
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  )

(provide 'init-git)
