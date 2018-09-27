;; ============================================
;; ui for git
;; ============================================
(use-package magit
  :after modalka
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (define-key modalka-mode-map (kbd "SPC g") #'magit-status)
  )

(provide 'init-git)
