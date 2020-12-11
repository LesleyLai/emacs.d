;; flycheck for syntax check
(use-package flycheck
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-grammarly
  :ensure t
  :defer 60
  :custom
  ((flycheck-grammarly-check-time 5)))

(provide 'init-flycheck)
