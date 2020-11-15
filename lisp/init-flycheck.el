;; flycheck for syntax check
(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'init-flycheck)
