;; flycheck for syntax check
(use-package flycheck
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'init-flycheck)
