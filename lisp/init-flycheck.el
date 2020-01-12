;; flycheck for syntax check
(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  (add-hook 'css-mode-hook 'global-flycheck-mode))

(provide 'init-flycheck)
