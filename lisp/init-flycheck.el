;; flycheck for syntax check
(use-package flycheck
  :after web-mode
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  (add-hook 'css-mode-hook 'global-flycheck-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(provide 'init-flycheck)
