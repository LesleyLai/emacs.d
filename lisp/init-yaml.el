;; YAML mode support
(use-package yaml-mode
  :ensure
  :mode "\\.yml\\'"
  :hook (yaml-mode . global-flycheck-mode))

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(provide 'init-yaml)
