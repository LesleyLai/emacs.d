(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
(push 'company-lsp company-backends))

(provide 'init-lsp-mode)
