(use-package lsp-mode
  :ensure t
  :after typescript-mode web-mode js2-mode rust-mode python-mode
  :hook (c-mode . lsp)
  :hook (c++-mode . lsp)
  :hook (css-mode . lsp)
  :hook (python-mode . lsp)
  :hook (typescript-mode . lsp)
  :hook (web-mode . lsp)
  :hook (js2-mode . lsp)
  :hook (rust-mode . lsp)
  :commands lsp
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'lsp-after-open-hook #'lsp-ui-mode)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        ;; lsp-ui-doc-position 'at-point
        )
  )

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-recompletion t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates t
        company-lsp-async t)
  )


(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  )

(provide 'init-lsp-mode)
