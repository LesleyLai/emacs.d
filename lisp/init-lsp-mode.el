(use-package lsp-mode
  :ensure t
    :commands (lsp lsp-deferred)
  :init
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         )
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
  :defer t
  :after company lsp-mode yasnippet
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-recompletion 'auto
        company-lsp-enable-snippet t
        company-lsp-filter-candidates nil
        company-lsp-cache-candidates t
        company-lsp-async t)
  )


(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  )

(provide 'init-lsp-mode)
