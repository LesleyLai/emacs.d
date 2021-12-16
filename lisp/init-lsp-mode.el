(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil)
  :diminish
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (cmake-mode . lsp)
         (css-mode . lsp)
         (elm-mode . lsp)
         (html-mode . lsp)
         (js2-mode . lsp)
         (json-mode . lsp)
         (LaTeX-mode . lsp)
         (python-mode . lsp)
         (powershell-mode . lsp)
         (racket-mode . lsp)
         (rust-mode . lsp)
         (sh-mode . lsp)
         (yaml-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        ;; lsp-ui-doc-position 'at-point
        )
  )

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  )

(use-package dap-mode
  :ensure t
  :commands dap-mode
  :after posframe
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (require 'dap-lldb))

(provide 'init-lsp-mode)
