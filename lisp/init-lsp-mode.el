(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib")))))
  :hook (((astro-mode
           c-mode
           c++-mode
           cmake-mode
           css-mode
           scss-mode
           elm-mode
           html-mode
           js2-mode
           json-mode
           typescript-mode
           web-mode
           LaTeX-mode
           powershell-mode
           racket-mode
           rust-mode
           sh-mode
           yaml-mode)
          . eglot-ensure)))

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
