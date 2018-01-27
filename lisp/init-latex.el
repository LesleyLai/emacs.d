(use-package auctex
  :ensure t
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup))
  (use-package company-auctex
    :ensure t
    :requires company
    :config
    (company-auctex-init))
  )

(use-package latex-preview-pane
  :ensure t)

(use-package company-math
  :ensure t
  :requires company
  :config
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (set (make-local-variable 'company-backends)
                                    '(company-math-symbols-latex
                                      company-latex-commands
                                      company-math-symbols-unicode
                                      company-files
                                      company-capf
                                      company-semantic
                                      company-dabbrev
                                      ))
                               )))



(provide 'init-latex)
