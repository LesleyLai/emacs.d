(use-package elm-mode
  :ensure t
  :mode ("\\.elm\\'" . elm-mode)
  :after company
  :diminish elm-format-on-save-mode
  :diminish elm-indent-mode
  :config
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t))
  (add-hook 'elm-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends)
                                  '(company-elm
                                    company-yasnippet
                                    company-files)))))

(use-package flycheck-elm
  :ensure t
  :after elm-mode flycheck
  :config
   (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  )

(use-package elm-yasnippets
  :ensure t)

(provide 'init-elm)
