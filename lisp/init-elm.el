(use-package elm-mode
  :ensure t
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t))
  )

(use-package flycheck-elm
  :ensure t
  :after elm-mode
  :config
  (eval-after-load 'elm-mode (flycheck-elm-setup))
  )

(provide 'init-elm)
