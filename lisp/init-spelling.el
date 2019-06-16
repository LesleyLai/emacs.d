;;; init-spelling.el --- Spell checking

(use-package flyspell-mode
  :defer t
  :config
  (progn
    (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
    (customize-set-variable 'ispell-program-name "aspell")
    (customize-set-variable 'ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    )
  )

(provide 'init-spelling)
