(use-package sr-speedbar
  :ensure t
  :defer t
  :init
  ;; Show tree on the left side
  (setq sr-speedbar-right-side nil)
  ;; Show all files
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-max-width 70)
  (setq sr-speedbar-width 20)
)

(provide 'init-speedbar)

