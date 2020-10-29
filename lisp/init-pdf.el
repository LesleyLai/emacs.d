(use-package pdf-tools
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(provide 'init-pdf)
