;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (bind-key "C-c i" 'insert-latex LaTeX-mode-map)                      ;;
;; (bind-key "C-c C-c" 'TeX-comment-or-uncomment-region LaTeX-mode-map) ;;
;; (bind-key "C-c C-k" 'TeX-command-master LaTeX-mode-map)              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auctex
  :ensure t
  :config
  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup))
  (use-package company-auctex
    :ensure t
    :requires company
    :config
    (company-auctex-init)))


(provide 'init-latex)
