;; Misc load later

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; Sudo edit
(use-package sudo-edit :ensure t)

;;  Startup benchmark
(use-package esup :ensure t)

;; better terminal
(defalias 'term 'ansi-term)

(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  )

(provide 'init-misc-lazy)
