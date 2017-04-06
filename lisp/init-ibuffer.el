(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer) ; make ibuffer default

  (setq ibuffer-saved-filter-groups
        `(("default"
           ("Dired" (mode . dired-mode))
           ("Web"
            (or (name . "\\.js")
                (name . "\\.css")
                (name . "\\.html")
                (name . "\\.php")
                (name . "\\.xml")
                (mode . yaml-mode)))
           ("Text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (mode . markdown-mode)
                (mode . text-mode)))
           ("Data"
            (or (mode . gnuplot-mode)
                (mode . octave-mode)
                (mode . R-mode)))
           ("Shell"
            (or (mode . inferior-ess-mode)
                (mode . inferior-python-mode)
                (mode . eshell-mode)
                (mode . gnuplot-comint-mode)
                (mode . comint-mode)))
           ("Helper"
            (or (mode . help-mode)
                (mode . ess-help-mode)))
           ("C++"
            (or (mode . c++-mode)))
           ("Python"
            (or (mode . python-mode))))))
  )

(provide 'init-ibuffer)
 
