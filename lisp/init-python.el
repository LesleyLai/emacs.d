(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    ))

(use-package company-anaconda
  :ensure t
  :init (add-to-list 'company-backends 'company-anaconda))

;; enable autopep8 formatting on save
;; need autopep8 installed
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'anaconda-mode-hook 'py-autopep8-enable-on-save)
  )

(defun python-mode-hook-setup ()
  (subword-mode +1)
  (eldoc-mode 1)
  )

(add-hook 'python-mode-hook 'python-mode-hook-setup)

;; Major mode to edit pip requirements files
;; enable autopep8 formatting on save
;; need autopep8 installed
(use-package pip-requirements
  :ensure t)

(setq python-shell-interpreter "ipython3")

(provide 'init-python)
