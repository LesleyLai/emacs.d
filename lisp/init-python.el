;; Elpy
;; run command `pip install jedi flake8 importmagic autopep8` in shell,
;; check https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t
  :commands elpy-enable
  :after flycheck
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  ;;  flycheck for elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  
  :bind (:map elpy-mode-map
              ("f5" . elpy-shell-send-region-or-buffer))
  )

;; enable autopep8 formatting on save
;; need autopep8 installed
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

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

(provide 'init-python)
