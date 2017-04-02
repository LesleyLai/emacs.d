;; Elpy
;; {{
;; run command `pip install jedi flake8 importmagic autopep8` in shell,
;; check https://github.com/jorgenschaefer/elpy
(require-package 'elpy)
(elpy-enable)

;;  flycheck for elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;  Use python 3 by default
(if (executable-find "python3")
    (progn (setq elpy-rpc-python-command "python3")
            (elpy-use-cpython "python3"))
  )

;;  Use ipython if avilable
(if (executable-find "ipython3")
    (elpy-use-ipython "ipython3")
  (if (executable-find "ipython")
      (elpy-use-ipython "ipython")))
;; }}

;; enable autopep8 formatting on save
;; need autopep8 installed
(require-package 'py-autopep8)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defun python-mode-hook-setup ()
  (subword-mode +1)
  (eldoc-mode 1)
  )

(add-hook 'python-mode-hook 'python-mode-hook-setup)

;; Major mode to edit pip requirements files
(require-package 'pip-requirements)

(provide 'init-python)
