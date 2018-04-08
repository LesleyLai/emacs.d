(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (customize-set-variable 'flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after company flycheck typescript-mode
  :config
  (progn
    ;; aligns annotation to the right hand side
    (customize-set-variable 'company-tooltip-align-annotations t)

    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    ))


(provide 'init-typescript)
