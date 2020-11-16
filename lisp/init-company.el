;;; init-company.el --- Company mode setup

;;; Commentary:
;; 

;;; Code:
(use-package company
  :ensure t
  :defer 5
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; align annotations to the right tooltip border
  (customize-set-variable 'company-tooltip-align-annotations 't)

  ;; decrease delay before autocompletion popup shows
  (customize-set-variable 'company-idle-delay .1)

  (define-key company-active-map (kbd "M-i") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "M-k") 'company-select-next-or-abort)
  )

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode)
  (customize-set-variable 'company-quickhelp-delay .1)
  )

(use-package company-c-headers :ensure t)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-cmake)))

;; Resolve conflicts with fci mode by temporarily prohibit it {{
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
;; }}

(provide 'init-company)

;;; init-company.el ends here
