(use-package company
             :ensure t
             :config
             (add-hook 'after-init-hook 'global-company-mode)

             ;; align annotations to the right tooltip border
             (setq company-tooltip-align-annotations 't)
             ;; decrease delay before autocompletion popup shows
             (setq company-idle-delay .3))

(use-package company-c-headers :ensure t)

(use-package company-tern :ensure t)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-tern)))

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
