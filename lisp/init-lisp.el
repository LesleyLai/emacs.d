;; keeps parentheses under control.
(use-package paredit
             :ensure t
             :config
             (add-hook 'eval-expression-minibuffer-setup-hook
                       #'enable-paredit-mode))

;; eldoc
(use-package eldoc
             :after paredit
             :init
             (eldoc-add-command
              'paredit-backward-delete
              'paredit-close-round)
             )

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
             :ensure t)

;; -------------------------------------------
;; Enable desired features for all lisp modes
;; -------------------------------------------
(defun lisp-modes-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (eldoc-mode)
  (electric-pair-mode))

(let* ((lispy-hooks '(lisp-mode-hook
                      inferior-lisp-mode-hook
                      lisp-interaction-mode-hook
                      ielm-mode-hook
                      emacs-lisp-mode-hook
                      scheme-mode-hook
                      racket-mode-hook
                      )))
  (dolist (hook lispy-hooks)
    (add-hook hook 'lisp-modes-setup)))

(provide 'init-lisp)
