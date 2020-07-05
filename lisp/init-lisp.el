;; keeps parentheses under control.
(use-package paredit
             :ensure t
             :config
             (add-hook 'eval-expression-minibuffer-setup-hook
                       #'enable-paredit-mode)
             )


(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-M-j") 'paredit-backward)
     (define-key paredit-mode-map (kbd "C-M-l") 'paredit-forward)
     (define-key paredit-mode-map (kbd "M-d") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "C-M-b") nil)
     (define-key paredit-mode-map (kbd "C-M-f") nil)))

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
  (eldoc-mode))

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
;;; init-lisp.el ends here
