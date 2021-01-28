;; keeps parentheses under control.
(use-package paredit
  :ensure t
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              (";" . nil)
              ("<M-up>" . nil)
              ("<M-down>" . nil)
              ("<M-left>" . nil)
              ("<M-right>" . nil)))


(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-M-j") 'paredit-backward)
     (define-key paredit-mode-map (kbd "C-M-l") 'paredit-forward)
     (define-key paredit-mode-map (kbd "M-d") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "M-f") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "M-e") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "M-r") 'paredit-forward-kill-word)
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
  :ensure t
  :defer 5
  :diminish)

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
                      racket-repl-mode-hook
                      clojure-mode-hook
                      )))
  (dolist (hook lispy-hooks)
    (add-hook hook 'lisp-modes-setup)))

(provide 'init-lisp)
;;; init-lisp.el ends here
