;; racket
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . lisp-mode))

;; paradit and eldoc
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

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
                      lisp-interaction-mode-hook)))
  (dolist (hook lispy-hooks)
    (add-hook hook 'lisp-modes-setup)))

(provide 'init-lisp)
