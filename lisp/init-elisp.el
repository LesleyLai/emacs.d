;; Emacs lisp specify setup

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)


(defun elisp-mode-hook-setup ()
  "Enable features useful in Emacs Lisp mode."
    (checkdoc-minor-mode))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup)

(provide 'init-elisp)
