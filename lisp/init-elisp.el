;; ------------------------------------------------------------------
;; Emacs lisp specify setup
;; ------------------------------------------------------------------

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

(defun recompile-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (byte-compile-dest-file buffer-file-name))
                  (byte-compile-file buffer-file-name)))))
(add-hook 'emacs-lisp-mode-hook 'recompile-elc-on-save
          )


(defun elisp-mode-hook-setup ()
  "Enable features only useful in Emacs Lisp mode."
    (checkdoc-minor-mode))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup)

(provide 'init-elisp)
