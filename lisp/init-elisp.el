;;; init-elisp.el --- Emacs lisp specify setup
;;; Commentary:
;; 

;;; Code:

(defun recompile-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (byte-compile-dest-file buffer-file-name))
                  (byte-compile-file buffer-file-name)))))

(defun eval-region-or-buffer () (interactive)
       (let ((debug-on-error t))
         (cond
          (mark-active
           (call-interactively 'eval-region)
           (message "Region evaluated!")
           (setq deactivate-mark t))
          (t
           (eval-buffer)
           (message "Evaluate buffer %s" (buffer-name))
           ))))

(use-package emacs-lisp-mode
  :hook ((emacs-lisp-mode . recompile-elc-on-save)
         (emacs-lisp-mode . checkdoc-minor-mode))
  :custom
  (load-prefer-newer t)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-c" . eval-region-or-buffer)
         ("<f5>" . eval-region-or-buffer)
         :map lisp-interaction-mode-map
         ("C-c C-c" . eval-region-or-buffer)
         ("<f5>" . eval-region-or-buffer))
  :interpreter (("emacs" . emacs-lisp-mode))
  )


(provide 'init-elisp)
;;; init-elisp.el ends here
