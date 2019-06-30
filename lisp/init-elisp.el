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

(defun elisp-mode-hook-setup ()
  "Enable features only useful in Emacs Lisp mode."
  (checkdoc-minor-mode))

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
  :init
  (progn
    (customize-set-variable 'load-prefer-newer t)
    (add-hook 'emacs-lisp-mode-hook 'recompile-elc-on-save)
    (add-to-list 'completion-styles 'initials t)
    (add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup))
  :bind (("C-c C-c" . eval-region-or-buffer)
         ("<f5>" . eval-region-or-buffer))
  :interpreter (("emacs" . emacs-lisp-mode))
  )


(provide 'init-elisp)
;;; init-elisp.el ends here
