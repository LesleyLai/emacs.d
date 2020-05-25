;; Racket mode
(use-package racket-mode
  :ensure t
  :defer t
  :hook
  (racket-repl-mode
   . (lambda ()
       (define-key racket-repl-mode-map (kbd "C-w") 'kill-current-buffer))))

(require 'lsp-mode)
(defcustom lsp-racket-executable-path "racket"
  "Path to Racket executable."
  :group 'lsp-racket
  :type 'string)

(defcustom lsp-racket-server-args '()
  "Extra arguments for the Racket language server."
  :group 'lsp-racket
  :type '(repeat string))

(defun lsp-racket--server-command ()
  "Generate the language server startup command."
  `(,lsp-racket-executable-path "--lib" "racket-langserver" ,@lsp-racket-server-args))

(defvar lsp-racket--config-options `())

(setq lsp-language-id-configuration
     (cons (cons ".*\\.rkt$" "racket") lsp-language-id-configuration))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection 'lsp-racket--server-command)
                  :major-modes '(racket-mode)
                  :server-id 'racket
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       `(:racket ,lsp-racket--config-options))))))


(provide 'init-racket)
