;; Major mode
(use-package tuareg
  :ensure
  :config
  (setq tuareg-interactive-program "utop"
        tuareg-opam "opam"))


;; Reason
(use-package reason-mode
  :ensure t
  )

;; Interective Repl
(use-package utop
  :after (tuareg reason-mode)
  :ensure
  :hook (tuareg-mode . utop-minor-mode)
  :hook (reason-mode . utop-minor-mode))

(use-package ocamlformat
  :commands ocamlformat
  :custom
  (ocamlformat-command "ocamlformat")
  :hook (tuareg-mode . (lambda ()
                         (if tuareg-mode
                             (add-hook 'before-save-hook #'ocamlformat-before-save)
                           (remove-hook 'before-save-hook #'ocamlformat-before-save)
                           ))))


(use-package merlin
  :ensure
  :custom
  (merlin-completion-with-doc t)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("M-?" . merlin-occurrences)
              ("C-c C-j" . merlin-jump)
              ("C-c i" . merlin-locate-ident)
              ("C-c C-e" . merlin-iedit-occurrences)
              )
  :after company
  :config
  (add-to-list 'company-backends 'merlin-company-backend)
  (setq merlin-command "ocamlmerlin")
  :hook
  ;; Start merlin on ml files
  ((reason-mode tuareg-mode caml-mode) . merlin-mode)
  )

;; merlin-eldoc is too slow for my current windows setup
(if (not *win64*)
      (use-package merlin-eldoc
        :ensure t
        :hook
        ((reason-mode tuareg-mode caml-mode) . merlin-eldoc-setup))
      )

(use-package dune
  :ensure
  :when (executable-find "dune")
  :mode "\dune\\'"
  )

(use-package ocp-indent
  :ensure
  :when (executable-find "ocp-indent")
  :hook (tuareg-mode . ocp-setup-indent))


(provide 'init-ocaml)
