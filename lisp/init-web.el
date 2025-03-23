(use-package company-web
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))

;; Web mode
(use-package web-mode
  :ensure t
  :after company company-web-html
  :config
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (customize-set-variable 'web-mode-markup-indent-offset 2)
  (customize-set-variable 'web-mode-css-indent-offset 2)
  (customize-set-variable 'web-mode-code-indent-offset 2)
  (customize-set-variable 'web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (customize-set-variable 'web-mode-enable-auto-pairing t)
  (customize-set-variable 'web-mode-enable-css-colorization t)
  (customize-set-variable 'web-mode-enable-auto-indentation nil) ; disable auto indentation
  (customize-set-variable 'web-mode-imenu-regexp-list
                          '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
                            ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
                            ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
                            ;; angular imenu
                            (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
  )

(add-hook 'web-mode-hook (lambda ()
                           (add-to-list 'company-dabbrev-code-modes 'web-mode)
                           (company-mode t)))

(use-package astro-ts-mode
  :after treesit-auto
  :ensure
  :mode "\\.astro\\'"  
  )

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
; TODO: make the following automatic
; (mapc #'treesit-install-language-grammar '(astro css tsx))


(use-package json-mode
  :ensure
  :mode "\\.json\\'"
  :config
  (customize-set-variable 'json-reformat:indent-width 2)
  (customize-set-variable 'js-indent-level 2)
  )


;; Js2-mode
(use-package js2-mode
  :ensure
  :mode (("\\.js$" . js2-mode)
         ("\\.mjs$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (:map js2-mode-map
              ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))))

;; Typescript
(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  :config
  (setq-default typescript-indent-level 2)
  )

;; Rainbow mode
(use-package rainbow-mode
  :after js2-mode typescript-mode web-mode
  :ensure
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'typescript-mode-hook 'rainbow-mode))

;; Prettier
(use-package prettier-js
  :after (astro-mode markdown-mode web-mode js2-mode web-mode typescript-mode)
  :ensure
  :commands prettier-js-mode
  :init
  (when (executable-find "gdiff")
    (setq prettier-js-diff-command "gdiff"))
  :hook ((astro-ts-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (markdown-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (json-mode . prettier-js-mode))
  )

;; Auto revert package.json
(add-to-list 'revert-without-query "package.jsonS")

(provide 'init-web)
