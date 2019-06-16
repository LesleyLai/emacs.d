(use-package company-web
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\(\\.erb\\)?\\'" . web-mode)) ; ruby
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . web-mode)) ; ruby

;; Web mode
(use-package web-mode
  :ensure t
  :after company company-web-html
  )

;; Reason
(use-package reason-mode
  :ensure t
  :config
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (customize-set-variable 'web-mode-markup-indent-offset 2)
  (customize-set-variable 'web-mode-css-indent-offset 2)
  (customize-set-variable 'web-mode-code-indent-offset 2)
  (customize-set-variable 'web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (customize-set-variable 'web-mode-enable-auto-pairing t)
  (customize-set-variable 'web-mode-enable-css-colorization t)
  (customize-set-variable 'web-mode-imenu-regexp-list
                          '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
                            ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
                            ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
                            ;; angular imenu
                            (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
  )

(add-hook 'web-mode-hook (lambda ()
                           (add-to-list 'company-dabbrev-code-modes 'web-mode)
                           (set (make-local-variable 'company-backends)
                                '(company-web-html
                                  company-css
                                  company-yasnippet
                                  company-files
                                  company-lsp))
                           (company-mode t)))

(use-package json-mode
  :ensure
  :mode "\\.json\\'")

;; Js2-mode
(use-package js2-mode
  :ensure
  :mode ("\\.js$" . js2-mode)
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j"
                                         'join-line-or-lines-in-region
                                         js2-mode-map)))))

;; Typescript
(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  :config
  (setq-default typescript-indent-level 2)
  )

;; Rainbow mode
(use-package rainbow-mode
  :after js2-mode typescript-mode
  :ensure
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)
  (add-hook 'typescript-mode-hook 'rainbow-mode))

(provide 'init-web)
