(use-package company-web :ensure t)

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
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\(\\.erb\\)?\\'" . web-mode)) ; ruby
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . web-mode)) ; ruby

(use-package web-mode
  :ensure t
  :after company company-web-html company-tern tide
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (add-to-list 'company-dabbrev-code-modes 'web-mode)
                             (set (make-local-variable 'company-backends)
                                  '(company-tern
                                    company-web-html
                                    company-css
                                    company-yasnippet
                                    company-files))
                             (when (or
                                    (string-equal "tsx" (file-name-extension buffer-file-name))
                                    (string-equal "jsx" (file-name-extension buffer-file-name)))
                               (setup-tide-mode))
                             (company-mode t)))

  (flycheck-add-mode 'typescript-tslint 'web-mode)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx")
                  )
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))


  (eval-after-load 'web-mode
    '(progn
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
       ))
  )

(provide 'init-web)
