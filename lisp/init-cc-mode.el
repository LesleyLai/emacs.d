;; clang-format
(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "llvm"))


(use-package cmake-ide
  :ensure t)

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  (local-set-key [C-tab] 'clang-format-region)
  (local-set-key [C-M-tab] 'clang-format-buffer)
  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0))

(defun my-c-mode-setup ()
  "C/C++ only setup"

  (setq cc-search-directories '("."
                                "/usr/include"
                                "/usr/local/include/*"
                                "../*/include"
                                "$WXWIN/include"))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (my-common-cc-mode-setup)
  (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
    (my-c-mode-setup))

  ;; GNU Global source code tagging system
  (use-package ggtags
    :ensure t
    :config
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found"
                                    (shell-command-to-string "global -p"))))
      (setq gtags-suggested-key-mapping t)
      (ggtags-mode 1)))
  )

(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)


(provide 'init-cc-mode)
