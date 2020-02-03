;; clang-format
(use-package clang-format
  :ensure t
  :defer 3
  :config
  (setq clang-format-style-option "llvm"))

;; CMake
(use-package cmake-mode
  :ensure)

(use-package highlight-doxygen
  :ensure t
  :after cc-mode
  :defer t
  :hook ((cc-mode . highlight-doxygen-mode)
         (c-mode . highlight-doxygen-mode)
         (c++-mode . highlight-doxygen-mode)))

(use-package cmake-font-lock
  :ensure
  :after (cmake-mode)
  :config
  (progn
    (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))
  )

;; Modern c++ syntax highlighter
(use-package modern-cpp-font-lock :ensure t)

(use-package cc-mode
  :after modern-cpp-font-lock
  :defer 2
  :config
  (modern-c++-font-lock-global-mode t)
)

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (customize-set-variable 'c-basic-offset 2)
  ;; give me NO newline automatically after electric expressions are entered
  (customize-set-variable 'c-auto-newline nil)

  (local-set-key [C-tab] 'clang-format-region)
  (local-set-key [C-M-tab] 'clang-format-buffer)
  )

(defun my-c-mode-setup ()
  "C/C++ only setup"
  ;; make a #define be left-aligned
  (customize-set-variable 'c-electric-pound-behavior (quote (alignleft))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (my-common-cc-mode-setup)
  (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
    (my-c-mode-setup))
  )

(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)


(provide 'init-cc-mode)
